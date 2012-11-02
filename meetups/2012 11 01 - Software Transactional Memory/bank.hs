{-# LANGUAGE TupleSections #-}

module Main where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Data.Functor
import           Data.IORef
import           Data.List
import           Data.Map ( Map )
import qualified Data.Map as M
import           Data.Maybe
import           Network
import           Network.Socket (close)
import           System.IO
import           Text.Printf

type Bank = TVar State

data State = State { credit :: Map Account (TVar CHF) }

type Account = Int
type CHF     = Int

port :: PortNumber
port = 9876

main :: IO ()
main = withSocketsDo $ do
         putStrLn $ printf "Listening on port: %s..." (show port)
         s <- listenOn $ PortNumber port
         bank <- atomically $ newTVar $ State {credit = M.empty}

         forever $ do
           c@(h, _, _) <- accept s
           void $ forkFinally (handleRequest (handleSession bank) c) $ \e -> do
             case e of
               Left ex -> putStrLn $ "Exception: " ++ show ex
               Right _ -> return ()
             hClose h

handleRequest :: ([String] -> IO String)
              -> (Handle, HostName, PortNumber)
              -> IO ()
handleRequest handleSession (h, hostname, port) = do
  putStrLn $ printf "Accepting connection from: %s on port: %s"
                    hostname (show port)
  hSetBuffering h LineBuffering
  loop
    where
      loop = do
        req <- hGetLine h
        let command = words req
        if command == ["CLOSE"]
          then return ()
          else do
            (handleSession command >>= hPutStrLn h) `catch` \e ->
              hPutStrLn h $ "Exception: " ++ show (e :: SomeException)
            loop

showAccounts :: Map Account (TVar CHF) -> STM String
showAccounts m = intercalate "\n" <$> mapM showAccount (M.toList m)

showAccount :: (Account, TVar CHF) -> STM String
showAccount (account, chfTVar) = (show . (account,)) <$> readTVar chfTVar

look k m = maybe (err "not found") return $ M.lookup k m

err :: String -> STM a
err = throwSTM . ErrorCall

handleSession :: Bank -> [String] -> IO String
handleSession bank ["LIST"] = atomically $ do
  st <- readTVar bank
  creditStr <- showAccounts $ credit st
  return creditStr

handleSession bank ["CREATE", accountStr, chfStr] = atomically $ do
  let account = read accountStr
      chf     = read chfStr
  st <- readTVar bank
  when (account `M.member` credit st) $
    error $ "Credit account already exists!"
  chfTVar <- newTVar chf
  let st' = st {credit = M.insert account chfTVar $ credit st}
  writeTVar bank st'
  return "Credit account created."

handleSession bank ["CREDIT", accountStr, deltaStr] = atomically $ do
  let account = read accountStr
      delta   = read deltaStr
  st <- readTVar bank
  chfTVar <- look account $ credit st
  chf <- readTVar chfTVar
  let chf' = chf + delta
  when (chf' < 0) $
    err $ printf "Can't have negative amount of: %i" chf'
  writeTVar chfTVar chf'
  return "Account credited."

handleSession bank ["CREDIT_BLOCK", accountStr, deltaStr] = atomically $ do
  let account = read accountStr
      delta   = read deltaStr
  st <- readTVar bank
  creditAccount account delta st

handleSession bank ["CREDIT_ONEOF", account1Str, account2Str, deltaStr] = atomically $ do
  let account1 = read account1Str
      account2 = read account2Str
      delta    = read deltaStr
  st <- readTVar bank

  creditAccount account1 delta st
    `orElse`
       creditAccount account2 delta st

handleSession bank ["TRANSFER", fromAccountStr, toAccountStr, deltaStr] = atomically $ do
  let fromAccount = read fromAccountStr
      toAccount   = read toAccountStr
      delta       = read deltaStr
  st <- readTVar bank

  fromChfTVar <- look fromAccount $ credit st
  toChfTVar   <- look toAccount   $ credit st

  fromChf <- readTVar fromChfTVar
  toChf   <- readTVar toChfTVar

  let fromChf' = fromChf - delta
      toChf'   = toChf   + delta

  when (fromChf' < 0) $
    err $ printf "Can't have negative amount of %i in from account" fromChf'

  when (toChf' < 0) $
    err $ printf "Can't have negative amount of %i in to account" toChf'

  return $! fib 40

  writeTVar fromChfTVar fromChf'
  writeTVar toChfTVar   toChf'

  return "Transfer complete."

handleSession _ _ = return "Unknown command!"

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

creditAccount :: Account -> CHF -> State -> STM String
creditAccount account delta st = do
  chfTVar <- look account $ credit st
  chf <- readTVar chfTVar
  let chf' = chf + delta
  when (chf' < 0) retry
  writeTVar chfTVar chf'
  return "Account credited."
