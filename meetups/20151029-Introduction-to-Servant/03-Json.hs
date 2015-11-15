{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

-- Servant makes it easy to deliver JSON responses.

import Servant
import Network.Wai (Application)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Handler.Warp (run)
import GHC.Generics
import Data.Aeson

type API = "hello"   :> Get '[JSON] String
      :<|> "numbers" :> Get '[JSON] [Int]
      :<|> "pairs"   :> Get '[JSON] [Pair]

helloServer = return "Hello World!"

numberServer = return [1..17]

data Pair = Pair
    { x :: Int
    , y :: Int
    } deriving (Eq, Show, Generic)

instance ToJSON Pair

pairServer = return [Pair 10 20, Pair 1 2]

server :: Server API
server = helloServer
    :<|> numberServer
    :<|> pairServer

proxy :: Proxy API
proxy = Proxy

app :: Application
app = serve proxy server

main :: IO ()
main = do
  putStrLn "Server endpoints:"
  putStrLn "  http://localhost:8000/hello/"
  putStrLn "  http://localhost:8000/numbers/"
  putStrLn "  http://localhost:8000/pairs/"
  run 8000 $ logStdoutDev $ app
