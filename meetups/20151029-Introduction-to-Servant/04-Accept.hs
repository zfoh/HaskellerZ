{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

-- An endpoint can deliver data in different formats. The Accept sent
-- by the cliet header is used to determine the correct format.

import Servant
import Network.Wai (Application)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Handler.Warp (run)
import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8       as BC

type PairsEndpoint = "pairs" :> Get '[PlainText, JSON] [Pair]

type API = "hello"   :> Get '[JSON] String
      :<|> "numbers" :> Get '[JSON] [Int]
      :<|> PairsEndpoint

helloServer = return "Hello World!"

numberServer = return [1..17]

data Pair = Pair
    { x :: Int
    , y :: Int
    } deriving (Eq, Show, Generic)

instance ToJSON Pair

instance MimeRender PlainText a => MimeRender PlainText [a] where
    mimeRender p xs =  BC.intercalate ", " (map (mimeRender p) xs)

instance MimeRender PlainText Pair where
    mimeRender _ =  BC.pack . show

pairServer :: Server PairsEndpoint
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
  putStrLn "  http://localhost:8000/pairs/ (application/json, text/plain)"
  run 8000 $ logStdoutDev $ app
