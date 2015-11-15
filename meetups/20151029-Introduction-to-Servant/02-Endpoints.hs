{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

-- An API can have multiple endpoints combined with :<|>.

import Servant
import Network.Wai (Application)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Handler.Warp (run)
import qualified Data.ByteString.Lazy.Char8 as BC
import GHC.Generics

instance MimeRender PlainText Int where
    mimeRender _ = BC.pack . show

type API = "greeting" :> Get '[PlainText] String
      :<|> "status"   :> Get '[PlainText] Int

greetingServer = return "Hello World!\n"

statusServer = return 42

server :: Server API
server = greetingServer
    :<|> statusServer

proxy :: Proxy API
proxy = Proxy

app :: Application
app = serve proxy server

main :: IO ()
main = do
  putStrLn "Server endpoints:"
  putStrLn "  http://localhost:8000/greeting/"
  putStrLn "  http://localhost:8000/status/"
  run 8000 $ logStdoutDev $ app
