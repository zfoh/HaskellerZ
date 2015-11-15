{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- Simple Hello World web server.

import Servant
import Network.Wai (Application)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Handler.Warp (run)
import GHC.Generics

-- For servant-server < 0.5:
--
-- import qualified Data.ByteString.Lazy.Char8 as BC
-- instance MimeRender PlainText String where
--     mimeRender _ = BC.pack

type API = Get '[PlainText] String

server :: Server API
server = return "Hello World!\n"

proxy :: Proxy API
proxy = Proxy

app :: Application
app = serve proxy server

main :: IO ()
main = do
  putStrLn "Server endpoints:"
  putStrLn "  http://localhost:8000/"
  run 8000 $ logStdoutDev $ app
