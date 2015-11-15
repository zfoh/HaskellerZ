{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- Use addHeader to send headers back to the client. Note that the
-- headers become part of the type describing the endpoint.

import Servant
import Network.Wai (Application)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Handler.Warp (run)
import GHC.Generics

type ResponseHeaders = '[ Header "X-Count" Int
                        , Header "X-Color" String
                        ]

type API = Get '[PlainText] (Headers ResponseHeaders String)

server :: Server API
server = return $ addHeader 42 $ addHeader "red" "Hello World!\n"

proxy :: Proxy API
proxy = Proxy

app :: Application
app = serve proxy server

main :: IO ()
main = do
  putStrLn "Server endpoints:"
  putStrLn "  http://localhost:8000/"
  run 8000 $ logStdoutDev $ app
