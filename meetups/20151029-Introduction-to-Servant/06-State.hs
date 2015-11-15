{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- Request handlers can use liftIO to run IO computations. By passing
-- a MVar down to an endpoint, the server can hold and manipulate
-- state that persists between requests.

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant


type StateAPI = "state" :> Get '[PlainText] String

server :: MVar Int -> Server StateAPI
server ref = do
  i <- liftIO $ modifyMVar ref (\i -> return (i + 1, i))
  return $ "Current count: " ++ (show i)

app :: MVar Int -> Application
app ref = serve (Proxy :: Proxy StateAPI) (server ref)

main :: IO ()
main = do
  putStrLn "Server endpoints:"
  putStrLn "  http://localhost:8000/state/"
  ref <- newMVar 0
  run 8000 $ logStdoutDev $ app ref
