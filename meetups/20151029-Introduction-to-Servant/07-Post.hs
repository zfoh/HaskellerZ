{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

-- POST data is automatically deserialized using the FromJSON type
-- class before being passed to request handlers.

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Data.Aeson
import GHC.Generics
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant

data Pair = Pair
    { x :: Int
    , y :: Int
    } deriving (Eq, Show, Generic)

instance ToJSON Pair
instance FromJSON Pair

data PostStatus = PostStatus
    { msg :: String
    } deriving (Eq, Show, Generic)

instance ToJSON PostStatus

type API = "pairs" :> Get '[JSON] [Pair]
      :<|> "pairs" :> ReqBody '[JSON] Pair
                   :> Post '[JSON] PostStatus

getPairsServer ref = liftIO $ readMVar ref >>= return

postPairServer ref pair = do
  i <- liftIO $ modifyMVar ref addPair
  return $ PostStatus $ "Your pair is number " ++ (show i)
    where
      addPair :: [Pair] -> IO ([Pair], Int)
      addPair pairs = return (pair:pairs, 1 + length pairs)

server :: MVar [Pair] -> Server API
server ref = getPairsServer ref
        :<|> postPairServer ref

proxy :: Proxy API
proxy = Proxy

app :: MVar [Pair] -> Application
app ref = serve proxy $ server ref

main :: IO ()
main = do
  putStrLn "Server endpoints:"
  putStrLn "  http://localhost:8000/pairs/ (GET, POST)"
  ref <- newMVar []
  run 8000 $ logStdoutDev $ app ref


-- $ curl http://localhost:8000/pairs/

-- $ curl -d '{"x": 10, "y": 20}' http://localhost:8000/pairs/

-- $ curl -H 'Content-Type: application/json' -d 'bla bla' http://localhost:8000/pairs/

-- $ curl -H 'Content-Type: application/json' -d '{"x": 10, "y": 20}' http://localhost:8000/pairs/
