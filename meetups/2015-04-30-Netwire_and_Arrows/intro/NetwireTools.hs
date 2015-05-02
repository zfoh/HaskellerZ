module NetwireTools where

import Data.Functor.Identity
import Data.Time.Clock (NominalDiffTime)
import Control.Concurrent
import Control.Wire.Core
import Control.Wire.Session
import System.IO

testFreq :: Int
testFreq = 50

iterDelay :: Int
iterDelay = 1000000 `div` testFreq

stepDelta :: Timed NominalDiffTime ()
stepDelta = Timed (1 / fromIntegral testFreq) ()


testSimpleWire ::
  (Show b, Show e)
  => Wire (Timed NominalDiffTime ()) e Identity () b
  -> IO ()
testSimpleWire w0 = loop w0
  where
    loop w = do
      putStr "\r"
      let (eb, w') = runIdentity $ stepWire w stepDelta (Right ())
      putStr $ either (("I: "++) . show) show eb
      putStr "\ESC[K"
      hFlush stdout
      threadDelay iterDelay
      loop w'



--------------------------------------------------------------------------------

debugWire :: (Show s, Show e, Show a) => Wire s e IO a a
debugWire = WGen $ \s ea -> do
  putStr $ "\n\ESC[K   s: " ++ show s ++ ", ea: " ++ show ea ++ "\ESC[1A\r"
  return (ea, debugWire)


testWireIO ::
  (Show b, Show e)
  => Session IO s
  -> Wire s e IO () b
  -> IO ()
testWireIO sess0 w0 = loop sess0 w0
  where
    loop sess w = do
      putStr "\r\ESC[K"
      (ds, sess') <- stepSession sess
      (eb, w') <- stepWire w ds (Right ())
      putStr $ either (("I: "++) . show) show eb
      hFlush stdout
      threadDelay iterDelay
      loop sess' w'
