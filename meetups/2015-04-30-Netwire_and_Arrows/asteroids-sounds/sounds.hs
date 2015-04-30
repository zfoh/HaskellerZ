module Main where

import Data.Word
import Control.Wire hiding (loop)
import FRP.Netwire (integral)
import Prelude hiding ((.), id)
import Sound.Pulse.Simple as Pulse
import System.Random

type SoundWire a b = Wire (Timed Double ()) () Identity a b

play :: SoundWire () Double -> IO ()
play wire = do
  pulse <- simpleNew Nothing "Haskell" Play Nothing "Netwire Sound demo"
           (SampleSpec (U8 Raw) 44100 1) Nothing Nothing
           -- ^ The sample rate above should have been 22050, but we
           -- double it to account for a bug in Ollie's code. (He uses
           -- two channels instead of one.)
  simpleWrite pulse $ render 22050 wire
  simpleDrain pulse
  simpleFree pulse

render :: Int -> SoundWire () Double -> [Word8]
render sampleRate wire = go wire
  where
    ds = Timed (1 / fromIntegral sampleRate) ()
    go w = let (r, w') = runIdentity $ stepWire w ds (Right ())
           in case r of
             Left _ -> []
             Right sample -> floor (127.999999 * (sample + 1)) : go w'

--------------------------------------------------------------------------------

sineConst :: Double -> SoundWire a Double
sineConst frequency = sin $ 2 * pi * pure frequency * time
-- alternatively:
-- sineConst frequency = sin . ((2*pi*frequency)*) <$> time

sineWrong :: SoundWire Double Double
sineWrong = sin $ 2 * pi * id * time

sine :: SoundWire Double Double
sine = sin $ 2 * pi * integral 0

sine' :: SoundWire Double Double
sine' = go 0
  where
    go theta = mkSF $ \s frequency ->
      let theta' = theta + 2 * pi * frequency * dtime s
      in (sin theta', go theta')

-- | Quantize a signal.
quantize :: Double -> SoundWire Double Double
quantize ratio = arr $ \s -> ratio * fromIntegral (floor (s / ratio) :: Int)

-- | Reduce sampling frequency.
rateReduce :: Int -> SoundWire Double Double
rateReduce rate = loop
  where
    loop = for period . keep --> loop
    period = 1 / fromIntegral rate

-- | Removed in Netwire 5.
keep :: (Monad m, Monoid s) => Wire s e m a a
keep = mkSF $ \_ x -> (x, pure x)

-- | Decay a single over a period of time, inhibiting after.
decay :: Double -> SoundWire Double Double
decay duration = id * (when (>0) . integral 1 . pure ((-1) / duration))

-- | Gate a signal to only pass above a certain threshold.
gate :: (Num a, Ord a, Arrow arr) => a -> arr a a
gate threshold = arr $ \s -> if s >= threshold then s else 0

-- | Noise within +/- 1
noise :: SoundWire a Double
noise = loop (mkStdGen 42)
  where
    loop gen = mkSF $ \_ _ ->
      let (x, gen') = randomR (-1,1) gen
      in (x, loop gen')

explosion :: SoundWire a Double
explosion = decay 2 . gate 0.4 . rateReduce 500 . quantize 0.2 . noise

death :: SoundWire a Double
death = decay 5 . gate 0.3 . rateReduce 3000 . quantize 0.2 . noise

-- | Clamp the signal between -1 and 1.
clamp :: (Num a, Ord a, Arrow arr) => arr a a
clamp = arr $ min 1 . max (-1)

pew :: SoundWire a Double
pew = decay 0.5 . rateReduce 5000 . ((clamp . (sinDrop * 1000)) / 2)
  where sinDrop = sine . (100 + decay 0.5 . 600)

ufo :: SoundWire a Double
ufo = for (1 / 3) . sine . ((sine . 3) * 200 + 100)


--------------------------------------------------------------------------------

main :: IO ()
main = play $ pew --> pew --> explosion --> ufo --> ufo --> ufo --> pew --> death

--------------------------------------------------------------------------------


playHIFI :: SoundWire () Double -> IO ()
playHIFI wire = do
  pulse <- simpleNew Nothing "Haskell" Play Nothing "Netwire Sound demo"
           (SampleSpec (F32 LittleEndian) 44100 1) Nothing Nothing
  simpleWrite pulse $ renderHIFI 44100 wire
  simpleDrain pulse
  simpleFree pulse

renderHIFI :: Int -> SoundWire () Double -> [Float]
renderHIFI sampleRate wire = go wire
  where
    ds = Timed (1 / fromIntegral sampleRate) ()
    go w = let
      (r, w') = runIdentity $ stepWire w ds (Right ())
      in case r of
        Left _ -> []
        Right sample -> realToFrac sample : go w'
