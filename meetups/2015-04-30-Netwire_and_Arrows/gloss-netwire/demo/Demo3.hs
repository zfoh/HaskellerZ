{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Control.Monad.Fix (MonadFix)
import Control.Wire hiding ((<>))
import Data.Monoid
import Graphics.Gloss
import Graphics.Gloss.Interface.FRP.Netwire
import Data.VectorSpace

type XV = (Float, Float)
type VA = (Float, Float)

integralRK4 :: (HasTime Float s) => (XV -> Float) -> XV -> Wire s e m a XV
integralRK4 accFn = go
  where
    step :: XV -> Float -> VA -> VA
    step (x,v) dt (dx,dv) = (v', accFn (x',v'))
      where
        x' = x + dt * dx
        v' = v + dt * dv

    go xv = mkSF $ \ds _ -> let
      dt = dtime ds
      a = step xv 0 (0,0)
      b = step xv (dt/2) a
      c = step xv (dt/2) b
      d = step xv dt c
      xv' = xv ^+^ (dt/6) *^ (a ^+^ 2*^(b ^+^ c) ^+^ d)
      in (xv', go xv')


collect :: Wire s e m a [a]
collect = go []
  where
    go l = mkSFN $ \x -> let l' = x : l in (l', go l')

game :: (MonadFix m, HasTime Float s) => Wire s e m a Picture
game = proc _ -> do
  rec
    (x,_) <- integralRK4 (\(x,_) -> (-1)*x) (300,0) -< ()
    (y,_) <- integralRK4 (\(x,_) -> (-2)*x) (200,0) -< ()
  path <- collect -< (x,y)
  returnA -< line path <> translate x y (circleSolid 20)

main :: IO ()
main = playWire (InWindow "Gloss-Netwire demo" (800,600) (0,0)) white 60 game
