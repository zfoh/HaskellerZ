{-# LANGUAGE Arrows #-}

module Main (main) where

import Control.Wire
import FRP.Netwire.Move (integral)
import Graphics.Gloss
import Graphics.Gloss.Interface.FRP.Netwire
import Control.Monad.Fix (MonadFix)

game :: (MonadFix m, HasTime t s) => Wire s e m a Picture
game = proc _ -> do
  rec
    x <- integral 300 -< vx
    vx <- integral 0 -< -x
  returnA -< translate x 0 $ circleSolid 20

main :: IO ()
main = playWire (InWindow "Gloss-Netwire demo" (800,600) (0,0)) white 60 game
