{-# LANGUAGE Arrows #-}

module Main (main) where

import Control.Monad.Fix (MonadFix)
import Control.Wire hiding ((<>))
import Data.Monoid
import FRP.Netwire.Move (integral)
import Graphics.Gloss
import Graphics.Gloss.Interface.FRP.Netwire

collect :: Wire s e m a [a]
collect = go []
  where
    go l = mkSFN $ \x -> let l' = x : l in (l', go l')

game :: (MonadFix m, HasTime t s) => Wire s e m a Picture
game = proc _ -> do
  rec
    x <- integral 300 -< vx
    vx <- integral 0 -< (-1 * x)
  rec
    y <- integral 200 -< vy
    vy <- integral 0 -< (-2 * y)
  path <- collect -< (x,y)
  returnA -< line path <> translate x y (circleSolid 20)

main :: IO ()
main = playWire (InWindow "Gloss-Netwire demo" (800,600) (0,0)) white 60 game
