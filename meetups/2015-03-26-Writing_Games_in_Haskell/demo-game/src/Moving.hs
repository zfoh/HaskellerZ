{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Moving where

import Control.Applicative
import Data.Typeable
import Control.Lens
import Data.AffineSpace
import Graphics.Gloss
import Graphics.Gloss.Data.Vector

class Moving thing where
  pos :: Lens' thing Point
  spd :: Lens' thing (Float,Float)
  radius :: thing -> Float

translateP :: Point -> Picture -> Picture
translateP (x,y) = translate x y

bounceOffBorder :: Moving thing => (Float,Float) -> thing -> thing
bounceOffBorder arenaSize t = t & horiz & vert
  where
    (w,h) = arenaSize & both %~ (/2) & both -~ radius t
    (x,y) = t ^. pos
    (vx, vy) = t ^. spd
    horiz = if abs x > w && signum x == signum vx then spd._1 %~ negate else id
    vert  = if abs y > h && signum y == signum vy then spd._2 %~ negate else id

touching :: (Moving t1, Moving t2) => t1 -> t2 -> Bool
touching t1 t2 = magV (t1^.pos .-. t2^.pos) < radius t1 + radius t2

data MovingThing where
  MovingThing :: Moving t => t -> MovingThing
  deriving (Typeable)

instance Moving MovingThing where
  pos f (MovingThing t) = MovingThing <$> pos f t
  spd f (MovingThing t) = MovingThing <$> spd f t
  radius (MovingThing t) = radius t

asThing :: (Typeable thing, Moving thing) => Prism' MovingThing thing
asThing = prism' MovingThing cast

toThing :: Moving thing => thing -> MovingThing
toThing = MovingThing
