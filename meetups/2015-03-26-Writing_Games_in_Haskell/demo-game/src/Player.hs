{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Player where

import Control.Arrow
import Control.Lens
import Data.AffineSpace
import Data.Default
import Data.VectorSpace
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle

import Keys

playerR :: Float
playerR = 30

data Player
  = Player
    { _pPos :: !Point
    , _pSpd :: !(Float, Float)
    , _pAng :: !Float
    , _pRot :: !Float
    , _pColor :: !Color
    , _pFire :: !Bool
    } deriving (Show)
makeLenses ''Player

instance Default Player where
  def = Player (0,0) (0,0) 0 0 black False

drawPlayer :: Player -> Picture
drawPlayer Player{..} = translateP _pPos $ rotate (-radToDeg _pAng) $
                        Pictures [ body1, body2, flame ]
  where
    body1 = color (dim $ dim _pColor) $ circleSolid playerR
    body2 = color (light _pColor) $ polygon [(60,0),(-42,-12),(-32,0),(-42,12)]
    flamePath = [(-50,0), (-60,6), (-70,8), (-80,6), (-90,0)]
    flamePath' = tail $ reverse $ tail $ map (second negate) flamePath
    flame = if not _pFire then Blank
            else color red $ polygon $ flamePath ++ flamePath'


translateP :: Point -> Picture -> Picture
translateP (x,y) = translate x y


stepPlayer :: Float -> Player -> Player
stepPlayer dt p@Player{..} = p & pPos .~ pos' & pSpd .~ spd' & pAng +~ 3*dt*_pRot
  where
    pos' = _pPos .+^ dt *^ _pSpd
    spd' = _pSpd ^+^ dt *^ acc
    acc = if _pFire then 72 *^ dir else (0,0)
    dir = unitVectorAtAngle _pAng

updatePlayerByKeys :: ActiveKeys -> Player -> Player
updatePlayerByKeys (ActiveKeys l r d) = pRot .~ rot >>> pFire .~ not d
  where
    rot = case (l,r) of
      (True, False) -> 1
      (False, True) -> -1
      _ -> 0
