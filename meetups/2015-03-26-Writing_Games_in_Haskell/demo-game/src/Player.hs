{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Player where

import Control.Lens
import Data.AffineSpace
import Data.Default
import Data.VectorSpace
import Graphics.Gloss

import Keys

playerR :: Float
playerR = 30

data Player
  = Player
    { _pPos :: !Point
    , _pSpd :: !(Float, Float)
    } deriving (Show)
makeLenses ''Player

instance Default Player where
  def = Player (0,0) (0,0)

drawPlayer :: Player -> Picture
drawPlayer Player{..} = translateP _pPos $ circleSolid playerR

translateP :: Point -> Picture -> Picture
translateP (x,y) = translate x y


stepPlayer :: Float -> Player -> Player
stepPlayer dt p@Player{..} = p & pPos %~ (.+^ (dt *^_pSpd))


updatePlayerByKeys :: ActiveKeys -> Player -> Player
updatePlayerByKeys (ActiveKeys left right down) = pSpd .~ spd
  where
    spd = 50 * (horiz ^+^ vert)
    vert = if down then (0,-1) else (0,0)
    horiz = case (left,right) of
      (True, False) -> (-1,0)
      (False, True) -> (1,0)
      _ -> (0,0)
