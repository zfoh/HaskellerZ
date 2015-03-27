{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Player where

import Graphics.Gloss
import Control.Lens
import Data.Default

playerR :: Float
playerR = 30

data Player
  = Player
    { _pPos :: !Point
    } deriving (Show)
makeLenses ''Player

instance Default Player where
  def = Player (0,0)

drawPlayer :: Player -> Picture
drawPlayer Player{..} = translateP _pPos $ circleSolid playerR

translateP :: Point -> Picture -> Picture
translateP (x,y) = translate x y


stepPlayer :: Float -> Player -> Player
stepPlayer dt = pPos._1 +~ 50*dt
