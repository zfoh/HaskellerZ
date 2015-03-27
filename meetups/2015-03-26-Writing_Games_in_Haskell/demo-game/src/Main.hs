{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Arrow
import Control.Lens
import Data.Default
import Data.Foldable
import Data.Monoid
import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Interface.Pure.Game

import Player
import Keys

data World
  = World
    { _wPlayers :: ![Player]
    , _wKeys :: !KeySet
    , _wSize :: !(Float,Float)
    } deriving (Show)
makeLenses ''World

iniWorld :: World
iniWorld = World [player0, player1, player2] mempty iniSize

player0 :: Player
player0 = def & pColor .~ blue & pPos .~ (300,300) & pAng .~ degToRad 225

player1 :: Player
player1 = def & pColor .~ red & pPos .~ (-300,-300) & pAng .~ degToRad 45

player2 :: Player
player2 = def & pColor .~ green & pPos .~ (300,-300) & pAng .~ degToRad 135


iniSize :: Num a => (a,a)
iniSize = (1200,800)

borderWidth :: Float
borderWidth = 10

arenaSize :: World -> (Float,Float)
arenaSize = view wSize >>> both -~ (2*borderWidth)

drawWorld :: World -> Picture
drawWorld World{..} = drawBorder _wSize <> foldMap drawPlayer _wPlayers
  where
    drawBorder (w,h) = color (dark $ dark violet) $ Pictures [ l, r, t, b ]
      where
        bw = 2 * borderWidth
        l = translate (-w/2) 0 $ rectangleSolid bw h
        r = translate (w/2) 0 $ rectangleSolid bw h
        b = translate 0 (-h/2) $ rectangleSolid w bw
        t = translate 0 (h/2) $ rectangleSolid w bw


stepWorld :: Float -> World -> World
stepWorld dt w
  = w & wPlayers.traverse %~ stepPlayer dt
      & wPlayers.traverse %~ bounceOffBorder (arenaSize w)

handleEvents :: Event -> World -> World
handleEvents ev = saveSize >>> wKeys %~ updateKeys ev >>> updatePlayers
  where
    updatePlayers w@World{..} =
      w & wPlayers.traversed %@~ \i -> updatePlayerByKeys (activeForId i _wKeys)
    saveSize = case ev of
      EventResize sz -> wSize .~ (sz & both %~ fromIntegral)
      _ -> id

main :: IO ()
main = play window white 60 iniWorld drawWorld handleEvents stepWorld
  where
    window = InWindow "Demo Game" iniSize (0,0)
