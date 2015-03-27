{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Lens
import Data.Default
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Control.Arrow
import Data.Monoid

import Player
import Keys

data World
  = World
    { _wPlayer :: !Player
    , _wKeys :: !KeySet
    , _wSize :: !(Float,Float)
    } deriving (Show)
makeLenses ''World

iniWorld :: World
iniWorld = World def mempty iniSize

iniSize :: Num a => (a,a)
iniSize = (1200,800)

borderWidth :: Float
borderWidth = 10

arenaSize :: World -> (Float,Float)
arenaSize = view wSize >>> both -~ (2*borderWidth)

drawWorld :: World -> Picture
drawWorld World{..} = drawBorder _wSize <> drawPlayer _wPlayer
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
  = w & wPlayer %~ stepPlayer dt
      & wPlayer %~ bounceOffBorder (arenaSize w)

handleEvents :: Event -> World -> World
handleEvents ev = saveSize >>> wKeys %~ updateKeys ev >>> updatePlayers
  where
    updatePlayers w@World{..} =
      w & wPlayer %~ updatePlayerByKeys (activeForId 0 _wKeys)
    saveSize = case ev of
      EventResize sz -> wSize .~ (sz & both %~ fromIntegral)
      _ -> id

main :: IO ()
main = play window white 60 iniWorld drawWorld handleEvents stepWorld
  where
    window = InWindow "Demo Game" iniSize (0,0)
