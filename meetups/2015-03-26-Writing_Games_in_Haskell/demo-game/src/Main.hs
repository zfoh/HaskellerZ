{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Graphics.Gloss
import Control.Lens
import Data.Default

import Player

data World
  = World
    { _wPlayer :: !Player
    } deriving (Show)
makeLenses ''World

iniWorld :: World
iniWorld = World def

drawWorld :: World -> Picture
drawWorld World{..} = drawPlayer _wPlayer

stepWorld :: Float -> World -> World
stepWorld dt = wPlayer %~ stepPlayer dt

main :: IO ()
main = play window white 60 iniWorld drawWorld (const id) stepWorld
  where
    window = InWindow "Demo Game" (1200,800) (0,0)
