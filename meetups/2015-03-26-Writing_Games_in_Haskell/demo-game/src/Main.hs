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
    } deriving (Show)
makeLenses ''World

iniWorld :: World
iniWorld = World def mempty

drawWorld :: World -> Picture
drawWorld World{..} = drawPlayer _wPlayer

stepWorld :: Float -> World -> World
stepWorld dt = wPlayer %~ stepPlayer dt

handleEvents :: Event -> World -> World
handleEvents ev = wKeys %~ updateKeys ev >>> updatePlayers
  where
    updatePlayers w@World{..} =
      w & wPlayer %~ updatePlayerByKeys (activeForId 0 _wKeys)

main :: IO ()
main = play window white 60 iniWorld drawWorld handleEvents stepWorld
  where
    window = InWindow "Demo Game" (1200,800) (0,0)
