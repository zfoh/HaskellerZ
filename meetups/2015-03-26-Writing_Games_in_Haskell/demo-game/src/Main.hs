{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Arrow
import Control.Lens
import Data.AffineSpace
import Data.Default
import Data.Foldable
import Data.Monoid
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Interface.Pure.Game
import System.Random

import Player
import Keys

data World
  = World
    { _wPlayers :: ![Player]
    , _wKeys :: !KeySet
    , _wSize :: !(Float,Float)
    , _wToken :: !Int
    , _wGrace :: !Float
    , _wShowScores :: !Bool
    } deriving (Show)
makeLenses ''World

iniWorld :: IO World
iniWorld = do
  let playerList = [player0, player1, player2]
  iniTokenHolder <- randomRIO (0, length playerList - 1)
  return $ World playerList mempty iniSize iniTokenHolder graceT True

player0 :: Player
player0 = def & pColor .~ blue & pPos .~ (300,300) & pAng .~ degToRad 225 & pName .~ "Blue"

player1 :: Player
player1 = def & pColor .~ red & pPos .~ (-300,-300) & pAng .~ degToRad 45 & pName .~ "Red"

player2 :: Player
player2 = def & pColor .~ green & pPos .~ (300,-300) & pAng .~ degToRad 135 & pName .~ "Green"

iniSize :: Num a => (a,a)
iniSize = (1200,800)

borderWidth :: Float
borderWidth = 10

graceT :: Float
graceT = 2

arenaSize :: World -> (Float,Float)
arenaSize = view wSize >>> both -~ (2*borderWidth)



drawWorld :: World -> Picture
drawWorld world@World{..} = drawBorder _wSize <> scores <> foldMap drawPlayer _wPlayers
                            <> token
  where
    scores = if not _wShowScores then Blank
             else ifoldMap (drawScore (arenaSize world)) _wPlayers
    drawBorder (w,h) = color (dark $ dark violet) $ Pictures [ l, r, t, b ]
      where
        bw = 2 * borderWidth
        l = translate (-w/2) 0 $ rectangleSolid bw h
        r = translate (w/2) 0 $ rectangleSolid bw h
        b = translate 0 (-h/2) $ rectangleSolid w bw
        t = translate 0 (h/2) $ rectangleSolid w bw
    token = translateP tokenPos $ color tokenColor $ circleSolid 15
    tokenPos = _pPos $ _wPlayers !! _wToken
    tokenColor = if _wGrace <= 0 then light yellow else greyN 0.7


stepWorld :: Float -> World -> World
stepWorld dt w@World{..}
  = w & wPlayers.traverse %~ stepPlayer dt
      & wPlayers.traverse %~ bounceOffBorder (arenaSize w)
      & wGrace -~ dt
      & updateScore
      & tokenInteractions
  where
    updateScore = if _wGrace < 0 then wPlayers.ix _wToken.pScore +~ dt else id

tokenInteractions :: World -> World
tokenInteractions w@World{..} = if _wGrace < 0 then upd w else w
  where
    c = _wToken
    cur = _pPos $ _wPlayers !! c
    hits = _wPlayers ^@.. traversed<.pPos & filter ((/=c) . fst) & filter (closeby . snd)
    closeby pos = magV (pos .-. cur) < 2 * playerR
    upd = case hits of
      [] -> id
      ((n,_):_) -> wToken .~ n >>> wGrace .~ graceT


handleEvents :: Event -> World -> World
handleEvents ev = saveSize >>> wKeys %~ updateKeys ev >>> updatePlayers >>> toggleDisplay
  where
    updatePlayers w@World{..} =
      w & wPlayers.traversed %@~ \i -> updatePlayerByKeys (activeForId i _wKeys)
    saveSize = case ev of
      EventResize sz -> wSize .~ (sz & both %~ fromIntegral)
      _ -> id
    toggleDisplay = case ev of
      EventKey (Char 't') Down _ _ -> wShowScores %~ not
      _ -> id


main :: IO ()
main = do
  world <- iniWorld
  let window = InWindow "Demo Game" iniSize (0,0)
  play window white 60 world drawWorld handleEvents stepWorld
