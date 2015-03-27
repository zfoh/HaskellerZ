{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad.Trans.State.Strict
import Data.Default
import Data.Foldable
import Data.Monoid
import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Interface.Pure.Game
import System.Random

import Keys
import Moving
import Player
import Target

data World
  = World
    { _wPlayers :: ![Player]
    , _wKeys :: !KeySet
    , _wSize :: !(Float,Float)
    , _wToken :: !Int
    , _wGrace :: !Float
    , _wShowScores :: !Bool
    , _wRandom :: !StdGen
    , _wTarget :: !Target
    } deriving (Show)
makeLenses ''World

iniWorld :: IO World
iniWorld = do
  rnd <- newStdGen
  let world0 = World [player0, player1, player2] mempty iniSize 0 graceT True rnd def
  return $ generateTarget Goal world0

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

allThings :: World -> [MovingThing]
allThings World{..} = toThing _wTarget : map toThing _wPlayers

drawWorld :: World -> Picture
drawWorld world@World{..} = drawBorder _wSize <> scores
                            <> drawTarget _wTarget
                            <> foldMap drawPlayer _wPlayers
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
    tokenPos = view pos $ allThings world !! _wToken
    tokenColor = if _wGrace <= 0 then light yellow else greyN 0.7


stepWorld :: Float -> World -> World
stepWorld dt w@World{..}
  = w & wPlayers.traverse %~ stepPlayer dt
      & wPlayers.traverse %~ bounceOffBorder (arenaSize w)
      & wTarget %~ stepTarget dt
      & wTarget %~ bounceOffBorder (arenaSize w)
      & wGrace -~ dt
      & tokenInteractions
      & changeTargetIfNeeded

changeTargetIfNeeded :: World -> World
changeTargetIfNeeded w@World{..} = w & upd
  where
    upd = case (_wToken == 0, _tTag _wTarget) of
      (False, Holder) -> generateTarget Goal
      (True, Goal) -> generateTarget Holder
      _ -> id

tokenInteractions :: World -> World
tokenInteractions w@World{..} = if _wGrace < 0 then upd w else w
  where
    c = _wToken
    everybody = allThings w
    cur = everybody !! c
    hits = everybody ^@.. traversed & filter ((/=c) . fst) & filter (touching cur . snd)
    upd = case hits of
      [] -> id
      ((n,_):_) -> wToken .~ n >>> wGrace .~ graceT >>> updScore n
    updScore n | n == 0, c > 0 = wPlayers.ix (c-1).pScore +~ 1
               | otherwise     = id

wRandomR :: Random a => (a,a) -> State World a
wRandomR = zoom wRandom . state . randomR

generateTarget :: TargetTag -> World -> World
generateTarget tag = execState $ do
  sz <- arenaSize <$> get
  let (w,h) = sz & both %~ (/2) & both -~ targetR tag
  x <- wRandomR (-w,w)
  y <- wRandomR (-h,h)
  vx <- wRandomR (-200,200)
  vy <- wRandomR (-200,200)
  wTarget .= Target (x,y) (vx,vy) tag


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
