{-# LANGUAGE TemplateHaskell #-}

module Keys where

import Control.Lens
import Data.Set
import Graphics.Gloss.Interface.Pure.Game

data ActionKey = ALeft | ARight | ADown
               deriving (Eq,Ord,Enum,Show)

data PlayerKey = PKey !Int !ActionKey
               deriving (Eq,Ord,Show)

type KeySet = Set PlayerKey

playerKey :: Key -> Maybe PlayerKey
playerKey k = case k of
  SpecialKey KeyLeft  -> Just $ PKey 0 ALeft
  SpecialKey KeyRight -> Just $ PKey 0 ARight
  SpecialKey KeyDown  -> Just $ PKey 0 ADown
  _ -> Nothing

updateKeys :: Event -> KeySet -> KeySet
updateKeys ev = case ev of
  EventKey key upDown _ _ -> case playerKey key of
    Nothing -> id
    Just pkey -> case upDown of
      Down -> insert pkey
      Up -> delete pkey
  _ -> id

data ActiveKeys
  = ActiveKeys
    { _akLeft  :: !Bool
    , _akRight :: !Bool
    , _akDown  :: !Bool
    } deriving (Show)
makeLenses ''ActiveKeys

activeForId :: Int -> KeySet -> ActiveKeys
activeForId pId keys = ActiveKeys left right down
  where
    [left, right, down] =
      Prelude.map (\a -> member (PKey pId a) keys) [ALeft, ARight, ADown]
