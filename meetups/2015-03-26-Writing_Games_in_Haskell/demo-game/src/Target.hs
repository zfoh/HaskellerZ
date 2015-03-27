{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Target where

import Control.Lens
import Data.AffineSpace
import Data.Default
import Data.Typeable
import Data.VectorSpace
import Graphics.Gloss

import Moving

data TargetTag = Holder | Goal  deriving (Eq,Show,Ord,Enum,Typeable)

goalR, holderR :: Float
goalR = 70
holderR = 30


data Target
  = Target
    { _tPos :: !Point
    , _tSpd :: !(Float,Float)
    , _tTag :: !TargetTag
    } deriving (Show,Typeable)
makeLenses ''Target

instance Default Target where
  def = Target (0,0) (0,0) Goal

instance Moving Target where
  pos = tPos
  spd = tSpd
  radius = targetR . _tTag

targetR :: TargetTag -> Float
targetR Holder = holderR
targetR Goal = goalR

drawTarget :: Target -> Picture
drawTarget Target{..} = case _tTag of
  Holder -> translateP _tPos $ Color black $ circleSolid holderR
  Goal -> translateP _tPos $ Color black $ thickCircle goalR 8

stepTarget :: Float -> Target -> Target
stepTarget dt t = t & pos %~ (.+^ dt *^ t^.spd)
