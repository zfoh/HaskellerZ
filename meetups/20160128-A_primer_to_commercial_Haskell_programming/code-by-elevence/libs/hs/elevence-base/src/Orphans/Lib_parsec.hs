{-# OPTIONS_GHC -fno-warn-orphans #-}

module Orphans.Lib_parsec () where

import Data.Aeson
import Text.Parsec.Error
import Text.Parsec.Pos

instance Ord ParseError where
  lhs `compare` rhs = errorMessages lhs `compare` errorMessages rhs

instance ToJSON SourcePos where
  toJSON p =
    let
      name   = sourceName p
      line   = sourceLine p
      column = sourceColumn p
    in
      toJSON (name, line, column)

instance FromJSON SourcePos where
  parseJSON v = do
    (name, line, column) <- parseJSON v
    return $ newPos name line column

instance ToJSON Message where
  toJSON = toJSON . fromEnum

instance FromJSON Message where
  parseJSON v = do
    e <- parseJSON v
    return $ toEnum e

instance ToJSON ParseError where
  toJSON err = do
    let pos  = errorPos err
    let msgs = errorMessages err
    toJSON (pos, msgs)

instance FromJSON ParseError where
  parseJSON v = do
    (pos, msgs) <- parseJSON v
    case msgs of
      []     -> return $ newErrorUnknown pos
      (m:ms) -> return $ foldr addErrorMessage (newErrorMessage m pos) ms
