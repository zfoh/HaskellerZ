{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Orphan instances for classes defined in the 'base' library.
module Orphans.Lib_base () where

import qualified Crypto.Types.PubKey.RSA as RSA

import           Data.String (IsString(fromString))
import           Data.Tagged (Tagged(..))

import           GHC.Generics


-- tagged

instance IsString a => IsString (Tagged tag a) where
    fromString = Tagged . fromString


-- crypto-types: RSA

instance Ord RSA.PublicKey where
        compare a b = check a b
          where check (RSA.PublicKey x1 x2 x3) (RSA.PublicKey y1 y2 y3)
                  = compare x1 y1 `_then` compare x2 y2 `_then` compare x3 y3 `_then`
                      EQ
                _then EQ x = x
                _then x _ = x

instance Ord RSA.PrivateKey where
        compare a b = check a b
          where check (RSA.PrivateKey x1 x2 x3 x4 x5 x6 x7)
                  (RSA.PrivateKey y1 y2 y3 y4 y5 y6 y7)
                  = compare x1 y1 `_then` compare x2 y2 `_then` compare x3 y3 `_then`
                      compare x4 y4
                      `_then` compare x5 y5
                      `_then` compare x6 y6
                      `_then` compare x7 y7
                      `_then` EQ
                _then EQ x = x
                _then x _ = x

deriving instance Generic RSA.PublicKey
deriving instance Generic RSA.PrivateKey


