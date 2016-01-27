{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Orphans instances for classes defined in the 'aeson' library.
module Orphans.Lib_aeson () where

-- We require the Generic RSA.PublicKey and Generic RSA.PrivateKey instances
import           Orphans.Lib_base ()


import           Bound.Var   (Var(..))

import           Control.Monad ((>=>))

import qualified Crypto.Types.PubKey.RSA    as RSA

import           Data.Aeson
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.Map.Strict            as MS
import qualified Data.List.NonEmpty         as NE
import           Data.Tagged                (Tagged(..))
import           Data.Time                  (NominalDiffTime)
import           Data.Void                  (Void, absurd)


-- RSA

instance FromJSON RSA.PublicKey
instance FromJSON RSA.PrivateKey

instance ToJSON RSA.PublicKey
instance ToJSON RSA.PrivateKey


-- bytestring

instance ToJSON BS.ByteString where
    toJSON = toJSON . B8.unpack . B64.encode

instance FromJSON BS.ByteString where
    parseJSON src = do
      str <- parseJSON src
      either fail return $ B64.decode $ B8.pack str


-- semigroups

instance ToJSON a => ToJSON (NE.NonEmpty a) where
    toJSON = toJSON . NE.toList

instance FromJSON a => FromJSON (NE.NonEmpty a) where
    parseJSON val = do
      xs0 <- parseJSON val
      case xs0 of
        []     -> fail "expected non-empty list"
        (x:xs) -> pure (x NE.:| xs)


-- containers


-- | Note that this is also an instance for the lazy version of @Map@s, as
-- they share the same underlying representation via a type synonym.
instance (Ord a, FromJSON v, FromJSON a) => FromJSON (MS.Map a v) where
    parseJSON = fmap MS.fromList . parseJSON

-- | Note that this is also an instance for the lazy version of @Map@s, as
-- they share the same underlying representation via a type synonym.
instance (ToJSON a, ToJSON v) => ToJSON (MS.Map a v) where
    toJSON = toJSON . MS.toAscList



-- bound

instance (ToJSON a, ToJSON b) => ToJSON (Var a b)
instance (FromJSON a, FromJSON b) => FromJSON (Var a b)


-- tagged

instance ToJSON a => ToJSON (Tagged tag a) where
    toJSON = toJSON . unTagged

instance FromJSON a => FromJSON (Tagged tag a) where
    parseJSON = fmap Tagged . parseJSON


-- void

instance ToJSON Void where
    toJSON = absurd

instance FromJSON Void where
    parseJSON _ = fail "Void is uninhabited"

-- time
instance ToJSON NominalDiffTime where
    toJSON = toJSON . fromEnum

instance FromJSON NominalDiffTime where
    parseJSON = parseJSON >=> return . toEnum
