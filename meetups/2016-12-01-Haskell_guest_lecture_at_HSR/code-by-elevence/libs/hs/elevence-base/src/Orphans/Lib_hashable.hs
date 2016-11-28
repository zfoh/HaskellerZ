{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Orphan instances for classes defined in the 'hashable' library.
module Orphans.Lib_hashable () where

-- we require the Generics orphans
import           Orphans.Lib_base ()

import qualified Crypto.Types.PubKey.RSA as RSA

import           Data.Hashable
import           Data.Tagged


-- RSA

instance Hashable RSA.PublicKey
instance Hashable RSA.PrivateKey

-- tagged

instance Hashable a => Hashable (Tagged tag a) where
    hashWithSalt salt = hashWithSalt salt . unTagged
