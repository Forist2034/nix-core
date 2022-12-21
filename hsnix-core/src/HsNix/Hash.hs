{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module HsNix.Hash
  ( NamedHashAlgo (..),
    SHA1,
    SHA256,
    SHA512,
    HashMode (..),
    Hash (..),
  )
where

import Crypto.Hash
import Data.ByteArray.Hash
import Data.Hashable
import Data.Proxy
import Data.Text (Text)

class (HashAlgorithm a) => NamedHashAlgo a where
  hashAlgoName :: Proxy a -> Text

instance NamedHashAlgo SHA1 where
  hashAlgoName _ = "sha1"

instance NamedHashAlgo SHA256 where
  hashAlgoName _ = "sha256"

instance NamedHashAlgo SHA512 where
  hashAlgoName _ = "sha512"

data HashMode
  = HashFlat
  | HashRecursive
  deriving (Show, Eq)

instance Hashable HashMode where
  hashWithSalt s v = hashWithSalt s (v == HashFlat)

newtype Hash a = Hash (Digest a)
  deriving (Eq)
  deriving newtype (Show, Read)

instance Hashable (Hash a) where
  hashWithSalt _ (Hash a) =
    case fnv1Hash a of
      FnvHash32 w -> fromIntegral w