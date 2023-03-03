{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

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
import qualified Data.ByteArray as BA
import Data.ByteArray.Hash
import qualified Data.ByteString.Unsafe as BSU
import Data.Hashable
import Data.Maybe (fromJust)
import Data.Proxy (Proxy)
import Data.Text (Text)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.IO.Unsafe (unsafeDupablePerformIO)

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
  deriving (Show, Eq, Lift)

instance Hashable HashMode where
  hashWithSalt s v = hashWithSalt s (v == HashFlat)

newtype Hash a = Hash (Digest a)
  deriving (Eq)
  deriving newtype (Show, Read)

instance Hashable (Hash a) where
  hashWithSalt _ (Hash a) =
    case fnv1Hash a of
      FnvHash32 w -> fromIntegral w

instance HashAlgorithm a => Lift (Hash a) where
  lift = unTypeCode . liftTyped
  liftTyped (Hash h) =
    [||
    Hash
      ( fromJust
          ( digestFromByteString
              ( unsafeDupablePerformIO
                  ( BSU.unsafePackAddressLen
                      $$(liftTyped (BA.length h))
                      $$(unsafeCodeCoerce (litE (stringPrimL (BA.unpack h))))
                  )
              )
          )
      )
    ||]
