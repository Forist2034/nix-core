{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module HsNix.Derivation.Drv.Internal.Hash (AlgoWrapper (..)) where

import Crypto.Hash
import Data.Proxy
import HsNix.Hash
import System.Nix.Hash

newtype AlgoWrapper a = AW a

deriving instance (HashAlgorithm a) => HashAlgorithm (AlgoWrapper a)

instance (NamedHashAlgo a) => NamedAlgo (AlgoWrapper a) where
  algoName = hashAlgoName (Proxy :: Proxy a)
