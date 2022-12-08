module Nix.Builtin.FetchUrl
  ( FetchUrlArg (name, url, hashAlgo, outputHash, isExecutable, unpack),
    defFetchUrlArg,
    BuiltinFetchUrl (..),
  )
where

import Data.Text (Text)
import Nix.Derivation
import Nix.Hash

data FetchUrlArg = FetchUrlArg
  { name, url :: Text,
    hashAlgo :: HashAlgo,
    outputHash :: Hash,
    isExecutable, unpack :: Bool
  }
  deriving (Eq, Show)

defFetchUrlArg :: Text -> Text -> HashAlgo -> Hash -> FetchUrlArg
defFetchUrlArg n u a h =
  FetchUrlArg
    { name = n,
      url = u,
      hashAlgo = a,
      outputHash = h,
      isExecutable = False,
      unpack = False
    }

class (MonadDeriv m) => BuiltinFetchUrl m where
  fetchUrl :: FetchUrlArg -> Derivation m