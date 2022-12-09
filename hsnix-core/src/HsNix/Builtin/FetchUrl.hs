module HsNix.Builtin.FetchUrl
  ( FetchUrlArg (name, url, hashAlgo, outputHash, isExecutable, unpack),
    defFetchUrlArg,
    BuiltinFetchUrl (..),
    fetchUrlStr,
  )
where

import Data.Text (Text)
import HsNix.Derivation
import HsNix.Hash

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

fetchUrlStr :: BuiltinFetchUrl m => FetchUrlArg -> m (DrvStr m)
fetchUrlStr f = storePathStr (fetchUrl f)