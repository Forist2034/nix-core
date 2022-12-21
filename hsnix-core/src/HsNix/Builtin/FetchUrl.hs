module HsNix.Builtin.FetchUrl
  ( FetchUrlArg (name, url, outputHash, isExecutable, unpack),
    defFetchUrlArg,
    BuiltinFetchUrl (..),
    fetchUrlStr,
  )
where

import Data.Text (Text)
import HsNix.Derivation
import HsNix.Hash

data FetchUrlArg a = FetchUrlArg
  { name, url :: Text,
    outputHash :: Hash a,
    isExecutable, unpack :: Bool
  }
  deriving (Eq, Show)

defFetchUrlArg :: Text -> Text -> Hash a -> FetchUrlArg a
defFetchUrlArg n u h =
  FetchUrlArg
    { name = n,
      url = u,
      outputHash = h,
      isExecutable = False,
      unpack = False
    }

class (MonadDeriv m) => BuiltinFetchUrl m where
  fetchUrl :: NamedHashAlgo a => FetchUrlArg a -> Derivation m

fetchUrlStr :: (BuiltinFetchUrl m, NamedHashAlgo a) => FetchUrlArg a -> m (DrvStr m)
fetchUrlStr f = storePathStr (fetchUrl f)