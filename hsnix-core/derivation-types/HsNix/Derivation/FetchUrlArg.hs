module HsNix.Derivation.FetchUrlArg (
  FetchUrlArg (..),
  defFetchUrlArg,
) where

import Data.Text (Text)
import HsNix.Hash
import HsNix.StorePathName

data FetchUrlArg a = FetchUrlArg
  { faName :: StorePathName,
    faUrl :: Text,
    faOutputHash :: Hash a,
    faIsExecutable, faUnpack :: Bool
  }
  deriving (Eq, Show)

defFetchUrlArg :: StorePathName -> Text -> Hash a -> FetchUrlArg a
defFetchUrlArg n u h =
  FetchUrlArg
    { faName = n,
      faUrl = u,
      faOutputHash = h,
      faIsExecutable = False,
      faUnpack = False
    }