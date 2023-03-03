{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module HsNix.Internal.OutputName (
  OutputName (..),
  validOutputName,
  makeOutputName,
  makeOutputNameOrFail,
) where

import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Stack (HasCallStack)
import HsNix.Internal.StorePathName

newtype OutputName = OutputName {outputNameText :: Text}
  deriving (Show, Eq, Ord, Hashable)

validOutputName :: Text -> Maybe String
validOutputName = validStorePathName

makeOutputName :: Text -> Either String OutputName
makeOutputName t = maybe (Right (OutputName t)) Left (validOutputName t)

makeOutputNameOrFail :: HasCallStack => Text -> OutputName
makeOutputNameOrFail t =
  case validOutputName t of
    Nothing -> OutputName t
    Just e -> error ("Invalid output name " ++ show t ++ ": " ++ e)