{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module HsNix.Internal.OutputName (
  OutputName (..),
  fodOutputName,
  validOutputName,
  makeOutputName,
  makeOutputNameThrow,
) where

import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Stack (HasCallStack)
import HsNix.Internal.StorePathName

newtype OutputName = OutputName {outputNameText :: Text}
  deriving (Show, Eq, Ord, Hashable)

fodOutputName :: OutputName
fodOutputName = OutputName "out"

validOutputName :: Text -> Maybe String
validOutputName = validStorePathName

makeOutputName :: Text -> Either String OutputName
makeOutputName t = maybe (Right (OutputName t)) Left (validOutputName t)

makeOutputNameThrow :: HasCallStack => Text -> OutputName
makeOutputNameThrow t =
  case validOutputName t of
    Nothing -> OutputName t
    Just e -> error ("Invalid output name " ++ show t ++ ": " ++ e)