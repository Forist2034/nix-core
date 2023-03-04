{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module HsNix.Internal.StorePathName (
  StorePathName (..),
  validStorePathName,
  makeStorePathName,
  makeStorePathNameThrow,
) where

import Data.Char
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Stack (HasCallStack)

newtype StorePathName = StorePathName {storePathNameText :: Text}
  deriving (Show, Eq, Ord, Hashable)

validStorePathName :: Text -> Maybe String
validStorePathName t
  | T.null t = Just "Null string"
  | T.length t > 211 = Just "Too long"
  | T.head t == '.' = Just "Leading dot"
  | Just c <- T.find (not . validChar) t = Just ("Invalid char " ++ show c)
  | otherwise = Nothing
  where
    validChar c =
      isAsciiLower c
        || isAsciiUpper c
        || isDigit c
        || c `elem` ("+-._?=" :: String)

makeStorePathName :: Text -> Either String StorePathName
makeStorePathName t = maybe (Right (StorePathName t)) Left (validStorePathName t)

makeStorePathNameThrow :: HasCallStack => Text -> StorePathName
makeStorePathNameThrow t =
  case validStorePathName t of
    Nothing -> StorePathName t
    Just e -> error ("Invalid StoreName: " ++ e)