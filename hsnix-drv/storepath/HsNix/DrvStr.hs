module HsNix.DrvStr (DrvStr (..), fromText, Quoted (..), quote) where

import Data.Hashable (Hashable)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T

newtype DrvStr = DStr {getDStr :: Text}
  deriving (Show, Eq, Ord, Semigroup, Monoid, IsString, Hashable)

fromText :: Text -> DrvStr
fromText = DStr

data Quoted = QChar Char | QStr DrvStr

quote :: (Char -> Bool) -> DrvStr -> [Quoted]
quote f (DStr ds) = go ds
  where
    go s
      | T.null s = []
      | otherwise =
          let (h, t) = T.break f s
           in case T.uncons t of
                Just (esc, ts) -> QStr (DStr h) : QChar esc : go ts
                Nothing -> [QStr (DStr h)]