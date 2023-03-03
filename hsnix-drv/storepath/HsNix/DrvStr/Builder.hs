module HsNix.DrvStr.Builder (
  LTB.Builder,
  LTB.singleton,
  fromDrvStr,
  fromStorePath,
  LTB.fromText,
  LTB.decimal,
  LTB.hexadecimal,
  toDrvStr,
) where

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.Builder.Int as LTB
import HsNix.DrvStr
import HsNix.StorePath

fromDrvStr :: DrvStr -> LTB.Builder
fromDrvStr (DStr s) = LTB.fromText s

fromStorePath :: StorePath -> LTB.Builder
fromStorePath = LTB.fromText . storePathText

toDrvStr :: LTB.Builder -> DrvStr
toDrvStr b = DStr (LT.toStrict (LTB.toLazyText b))