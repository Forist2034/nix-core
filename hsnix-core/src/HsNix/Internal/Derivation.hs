{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module HsNix.Internal.Derivation (
  SrcInput,
  Derivation (..),
  DrvDep (..),
  DrvM (..),
  runDrvM,
) where

import Control.Monad.Writer.Strict
import Data.Bifunctor
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable
import qualified Data.Vector as V
import GHC.Generics (Generic)
import qualified HsNix.Derivation.Backend as B
import HsNix.Derivation.Types

type SrcInput = B.SrcInput

data Derivation = Derivation
  { drvInfo :: B.Derivation,
    drvInputDrv :: V.Vector Derivation,
    drvInputSrc :: V.Vector B.SrcInput
  }
  deriving (Show, Generic)

instance Eq Derivation where
  l == r = drvInfo l == drvInfo r

instance Hashable Derivation where
  hashWithSalt s = hashWithSalt s . drvInfo

data DrvDep = DrvDep
  { drvDrvDep :: HM.HashMap Derivation (HS.HashSet (Maybe OutputName)),
    drvSrcDep :: HS.HashSet SrcInput
  }

instance Semigroup DrvDep where
  l <> r =
    DrvDep
      { drvDrvDep = HM.unionWith HS.union (drvDrvDep l) (drvDrvDep r),
        drvSrcDep = HS.union (drvSrcDep l) (drvSrcDep r)
      }

instance Monoid DrvDep where
  mempty = DrvDep HM.empty HS.empty

newtype DrvM a = DrvM (Writer DrvDep a)
  deriving (Functor, Applicative)

runDrvM ::
  (a -> [B.SrcInput] -> [(B.Derivation, [Maybe OutputName])] -> B.Derivation) ->
  DrvM a ->
  Derivation
runDrvM f (DrvM dm) =
  let (da, dep) = runWriter dm
      srcs = HS.toList (drvSrcDep dep)
   in Derivation
        { drvInfo =
            f
              da
              srcs
              ( bimap drvInfo HS.toList
                  <$> HM.toList (drvDrvDep dep)
              ),
          drvInputDrv = V.fromList (HM.keys (drvDrvDep dep)),
          drvInputSrc = V.fromList srcs
        }