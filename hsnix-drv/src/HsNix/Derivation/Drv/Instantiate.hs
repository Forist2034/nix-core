{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module HsNix.Derivation.Drv.Instantiate (DrvInst, runInst) where

import Control.Monad.Writer
import Data.Functor
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import HsNix.Builtin.AddFile
import HsNix.Builtin.FetchUrl
import qualified HsNix.Derivation as ND
import HsNix.Derivation.Drv.Internal.Derivation
import HsNix.Hash
import HsNix.Internal.System
import qualified System.Nix.Derivation as DrvBuild
import System.Nix.ReadonlyStore
import System.Nix.Store.Remote
import System.Nix.StorePath

data Deps = Deps
  { depM :: MonadStore (),
    drvDep :: M.Map StorePath (M.Map Text DrvHash),
    srcDep :: S.Set StorePath
  }

instance Semigroup Deps where
  l <> r =
    Deps
      { depM = depM l >> depM r,
        drvDep = M.unionWith M.union (drvDep l) (drvDep r),
        srcDep = S.union (srcDep l) (srcDep r)
      }

instance Monoid Deps where
  mempty = Deps {depM = pure (), drvDep = M.empty, srcDep = S.empty}

newtype DrvInst a = DI (Writer Deps a)
  deriving newtype (Functor, Applicative, Monad)

instance ND.HasStrBuilder DrvInst where
  newtype DrvStrBuilder DrvInst = DSB LTB.Builder
    deriving newtype (IsString, Semigroup, Monoid)
  fromDrvStr (DS t) = DSB (LTB.fromText t)
  quote f (DS dt) = quoteStr dt
    where
      quoteStr s
        | T.null s = []
        | otherwise =
            let (h, t) = T.break f s
             in case T.uncons t of
                  Just (esc, ts) -> ND.QStr (DS h) : ND.QEscape esc : quoteStr ts
                  Nothing -> [ND.QStr (DS h)]

instance ND.AsDrvStr (ND.DrvStrBuilder DrvInst) (ND.DrvStr DrvInst) where
  toDrvStr (DSB b) = DS (LT.toStrict (LTB.toLazyText b))

instance ND.AsDrvStr Text (ND.DrvStr DrvInst) where
  toDrvStr = DS

instance ToText (ND.DrvStr DrvInst) where
  toText (DS d) = d

instance ND.AsDrvStr (ND.StorePath DrvInst) (ND.DrvStr DrvInst) where
  toDrvStr (SP s) = DS (storePathToText s)

instance Eq (ND.Derivation DrvInst) where
  D {dDrv = l} == D {dDrv = r} = l == r

instance Show (ND.Derivation DrvInst) where
  show D {dDrv = l, dDefOut = d, dOut = o} =
    concat
      [ "D{ dDrv=",
        show l,
        "dDefOut=",
        show d,
        ",dOut=",
        show o
      ]

instance Hashable (ND.Derivation DrvInst) where
  hashWithSalt s D {dDrv = l} = hashWithSalt s l

instance ND.MonadDeriv DrvInst where
  newtype DrvStr DrvInst = DS Text
    deriving (Eq, Show)
    deriving newtype (IsString, Semigroup, Monoid, Hashable)
  newtype StorePath DrvInst = SP StorePath
    deriving (Eq)
    deriving newtype (Show, Hashable)
  data Derivation DrvInst = D
    { dDrv :: StorePath,
      dM :: MonadStore StorePath,
      dDefOut :: Text,
      dOut :: HM.HashMap Text (StorePath, DrvHash)
    }
  newtype BuildResult DrvInst = BResult (MonadStore StorePath)
  storePathOf d p =
    let out = fromMaybe (dDefOut d) p
        (sp, dh) = case HM.lookup out (dOut d) of
          Just v -> v
          Nothing -> error ("Derivation " ++ show d ++ "doesn't has output " ++ T.unpack out)
     in DI
          ( tell
              Deps
                { depM = void (dM d),
                  drvDep = M.singleton (dDrv d) (M.singleton out dh),
                  srcDep = S.empty
                }
              >> pure (SP sp)
          )
  derivation (DI w) =
    let (d, deps) = runWriter w
        (drv, out) = toDerivation d (drvDep deps) (srcDep deps)
        spName = ND.drvName d <> ".drv"
        drvText = (LT.toStrict . LTB.toLazyText . DrvBuild.buildDerivation) drv
        ref = HS.fromList (S.toList (srcDep deps) ++ M.keys (drvDep deps))
     in D
          { dDrv =
              computeStorePathForText
                "/nix/store"
                (StorePathName spName)
                (TE.encodeUtf8 drvText)
                ref,
            dM = addTextToStore spName drvText ref False,
            dDefOut = case ND.drvOutputs d of
              ND.RegularOutput os -> NEL.head os
              ND.FixedOutput _ -> "out",
            dOut = out
          }
  build D {dM = m} = BResult m

runInst :: ND.BuildResult DrvInst -> IO (Either String FilePath)
runInst (BResult b) =
  runStore b
    <&> fmap storePathToFilePath . fst

instance BuiltinFetchUrl DrvInst where
  fetchUrl fa =
    ND.derivation
      ( pure
          ( (ND.defaultDrvArg (name fa) "builtin:fetchurl" (System "builtin"))
              { ND.drvEnv =
                  [ ("url", ND.toDrvStr (url fa)),
                    ("executable", fromBool (isExecutable fa)),
                    ("unpack", fromBool (unpack fa)),
                    ("urls", ND.toDrvStr (url fa))
                  ],
                ND.drvPreferLocalBuild = True,
                ND.drvOutputs = ND.FixedOutput (outputHash fa),
                ND.drvHashMode =
                  if isExecutable fa || unpack fa
                    then HashRecursive
                    else HashFlat
              }
          )
      )
    where
      fromBool True = "1"
      fromBool False = ""

instance BuiltinAddText DrvInst where
  addTextFile n c =
    let p =
          computeStorePathForText
            "/nix/store"
            (StorePathName n)
            (TE.encodeUtf8 c)
            HS.empty
     in DI
          ( tell
              Deps
                { depM = void (addTextToStore n c HS.empty False),
                  drvDep = M.empty,
                  srcDep = S.singleton p
                }
              >> pure (SP p)
          )

instance BuiltinAddDrvStr DrvInst where
  addDrvStr n (DS t) = addTextFile n t