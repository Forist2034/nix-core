{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module HsNix.Derivation.Drv
  ( DirectDrv,
    ND.BuildResult (..),
  )
where

import Control.Monad.State
import Control.Monad.Writer
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
import System.Nix.StorePath

newtype BuildRes = BR {getBR :: [BuildPath] -> [BuildPath]}

instance Semigroup BuildRes where
  BR l <> BR r = BR (l . r)

instance Monoid BuildRes where
  mempty = BR id

type ResultM = WriterT BuildRes (State (HS.HashSet StorePath))

data Deps = Deps
  { depM :: ResultM (),
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

newtype DirectDrv a = DI (Writer Deps a)
  deriving newtype (Functor, Applicative, Monad)

instance ND.HasStrBuilder DirectDrv where
  newtype DrvStrBuilder DirectDrv = DSB LTB.Builder
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

instance ND.AsDrvStr (ND.DrvStrBuilder DirectDrv) (ND.DrvStr DirectDrv) where
  toDrvStr (DSB b) = DS (LT.toStrict (LTB.toLazyText b))

instance ND.AsDrvStr Text (ND.DrvStr DirectDrv) where
  toDrvStr = DS

instance ToText (ND.DrvStr DirectDrv) where
  toText (DS d) = d

instance ND.AsDrvStr (ND.StorePath DirectDrv) (ND.DrvStr DirectDrv) where
  toDrvStr (SP s) = DS (storePathToText s)

instance Eq (ND.Derivation DirectDrv) where
  D {dDrv = l} == D {dDrv = r} = l == r

instance Show (ND.Derivation DirectDrv) where
  show D {dDrv = l, dDefOut = d, dOut = o} =
    concat
      [ "D{ dDrv=",
        show l,
        "dDefOut=",
        show d,
        ",dOut=",
        show o
      ]

instance Hashable (ND.Derivation DirectDrv) where
  hashWithSalt s D {dDrv = l} = hashWithSalt s l

instance ND.MonadDeriv DirectDrv where
  newtype DrvStr DirectDrv = DS Text
    deriving (Eq, Show)
    deriving newtype (IsString, Semigroup, Monoid, Hashable)
  newtype StorePath DirectDrv = SP StorePath
    deriving (Eq)
    deriving newtype (Show, Hashable)
  data Derivation DirectDrv = D
    { dDrv :: StorePath,
      dM :: ResultM (),
      dDefOut :: Text,
      dOut :: HM.HashMap Text (StorePath, DrvHash)
    }
  newtype BuildResult DirectDrv = BResult [BuildPath]
    deriving (Show)
  storePathOf d p =
    let out = fromMaybe (dDefOut d) p
        (sp, dh) = case HM.lookup out (dOut d) of
          Just v -> v
          Nothing -> error ("Derivation " ++ show d ++ "doesn't has output " ++ T.unpack out)
     in DI
          ( tell
              Deps
                { depM = dM d,
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
        drvPath =
          computeStorePathForText
            "/nix/store"
            (StorePathName spName)
            (TE.encodeUtf8 drvText)
            ref
     in D
          { dDrv = drvPath,
            dM =
              gets (HS.member drvPath) >>= \e ->
                unless e $ do
                  depM deps
                  tell
                    ( BR
                        ( BuildPath
                            { buildPath = drvPath,
                              buildType =
                                StoreDrv
                                  { storeName = spName,
                                    storeDrv = drv,
                                    storeDrvText = drvText,
                                    storeRef = ref
                                  }
                            }
                            :
                        )
                    )
                  modify (HS.insert drvPath),
            dDefOut = case ND.drvOutputs d of
              ND.RegularOutput os -> NEL.head os
              ND.FixedOutput _ -> "out",
            dOut = out
          }
  build D {dM = m} = BResult (getBR (evalState (execWriterT m) HS.empty) [])

instance BuiltinFetchUrl DirectDrv where
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

instance BuiltinAddText DirectDrv where
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
                { depM =
                    gets (HS.member p) >>= \e ->
                      unless e $ do
                        tell
                          ( BR
                              ( BuildPath
                                  { buildPath = p,
                                    buildType = StoreText n c
                                  }
                                  :
                              )
                          )
                        modify (HS.insert p),
                  drvDep = M.empty,
                  srcDep = S.singleton p
                }
              >> pure (SP p)
          )

instance BuiltinAddDrvStr DirectDrv where
  addDrvStr n (DS t) = addTextFile n t