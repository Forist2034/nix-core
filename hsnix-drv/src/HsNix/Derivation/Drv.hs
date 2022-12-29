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
import HsNix.Dependent.Topo
import HsNix.Dependent.Vertex
import qualified HsNix.Derivation as ND
import HsNix.Derivation.Drv.Internal.Derivation
import HsNix.Derivation.Drv.Internal.Nar
import HsNix.Hash
import HsNix.Internal.System
import qualified System.Nix.Derivation as DrvBuild
import System.Nix.ReadonlyStore
import System.Nix.StorePath

nixStore :: FilePath
nixStore = "/nix/store"

data BuildDrv = DB
  { dbPath :: BuildPath,
    dbDep :: S.Set BuildDrv
  }
  deriving (Show, Eq, Ord)

instance Hashable BuildDrv where
  hashWithSalt s = hashWithSalt s . buildPath . dbPath

instance DepVertex BuildDrv where
  type Id BuildDrv = StorePath
  getId = buildPath . dbPath
  type EdgeSet BuildDrv = S.Set
  getDep = dbDep

data Deps = Deps
  { buildDrv :: S.Set BuildDrv,
    drvDep :: M.Map StorePath (M.Map Text DrvHash),
    srcDep :: S.Set StorePath
  }

instance Semigroup Deps where
  l <> r =
    Deps
      { buildDrv = S.union (buildDrv l) (buildDrv r),
        drvDep = M.unionWith M.union (drvDep l) (drvDep r),
        srcDep = S.union (srcDep l) (srcDep r)
      }

instance Monoid Deps where
  mempty = Deps {buildDrv = S.empty, drvDep = M.empty, srcDep = S.empty}

newtype DirectDrv a = DI (DepVert Deps a)
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
    { dDrv :: BuildDrv,
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
          ( addEdge
              Deps
                { buildDrv = S.singleton (dDrv d),
                  drvDep = M.singleton (buildPath (dbPath (dDrv d))) (M.singleton out dh),
                  srcDep = S.empty
                }
              >> pure (SP sp)
          )
  derivation (DI w) =
    let (d, deps) = runDepVert w
        (drv, out) = toDerivation d (drvDep deps) (srcDep deps)
        spName = ND.drvName d <> ".drv"
        drvText = (LT.toStrict . LTB.toLazyText . DrvBuild.buildDerivation) drv
        ref = HS.fromList (S.toList (srcDep deps) ++ M.keys (drvDep deps))
        drvPath =
          computeStorePathForText
            nixStore
            (StorePathName spName)
            (TE.encodeUtf8 drvText)
            ref
     in D
          { dDrv =
              DB
                { dbPath =
                    BuildPath
                      { buildPath = drvPath,
                        buildType =
                          StoreDrv
                            { storeName = spName,
                              storeDrv = drv,
                              storeDrvText = drvText,
                              storeRef = ref
                            }
                      },
                  dbDep = buildDrv deps
                },
            dDefOut = case ND.drvOutputs d of
              ND.RegularOutput os -> NEL.head os
              ND.FixedOutput _ -> "out",
            dOut = out
          }
  build D {dDrv = d} = BResult (fmap dbPath (topoSortDep (addVertex d)))

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
            nixStore
            (StorePathName n)
            (TE.encodeUtf8 c)
            HS.empty
     in DI
          ( addEdge
              Deps
                { buildDrv =
                    S.singleton
                      DB
                        { dbPath =
                            BuildPath
                              { buildPath = p,
                                buildType = StoreText n c
                              },
                          dbDep = S.empty
                        },
                  drvDep = M.empty,
                  srcDep = S.singleton p
                }
              >> pure (SP p)
          )

instance BuiltinAddDir DirectDrv where
  addDirectory n d =
    let p = makeNarPath nixStore n d
     in DI
          ( addEdge
              Deps
                { buildDrv =
                    S.singleton
                      DB
                        { dbPath =
                            BuildPath
                              { buildPath = p,
                                buildType = StoreDir n d
                              },
                          dbDep = S.empty
                        },
                  drvDep = M.empty,
                  srcDep = S.singleton p
                }
              >> pure (SP p)
          )