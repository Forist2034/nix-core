{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module HsNix.Derivation.Drv.Internal.Derivation
  ( ToText (..),
    DrvHash,
    toDerivation,
  )
where

import Crypto.Hash hiding (hash)
import Data.Bifunctor
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Proxy
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.Encoding as LTE
import Data.Tuple (swap)
import qualified Data.Vector as V
import HsNix.Derivation.Drv.Internal.Hash
import HsNix.Hash
import qualified HsNix.Internal.Derivation as ND
import HsNix.Internal.System
import Nix.Derivation
import System.Nix.Hash
import System.Nix.ReadonlyStore
import System.Nix.StorePath

class ToText a where
  toText :: a -> Text

makeOutputPath :: NamedAlgo a => Text -> Digest a -> Text -> StorePath
makeOutputPath outName h drvName =
  makeStorePath
    "/nix/store"
    ("output:" <> TE.encodeUtf8 outName)
    h
    (StorePathName outputPathName)
  where
    outputPathName =
      if outName == "out"
        then drvName
        else drvName <> "-" <> outName

toEnv :: forall a t. (ToText t, NamedHashAlgo a) => ND.DerivationArg t a -> M.Map Text Text
toEnv da =
  M.fromList
    ( concat
        [ [ ("name", ND.drvName da),
            ("builder", toText (ND.drvBuilder da)),
            ("system", systemName (ND.drvSystem da)),
            ( "preferLocalBuild",
              if ND.drvPreferLocalBuild da then "1" else T.empty
            ),
            ( "allowSubstitutes",
              if ND.drvAllowSubstitutes da then "1" else T.empty
            )
          ],
          fmap (second toText) (ND.drvEnv da),
          fmap (second toText) (ND.drvPassAsFile da),
          [ ("passAsFile", listToStr (fmap fst (ND.drvPassAsFile da)))
            | not (null (ND.drvPassAsFile da))
          ],
          case ND.drvOutputs da of
            ND.RegularOutput os ->
              ("outputs", listToStr (NEL.toList os))
                : fmap (,T.empty) (NEL.toList os)
            ND.FixedOutput h ->
              [ ("outputs", "out"),
                ("outputHashAlgo", hashAlgoName (Proxy :: Proxy a)),
                ( "outputHashMode",
                  case ND.drvHashMode da of
                    HashFlat -> "flat"
                    HashRecursive -> "recursive"
                ),
                ("outputHash", T.pack (show h)),
                ("out", T.empty)
              ],
          depToStr "allowedReferences" (ND.drvAllowedReferences da),
          depToStr "allowedRequisites" (ND.drvAllowedRequisites da),
          depToStr "disallowedReferences" (ND.drvDisallowedReferences da),
          depToStr "disallowedRequisites" (ND.drvDisallowedRequisites da)
        ]
    )
  where
    listToStr = mconcat . L.intersperse " "

    depToStr n v = case v of
      Just ds -> [(n, listToStr (fmap toText ds))]
      Nothing -> []

toPlatform :: ND.DerivationArg txt a -> Text
toPlatform = systemName . ND.drvSystem

toBuilder :: ToText t => ND.DerivationArg t a -> Text
toBuilder = toText . ND.drvBuilder

toArgs :: ToText t => ND.DerivationArg t a -> V.Vector Text
toArgs = V.fromList . fmap toText . ND.drvArgs

newtype DrvHash = DrvHash {drvHashBase16 :: Text}
  deriving (Show, Eq)

toDerivation ::
  forall a t.
  (ToText t, NamedHashAlgo a) =>
  ND.DerivationArg t a ->
  -- | (drv StorePath, (OuputName, OutputHash in Base16))
  M.Map StorePath (M.Map Text DrvHash) ->
  S.Set StorePath ->
  (Derivation StorePath Text, HM.HashMap Text (StorePath, DrvHash))
toDerivation da@ND.DerivationArg {ND.drvOutputs = ND.RegularOutput os} inDrv inSrc =
  let outPath =
        let maskedHash = hashDrv' maskedDrv
         in HM.fromList
              ( fmap
                  (\o -> (o, makeOutputPath o maskedHash (ND.drvName da)))
                  out
              )
      finalOut =
        M.fromList
          ( fmap
              (\o -> (o, toIADrvOut (fromJust (HM.lookup o outPath))))
              out
          )
      finalEnv = M.union (M.fromList (second storePathToText <$> HM.toList outPath)) preEnv
   in ( Derivation
          { outputs = finalOut,
            inputDrvs = M.map M.keysSet inDrv,
            inputSrcs = inSrc,
            platform = platform maskedDrv,
            builder = builder maskedDrv,
            args = args maskedDrv,
            env = finalEnv
          },
        let drvHash =
              DrvHash
                ( encodeDigestWith
                    Base16
                    ( hashDrv'
                        maskedDrv -- fill in storePath to get real digest
                          { outputs =
                              fmap
                                (toIADrvOut . storePathToText . path)
                                finalOut,
                            env = finalEnv
                          }
                    )
                )
         in fmap (,drvHash) outPath
      )
  where
    toIADrvOut p = DerivationOutput {path = p, hashAlgo = T.empty, hash = T.empty}
    hashDrv' =
      hashWith SHA256
        . LBS.toStrict
        . LTE.encodeUtf8
        . LTB.toLazyText
        . buildDerivationWith (LTB.fromString . show) (LTB.fromString . show)

    out = NEL.toList os
    preEnv = toEnv da
    maskedDrv =
      Derivation
        { outputs =
            let emptyDout = toIADrvOut T.empty
             in M.fromList (fmap (,emptyDout) out),
          inputDrvs =
            M.fromListWith
              S.union
              ( concatMap
                  (fmap (bimap drvHashBase16 S.singleton . swap) . M.assocs)
                  (M.elems inDrv)
              ),
          inputSrcs = S.map storePathToText inSrc,
          platform = toPlatform da,
          builder = toBuilder da,
          args = toArgs da,
          env = preEnv
        }
toDerivation da@ND.DerivationArg {ND.drvOutputs = ND.FixedOutput (Hash h)} inDrv inSrc =
  let hashAlg =
        let algName = hashAlgoName (Proxy :: Proxy a)
         in case ND.drvHashMode da of
              HashFlat -> algName
              HashRecursive -> "r:" <> algName
      storePath =
        makeFixedOutputPath
          "/nix/store"
          ( case ND.drvHashMode da of
              HashFlat -> False
              HashRecursive -> True
          )
          (coerce h :: Digest (AlgoWrapper a))
          (StorePathName (ND.drvName da))
      drvHash =
        ( DrvHash
            . encodeDigestWith Base16
            . hashWith SHA256
            . BS.intercalate ":"
        )
          ( "fixed:out"
              : fmap
                TE.encodeUtf8
                [ hashAlg,
                  encodeDigestWith Base16 h,
                  storePathToText storePath
                ]
          )
   in ( Derivation
          { outputs =
              M.singleton
                "out"
                DerivationOutput
                  { path = storePath,
                    hashAlgo = hashAlg,
                    hash = T.pack (show h)
                  },
            inputDrvs = M.map M.keysSet inDrv,
            inputSrcs = inSrc,
            platform = toPlatform da,
            builder = toBuilder da,
            args = toArgs da,
            env = M.insert "out" (storePathToText storePath) (toEnv da)
          },
        HM.singleton "out" (storePath, drvHash)
      )