{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module HsNix.Derivation.Backend (
  SrcInput,
  srcStorePath,
  addTextFile,
  addBinaryFile,
  addNar,
  Derivation,
  drvOutputPath,
  derivation,
  fetchUrl,
  BuildResult,
  buildDrv,
) where

import Crypto.Hash
import Data.Bifunctor
import Data.Binary (encode)
import qualified Data.ByteArray.Encoding as BAE
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Proxy
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.Encoding as LTE
import qualified Data.Vector as V
import qualified HsNix.Derivation.Types as DT
import HsNix.DrvDirect.Internal.Hash
import HsNix.DrvDirect.Internal.Types
import HsNix.DrvStr
import HsNix.Hash
import qualified HsNix.Internal.OutputName as ON
import qualified HsNix.Internal.StorePathName as SPN
import HsNix.Internal.System
import qualified HsNix.StorePath as SP
import qualified Nix.Derivation as ND
import Nix.Nar
import qualified System.Nix.Hash as NixHash
import qualified System.Nix.Internal.Base32 as NixBase32
import System.Nix.ReadonlyStore
import System.Nix.StorePath

storeRoot :: FilePath
storeRoot = "/nix/store"

mkSrcInput :: SPN.StorePathName -> SrcData -> StorePath -> SrcInput
mkSrcInput n v sp =
  SrcInput
    { srcName = SPN.storePathNameText n,
      srcData = v,
      srcSP = sp,
      srcSPText = storePathToText sp
    }

srcStorePath :: SrcInput -> SP.StorePath
srcStorePath = SP.StorePath . srcSPText

addTextFile :: SPN.StorePathName -> Text -> SrcInput
addTextFile n v =
  mkSrcInput
    n
    (SrcText v)
    ( computeStorePathForText
        storeRoot
        (coerce n)
        (TE.encodeUtf8 v)
        mempty
    )

hashNar :: SPN.StorePathName -> Nar -> StorePath
hashNar n v =
  makeFixedOutputPath
    storeRoot
    True
    (hashlazy @SHA256 (encode v))
    (coerce n)

addBinaryFile :: SPN.StorePathName -> ByteString -> SrcInput
addBinaryFile n v =
  mkSrcInput
    n
    (SrcBinary v)
    (hashNar n (Nar (Regular NonExecutable v)))

addNar :: SPN.StorePathName -> Nar -> SrcInput
addNar n v = mkSrcInput n (SrcNar v) (hashNar n v)

drvOutputPath :: Derivation -> Maybe ON.OutputName -> SP.StorePath
drvOutputPath d Nothing = SP.StorePath (drvDefaultSPText d)
drvOutputPath d (Just o) =
  SP.StorePath
    ( fromMaybe
        (error ("Derivation " ++ show d ++ " doesn't has output " ++ show o))
        (HM.lookup o (drvOutput d))
    )

toBase16Text :: Digest a -> Text
toBase16Text = TE.decodeASCII . BAE.convertToBase BAE.Base16

toEnv :: forall a. NamedHashAlgo a => DT.DerivationArg a -> M.Map Text Text
toEnv da =
  M.fromList
    ( concat
        [ [ ("name", SPN.storePathNameText (DT.drvName da)),
            ("builder", getDStr (DT.drvBuilder da)),
            ("system", systemName (DT.drvSystem da)),
            ("preferLocalBuild", boolToStr (DT.drvPreferLocalBuild da)),
            ("allowSubstitutes", boolToStr (DT.drvAllowSubstitutes da)),
            ("passAsFile", listToStr (fmap fst (DT.drvPassAsFile da)))
          ],
          fmap (second getDStr) (DT.drvEnv da),
          fmap (second getDStr) (DT.drvPassAsFile da),
          [ ( "exportReferenceGraph",
              listToStr
                ( concatMap
                    (\(n, sp) -> [n, SP.storePathText sp])
                    (DT.drvExportReferencesGraph da)
                )
            )
            | not (null (DT.drvExportReferencesGraph da))
          ],
          case DT.drvType da of
            DT.InputAddressed ons ->
              [("outputs", listToStr (coerce (NEL.toList ons)))]
            DT.FixedOutput
              { DT.drvImpureEnvs = env,
                DT.drvHashMode = m,
                DT.drvHash = Hash h
              } ->
                ("outputs", "out")
                  : ("outputHashAlgo", hashAlgoName @a Proxy)
                  : ("outputHashMode", hashModeToStr m)
                  : ("outputHash", toBase16Text h)
                  : [("impureEnvs", listToStr env) | not (null env)]
            DT.ContentAddressed m ons ->
              [ ("outputs", listToStr (coerce (NEL.toList ons))),
                ("outputHashAlgo", hashAlgoName @a Proxy),
                ("outputHashMode", hashModeToStr m)
              ],
          refsToStr "allowedReferences" (DT.drvAllowedReferences da),
          refsToStr "allowedRequisites" (DT.drvAllowedRequisites da),
          refsToStr "disallowedReferences" (DT.drvDisallowedReferences da),
          refsToStr "disallowedRequisites" (DT.drvDisallowedRequisites da)
        ]
    )
  where
    listToStr :: [Text] -> Text
    listToStr = T.intercalate " "

    boolToStr :: Bool -> Text
    boolToStr True = "1"
    boolToStr False = T.empty

    hashModeToStr :: HashMode -> Text
    hashModeToStr HashFlat = "flat"
    hashModeToStr HashRecursive = "recursive"

    refsToStr :: Text -> Maybe [DT.Reference] -> [(Text, Text)]
    refsToStr _ Nothing = []
    refsToStr n (Just ds) =
      [ ( n,
          listToStr
            ( fmap
                ( \case
                    DT.RefSelf o -> ON.outputNameText o
                    DT.RefDrv sp -> SP.storePathText sp
                )
                ds
            )
        )
      ]

addEnv ::
  [(Text, Text)] ->
  ND.Derivation Text Text ->
  ND.Derivation Text Text
addEnv e d = d {ND.env = M.union (M.fromList e) (ND.env d)}

toRefs ::
  [SrcInput] ->
  [(Derivation, [Maybe ON.OutputName])] ->
  StorePathSet
toRefs si di =
  HS.fromList
    (fmap srcSP si ++ fmap (drvPath . fst) di)

toAlgoId :: Text -> HashMode -> Text
toAlgoId algName mode =
  case mode of
    HashFlat -> algName
    HashRecursive -> "r:" <> algName

getDrvPath :: SPN.StorePathName -> Text -> StorePathSet -> StorePath
getDrvPath n drvTxt =
  computeStorePathForText
    storeRoot
    (StorePathName (SPN.storePathNameText n <> ".drv"))
    (TE.encodeUtf8 drvTxt)

builderToText :: LTB.Builder -> Text
builderToText = LT.toStrict . LTB.toLazyText

buildTxtDerivation :: ND.Derivation Text Text -> LTB.Builder
buildTxtDerivation = ND.buildDerivationWith str str
  where
    str :: Text -> LTB.Builder
    str = LTB.fromString . show

outputPathName :: SPN.StorePathName -> ON.OutputName -> Text
outputPathName (SPN.StorePathName dn) (ON.OutputName on) =
  if on == "out"
    then dn
    else dn <> "-" <> on

nixEncodeDigest :: Digest a -> Text
nixEncodeDigest = NixHash.encodeDigestWith NixHash.NixBase32

downstreamPlaceholder ::
  SPN.StorePathName ->
  StorePath ->
  ON.OutputName ->
  Text
downstreamPlaceholder n StorePath {storePathHash = StorePathHashPart h} o =
  "/"
    <> nixEncodeDigest
      ( hashWith
          SHA256
          ( mconcat
              [ "nix-upstream-output:",
                TE.encodeUtf8 (NixBase32.encode h),
                ":",
                TE.encodeUtf8 (outputPathName n o)
              ]
          )
      )

mkPreDrv ::
  NamedHashAlgo a =>
  DT.DerivationArg a ->
  [(Text, ND.DerivationOutput Text Text)] ->
  [(Derivation, [Maybe ON.OutputName])] ->
  [SrcInput] ->
  ND.Derivation Text Text
mkPreDrv da os di si =
  ND.Derivation
    { ND.outputs = M.fromList os,
      ND.inputDrvs = toInputDrv di,
      ND.inputSrcs = S.fromList (fmap srcSPText si),
      ND.platform = systemName (DT.drvSystem da),
      ND.builder = coerce (DT.drvBuilder da),
      ND.args = V.fromList (coerce (DT.drvArgs da)),
      ND.env = toEnv da
    }
  where
    toInputDrv =
      M.fromList
        . fmap
          ( \(d, dos) ->
              ( drvPathText d,
                S.fromList
                  ( fmap
                      (ON.outputNameText . fromMaybe (drvDefaultOut d))
                      dos
                  )
              )
          )

hashDerivation ::
  ND.Derivation Text Text ->
  [(Derivation, [Maybe ON.OutputName])] ->
  Digest SHA256
hashDerivation nd inDrv =
  hashlazy
    ( LTE.encodeUtf8
        ( LTB.toLazyText
            ( buildTxtDerivation
                (nd {ND.inputDrvs = toHashDrv inDrv})
            )
        )
    )
  where
    toHashDrv =
      M.fromList
        . fmap
          ( \(d, dos) ->
              ( drvHash d,
                S.fromList
                  ( fmap
                      (ON.outputNameText . fromMaybe (drvDefaultOut d))
                      dos
                  )
              )
          )

derivation ::
  forall a.
  NamedHashAlgo a =>
  DT.DerivationArg a ->
  [SrcInput] ->
  [(Derivation, [Maybe ON.OutputName])] ->
  Derivation
derivation
  da@DT.DerivationArg
    { DT.drvName = dn,
      DT.drvType = DT.InputAddressed ons@(defOut :| otherOut)
    }
  si
  di =
    let os = NEL.toList ons
        hashTyp =
          if any (\(d, _) -> drvHashType d == DeferredHash) di
            then DeferredHash
            else RegularHash
        ref = toRefs si di
        preDrv =
          mkPreDrv
            da
            ( let emptyOut =
                    ND.DerivationOutput
                      { ND.path = T.empty,
                        ND.hash = T.empty,
                        ND.hashAlgo = T.empty
                      }
               in fmap
                    (\(ON.OutputName o) -> (o, emptyOut))
                    os
            )
            di
            si
        maskedDrv =
          addEnv
            (fmap (\o -> (ON.outputNameText o, T.empty)) os)
            preDrv
     in case hashTyp of
          RegularHash ->
            let maskedHsh = hashDerivation maskedDrv di
                defOutSP = makeOutputPath dn defOut maskedHsh
                outSP =
                  (defOut, defOutSP)
                    : fmap
                      (\o -> (o, makeOutputPath dn o maskedHsh))
                      otherOut
                drv =
                  preDrv
                    { ND.outputs =
                        M.fromList
                          ( fmap
                              ( \(ON.OutputName o, sp) ->
                                  ( o,
                                    ND.DerivationOutput
                                      { ND.path = sp,
                                        ND.hashAlgo = T.empty,
                                        ND.hash = T.empty
                                      }
                                  )
                              )
                              outSP
                          ),
                      ND.env = M.union (M.fromList (coerce outSP)) (ND.env preDrv)
                    }
                drvTxt = builderToText (buildTxtDerivation drv)
                dPath = getDrvPath dn drvTxt ref
             in Derivation
                  { drvName = SPN.storePathNameText dn,
                    drvText = drvTxt,
                    drvPath = dPath,
                    drvPathText = storePathToText dPath,
                    drvHashType = hashTyp,
                    drvHash = toBase16Text (hashDerivation drv di),
                    drvRefs = ref,
                    drvDefaultOut = defOut,
                    drvDefaultSPText = defOutSP,
                    drvOutput = HM.fromList (coerce outSP)
                  }
          DeferredHash ->
            let drvTxt = builderToText (buildTxtDerivation maskedDrv)
                dPath = getDrvPath dn drvTxt ref
                defOutSP = downstreamPlaceholder dn dPath defOut
             in Derivation
                  { drvName = SPN.storePathNameText dn,
                    drvText = drvTxt,
                    drvPath = dPath,
                    drvPathText = storePathToText dPath,
                    drvHashType = hashTyp,
                    drvHash = toBase16Text (hashDerivation maskedDrv di),
                    drvRefs = ref,
                    drvDefaultOut = defOut,
                    drvDefaultSPText = defOutSP,
                    drvOutput =
                      HM.fromList
                        ( (defOut, defOutSP)
                            : fmap
                              (\o -> (o, downstreamPlaceholder dn dPath o))
                              otherOut
                        )
                  }
    where
      makeOutputPath n o hsh =
        storePathToText
          ( makeStorePath
              storeRoot
              ("output:" <> TE.encodeUtf8 (ON.outputNameText o))
              hsh
              (StorePathName (outputPathName n o))
          )
derivation da@DT.DerivationArg {DT.drvType = DT.FixedOutput _ mode (Hash h)} si di =
  let hashAlg = toAlgoId (hashAlgoName @a Proxy) mode
      storePathTxt =
        storePathToText
          ( makeFixedOutputPath
              storeRoot
              ( case mode of
                  HashFlat -> False
                  HashRecursive -> True
              )
              (coerce h :: Digest (AlgoWrapper a))
              (coerce (DT.drvName da))
          )
      drvTxt =
        builderToText
          ( buildTxtDerivation
              ( addEnv
                  [("out", storePathTxt)]
                  ( mkPreDrv
                      da
                      [ ( "out",
                          ND.DerivationOutput
                            { ND.path = storePathTxt,
                              ND.hashAlgo = hashAlg,
                              ND.hash = T.pack (show h)
                            }
                        )
                      ]
                      di
                      si
                  )
              )
          )
      ref = toRefs si di
      dPath = getDrvPath (DT.drvName da) drvTxt ref
   in Derivation
        { drvName = SPN.storePathNameText (DT.drvName da),
          drvText = drvTxt,
          drvPath = dPath,
          drvPathText = storePathToText dPath,
          drvHash =
            toBase16Text
              ( hashWith
                  SHA256
                  ( BS.intercalate
                      ":"
                      [ "fixed:out",
                        TE.encodeUtf8 hashAlg,
                        BAE.convertToBase BAE.Base16 h,
                        TE.encodeUtf8 storePathTxt
                      ]
                  )
              ),
          drvHashType = RegularHash,
          drvRefs = ref,
          drvDefaultOut = ON.OutputName "out",
          drvDefaultSPText = storePathTxt,
          drvOutput = HM.singleton (ON.OutputName "out") storePathTxt
        }
derivation
  da@DT.DerivationArg
    { DT.drvName = dn,
      DT.drvType = DT.ContentAddressed mode ons@(defOut :| otherOut)
    }
  si
  di =
    let os = NEL.toList ons
        drv =
          addEnv
            (fmap (\(ON.OutputName o) -> (o, hashPlaceHolder o)) os)
            ( mkPreDrv
                da
                ( let drvOutSpec =
                        ND.DerivationOutput
                          { ND.path = T.empty,
                            ND.hashAlgo = toAlgoId (hashAlgoName @a Proxy) mode,
                            ND.hash = T.empty
                          }
                   in fmap
                        (\o -> (ON.outputNameText o, drvOutSpec))
                        os
                )
                di
                si
            )
        drvTxt = builderToText (buildTxtDerivation drv)
        ref = toRefs si di
        dPath = getDrvPath dn drvTxt ref
        defOutSP = downstreamPlaceholder dn dPath defOut
     in Derivation
          { drvName = SPN.storePathNameText dn,
            drvText = drvTxt,
            drvPath = dPath,
            drvPathText = storePathToText dPath,
            drvHash = toBase16Text (hashDerivation drv di),
            drvHashType = DeferredHash,
            drvRefs = ref,
            drvDefaultOut = defOut,
            drvDefaultSPText = defOutSP,
            drvOutput =
              HM.fromList
                ( (defOut, defOutSP)
                    : fmap
                      ( \o ->
                          ( o,
                            downstreamPlaceholder dn dPath o
                          )
                      )
                      otherOut
                )
          }
    where
      hashPlaceHolder o =
        "/"
          <> nixEncodeDigest
            (hashWith SHA256 ("nix-output:" <> TE.encodeUtf8 o))

fetchUrl ::
  NamedHashAlgo a =>
  DT.FetchUrlArg a ->
  [SrcInput] ->
  [(Derivation, [Maybe ON.OutputName])] ->
  Derivation
fetchUrl fa =
  derivation
    ( (DT.defaultDrvArg (DT.faName fa) "builtin:fetchurl" (System "builtin"))
        { DT.drvType =
            DT.FixedOutput
              { DT.drvImpureEnvs =
                  [ "http_proxy",
                    "https_proxy",
                    "ftp_proxy",
                    "all_proxy",
                    "no_proxy"
                  ],
                DT.drvHashMode =
                  if DT.faUnpack fa || DT.faIsExecutable fa
                    then HashRecursive
                    else HashFlat,
                DT.drvHash = DT.faOutputHash fa
              },
          DT.drvEnv =
            [ ("url", fromText (DT.faUrl fa)),
              ("urls", fromText (DT.faUrl fa)),
              ("executable", if DT.faIsExecutable fa then "1" else mempty),
              ("unpack", if DT.faUnpack fa then "1" else mempty)
            ],
          DT.drvPreferLocalBuild = True
        }
    )

buildDrv :: [SrcInput] -> [Derivation] -> BuildResult
buildDrv src drv =
  BuildResult
    { brSource = src,
      brDrv = drv
    }