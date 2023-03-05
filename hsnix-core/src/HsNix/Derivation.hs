module HsNix.Derivation (
  SrcInput,
  B.addTextFile,
  B.addBinaryFile,
  B.addNar,
  srcStorePath,
  srcStorePathStr,
  module EDT,
  DerivationArg,
  Derivation,
  derivation,
  fetchUrl,
  drvStorePathOf,
  drvStorePathStrOf,
  DrvM,
  buildDerivations,
  buildDerivation,
) where

import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Bifunctor
import Data.Foldable
import Data.Functor
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified HsNix.Derivation.Backend as B
import HsNix.Derivation.DerivationArgs as EDT hiding (DerivationArg)
import qualified HsNix.Derivation.DerivationArgs as DT
import HsNix.Derivation.FetchUrlArg
import HsNix.DrvStr
import HsNix.Hash
import HsNix.Internal.Derivation
import HsNix.OutputName
import HsNix.StorePath

srcStorePath :: SrcInput -> DrvM StorePath
srcStorePath s =
  DrvM
    ( let sp = B.srcStorePath s
       in tell (mempty {drvSrcDep = HS.singleton s}) $> sp
    )

srcStorePathStr :: SrcInput -> DrvM DrvStr
srcStorePathStr = fmap storePathStr . srcStorePath

fetchUrl :: NamedHashAlgo a => DrvM (FetchUrlArg a) -> Derivation
fetchUrl = runDrvM B.fetchUrl

type DerivationArg = DT.DerivationArg DrvStr StorePath

derivation :: NamedHashAlgo a => DrvM (DerivationArg a) -> Derivation
derivation = runDrvM B.derivation

drvStorePathOf :: Derivation -> Maybe OutputName -> DrvM StorePath
drvStorePathOf d o =
  DrvM
    ( let sp = B.drvOutputPath (drvInfo d) o
       in tell (mempty {drvDrvDep = HM.singleton d (HS.singleton o)}) $> sp
    )

drvStorePathStrOf :: Derivation -> Maybe OutputName -> DrvM DrvStr
drvStorePathStrOf d o = storePathStr <$> drvStorePathOf d o

type BdM =
  State
    ( ([B.Derivation], HS.HashSet B.SrcInput),
      HS.HashSet B.Derivation
    )

buildDerivations :: [Derivation] -> B.BuildResult
buildDerivations ds =
  let (d, s) =
        fst
          ( execState
              (traverse_ go ds)
              (([], HS.empty), HS.empty)
          )
   in B.buildDrv (HS.toList s) (reverse d)
  where
    go :: Derivation -> BdM ()
    go d@Derivation {drvInfo = di} = do
      ex <- gets (HS.member di . snd)
      unless ex $ do
        modify (second (HS.insert di))
        traverse_ go (drvInputDrv d)
        modify
          ( first
              ( bimap
                  (di :)
                  (\s -> foldr' HS.insert s (drvInputSrc d))
              )
          )

buildDerivation :: Derivation -> B.BuildResult
buildDerivation d = buildDerivations [d]