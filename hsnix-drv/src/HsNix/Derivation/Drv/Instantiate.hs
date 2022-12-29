{-# LANGUAGE TypeApplications #-}

module HsNix.Derivation.Drv.Instantiate (runInst) where

import Control.Monad
import Control.Monad.IO.Class
import Crypto.Hash (SHA256)
import Data.Bifunctor
import Data.Foldable
import Data.Functor
import qualified Data.HashSet as HS
import HsNix.Builtin.AddFile
import HsNix.Derivation.Drv
import HsNix.Derivation.Drv.Internal.Derivation
import System.Directory
import System.Nix.Store.Remote
import System.Nix.StorePath
import System.Posix.Temp

runInst :: Bool -> BuildResult DirectDrv -> IO (Either String FilePath)
runInst verbose (BResult r) =
  runStore
    ( traverse_
        ( \BuildPath {buildPath = p, buildName = n@(StorePathName nt), buildType = t} ->
            case t of
              StoreDrv {storeDrvText = txt, storeRef = ref} -> do
                when verbose $
                  liftIO (putStrLn ("instantiate: add derivation " ++ show p))
                addTextToStore nt txt ref False
              StoreText c -> do
                when verbose $
                  liftIO (putStrLn ("instantiate: add text file " ++ show p))
                addTextToStore nt c HS.empty False
              StoreDir d -> do
                when verbose $ do
                  liftIO (putStrLn ("instantiate: add directory " ++ show p))
                dir <- liftIO $ mkdtemp "/tmp/source"
                liftIO (writeDirTree dir nt d)
                addToStore @SHA256 n dir True (const True) False
                  <* liftIO (removeDirectoryRecursive dir)
        )
        r
    )
    <&> second (const (storePathToFilePath (buildPath (last r))))
      . fst