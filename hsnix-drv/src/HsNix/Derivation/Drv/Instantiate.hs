module HsNix.Derivation.Drv.Instantiate (runInst) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Functor
import qualified Data.HashSet as HS
import HsNix.Derivation.Drv
import HsNix.Derivation.Drv.Internal.Derivation
import System.Nix.Store.Remote
import System.Nix.StorePath (storePathToFilePath)

runInst :: Bool -> BuildResult DirectDrv -> IO (Either String FilePath)
runInst verbose (BResult r) =
  runStore
    ( traverse
        ( \BuildPath {buildPath = p, buildType = t} ->
            case t of
              StoreDrv {storeName = n, storeDrvText = txt, storeRef = ref} -> do
                when (verbose) $
                  liftIO (putStrLn ("instantiate: add derivation " ++ show p))
                addTextToStore n txt ref False
              StoreText n c -> do
                when (verbose) $
                  liftIO (putStrLn ("instantiate: add text file " ++ show p))
                addTextToStore n c HS.empty False
        )
        r
    )
    <&> fmap (storePathToFilePath . last) . fst