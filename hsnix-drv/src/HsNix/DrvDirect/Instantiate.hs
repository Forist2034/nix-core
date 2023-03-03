{-# LANGUAGE TypeApplications #-}

module HsNix.DrvDirect.Instantiate (runStore, instantiate) where

import Control.Monad.IO.Class
import Crypto.Hash
import Data.Binary
import qualified Data.ByteString as BS
import Data.Foldable
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import HsNix.DrvDirect.Internal.Types
import Nix.Nar
import System.Nix.Nar
import System.Nix.Store.Remote
import System.Nix.StorePath

instantiate :: BuildResult -> MonadStore ()
instantiate br =
  do
    traverse_
      ( \s@SrcInput {srcName = name, srcSPText = sp} ->
          case srcData s of
            SrcText t -> do
              putLog name "text" sp
              addTextToStore name t mempty False
            SrcBinary b -> do
              putLog name "binary" sp
              addToStore
                @SHA256
                (StorePathName name)
                (dumpString (encodeNarStrict (Nar (Regular NonExecutable b))))
                False
                False
            SrcNar n -> do
              putLog name "nar" sp
              addToStore @SHA256
                (StorePathName name)
                (dumpString (encodeNarStrict n))
                True
                False
      )
      (brSource br)
    traverse_
      ( \d -> do
          putLog (drvName d) "deriv" (drvPathText d)
          addTextToStore (drvName d) (drvText d) (drvRefs d) False
      )
      (brDrv br)
  where
    width =
      max
        (maximum (T.length . srcName <$> brSource br))
        (maximum (T.length . drvName <$> brDrv br))
        + 3 -- two brackets and one space
        + maximum (T.length <$> ["text", "binary", "nar", "deriv"])
    putLog n t sp =
      liftIO
        ( TIO.putStrLn
            ( "instantiate: "
                <> T.justifyLeft width ' ' (n <> "[" <> t <> "]")
                <> " ("
                <> sp
                <> ")"
            )
        )

    encodeNarStrict :: Nar -> BS.ByteString
    encodeNarStrict = BS.toStrict . encode