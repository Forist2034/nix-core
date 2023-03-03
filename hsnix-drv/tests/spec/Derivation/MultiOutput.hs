module Derivation.MultiOutput (
  outputList,
) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import HsNix.Internal.OutputName

outputList :: NonEmpty OutputName
outputList =
  OutputName "lib"
    :| [ OutputName "out",
         OutputName "bin",
         OutputName "dev"
       ]
