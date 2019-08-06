{- TooManyCells.Program.Utility
Gregory W. Schwartz

Utility functions for the command line program.
-}

module TooManyCells.Program.Utility where

-- Remote
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Options.Generic

-- Local
import TooManyCells.Matrix.Types
import TooManyCells.Program.Options

-- | Read or return an error.
readOrErr :: (Read a) => String -> String -> a
readOrErr err = fromMaybe (error err) . readMaybe

-- | Normalization defaults.
getNormalization :: Options -> NormType
getNormalization opts@(MakeTree{}) =
  maybe
    TfIdfNorm
    (readOrErr "Cannot read --normalization.")
    . unHelpful
    . normalization
    $ opts
getNormalization opts@(Interactive{}) =
  maybe
    TfIdfNorm
    (readOrErr "Cannot read --normalization.")
    . unHelpful
    . normalization
    $ opts
getNormalization opts@(Differential{}) =
  maybe
    NoneNorm
    (readOrErr "Cannot read --normalization.")
    . unHelpful
    . normalization
    $ opts
getNormalization _ = TfIdfNorm
