{- TooManyCells.Program.Utility
Gregory W. Schwartz

Utility functions for the command line program.
-}

module TooManyCells.Program.Utility where

-- Remote

-- Local
import Options

-- | Notify user of limitations and error out for incompatabilities. Empty for
-- now.
limitationWarningsErrors :: Options -> IO ()
limitationWarningsErrors opts = do
    return ()

-- | Read or return an error.
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
