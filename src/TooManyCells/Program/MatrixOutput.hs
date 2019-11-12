{- TooManyCells.Program.MatrixOutput
Gregory W. Schwartz

MatrixOutput entrypoint into the program.
-}

{-# LANGUAGE OverloadedStrings #-}

module TooManyCells.Program.MatrixOutput where

-- Remote
import Options.Generic
import qualified System.Directory as FP

-- Local
import TooManyCells.Program.Options
import TooManyCells.Program.LoadMatrix
import TooManyCells.File.Types
import TooManyCells.Matrix.Utility
import TooManyCells.Matrix.Types

matrixOutputMain :: Options -> IO ()
matrixOutputMain opts = do
    let matOutput' = getMatrixOutputType . unHelpful . matOutput $ opts

    -- Load matrix once.
    scRes <- loadAllSSM opts
    let processedSc = fmap fst scRes

    -- Write matrix
    writeMatrixLike (MatrixTranspose False) matOutput' . extractSc $ processedSc
