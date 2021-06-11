{- TooManyCells.Program.MatrixOutput
Gregory W. Schwartz

MatrixOutput entrypoint into the program.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module TooManyCells.Program.MatrixOutput where

-- Remote
import qualified System.Directory as FP

-- Local
import TooManyCells.Program.Options
import TooManyCells.Program.LoadMatrix
import TooManyCells.File.Types
import TooManyCells.Matrix.Utility
import TooManyCells.Matrix.Types

matrixOutputMain :: Subcommand -> IO ()
matrixOutputMain sub@(MatrixOutputCommand opts) = do
    let matOutput' = getMatrixOutputType . matOutput $ opts

    -- Load matrix once.
    scRes <- loadAllSSM sub $ (loadMatrixOptions :: MatrixOutput -> LoadMatrixOptions) opts
    let processedSc = fmap fst scRes

    -- Write matrix
    writeMatrixLike (MatrixTranspose False) matOutput' . extractSc $ processedSc
matrixOutputMain _ = error "Wrong path in matrix-output, contact Gregory Schwartz for this error."
