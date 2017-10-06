{- Types
Gregory W. Schwartz

Collects the types used in the program
-}

{-# LANGUAGE QuasiQuotes #-}
    
module Types where

-- Remote
import Data.Text (Text)
import Data.Vector (Vector)
import Language.R as R
import Language.R.QQ (r)

-- Local

-- Basic
newtype Gene       = Gene { unGene :: (Text, Text) }
newtype Cell       = Cell { unCell :: Text }
newtype Cluster    = Cluster { unCluster :: Double }
newtype MatrixFile = MatrixFile { unMatrixFile :: FilePath }
newtype GeneFile   = GeneFile { unGeneFile :: FilePath }
newtype CellFile   = CellFile { unCellFile :: FilePath }
newtype Rows       = Rows { unRows :: [Double] }
newtype Cols       = Cols { unCols :: [Double] }
newtype Vals       = Vals { unVals :: [Double] }
newtype RMat s     = RMat { unRMat :: R.SomeSEXP s }
newtype RMatScaled s    = RMatScaled { unRMatScaled :: R.SomeSEXP s }
newtype RMatImportant s = RMatImportant { unRMatImportant :: R.SomeSEXP s }

-- Advanced
data SingleCells = SingleCells { matrix :: [[Double]]
                               , rowNames :: Vector Gene
                               , colNames :: Vector Cell
                               }
