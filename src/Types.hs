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
import qualified Numeric.LinearAlgebra as H

-- Local

-- Basic
newtype Cell            = Cell { unCell :: Text } deriving (Read, Show)
newtype CellFile        = CellFile { unCellFile :: FilePath }
newtype Cluster         = Cluster { unCluster :: Double }
newtype Cols            = Cols { unCols :: [Double] }
newtype Delimiter       = Delimiter { unDelimiter :: Char }
newtype Gene            = Gene { unGene :: Text } deriving (Read, Show)
newtype GeneFile        = GeneFile { unGeneFile :: FilePath }
newtype MatrixFile = MatrixFile
    { unMatrixFile :: FilePath
    } deriving (Read,Show)
newtype RMat s          = RMat { unRMat :: R.SomeSEXP s }
newtype RMatObsRow s    = RMatObsRow { unRMatObsRow :: R.SomeSEXP s }
newtype RMatFeatRow s   = RMatFeatRow { unRMatFeatRow :: R.SomeSEXP s }
newtype RMatObsRowImportant s = RMatObsRowImportant
    { unRMatObsRowImportant :: R.SomeSEXP s
    }
newtype RMatScaled s    = RMatScaled { unRMatScaled :: R.SomeSEXP s }
newtype Rows            = Rows { unRows :: [Double] }
newtype Vals            = Vals { unVals :: [Double] }

-- Advanced
data SCMatrix
    = MatObsRow { unMatObsRow :: H.Matrix H.R }
    | MatObsRowImportant { unMatObsRow :: H.Matrix H.R }
    deriving (Read,Show)
data SingleCells = SingleCells { matrix :: SCMatrix
                               , rowNames :: Vector Gene
                               , colNames :: Vector Cell
                               }
                   deriving (Read, Show)
