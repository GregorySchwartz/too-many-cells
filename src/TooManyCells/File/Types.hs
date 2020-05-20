{- TooManyCells.File.Types
Gregory W. Schwartz

Collects the file and argument types used in the program.
-}

{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuasiQuotes #-}

module TooManyCells.File.Types where

-- Remote
import Data.Colour.Palette.BrewerSet (Kolor)
import Data.Colour.SRGB (Colour (..), RGB (..), toSRGB)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Language.R as R
import Language.R.QQ (r)
import Math.Clustering.Hierarchical.Spectral.Sparse (ShowB)
import Math.Clustering.Hierarchical.Spectral.Types (ClusteringTree, ClusteringVertex)
import Math.Modularity.Types (Q (..))
import qualified Data.Aeson as A
import qualified Data.Clustering.Hierarchical as HC
import qualified Data.Graph.Inductive as G
import qualified Data.Sequence as Seq
import qualified Data.Sparse.Common as S
import qualified Numeric.LinearAlgebra as H

-- Local

-- Basic
newtype DendrogramFile  = DendrogramFile { unDendrogramFile :: FilePath }
newtype CellFile        = CellFile { unCellFile :: FilePath }
newtype FeatureFile        = FeatureFile { unFeatureFile :: FilePath }
newtype ProjectionFile  = ProjectionFile { unProjectionFile :: FilePath }
newtype CellWhitelistFile = CellWhitelistFile
    { unCellWhitelistFile :: FilePath
    }
newtype PriorPath   = PriorPath
    { unPriorPath :: FilePath
    } deriving (Eq,Ord,Read,Show)
newtype OutputDirectory  = OutputDirectory { unOutputDirectory :: FilePath }

-- Advanced
data MatrixFileFolder = MatrixFile FilePath | MatrixFolder FilePath
                        deriving (Read, Show)
data FragmentsFile = FragmentsFile FilePath
                     deriving (Read, Show)
data MatrixFileType = DecompressedMatrix MatrixFileFolder
                    | CompressedMatrix MatrixFileFolder
                    | CompressedFragments FragmentsFile
                    | BedGraph FilePath
                    | BigWig FilePath
                    deriving (Read, Show)
