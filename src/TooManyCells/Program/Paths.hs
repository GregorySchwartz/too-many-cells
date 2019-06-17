{- TooManyCells.Program.Paths
Gregory W. Schwartz

Paths entry point into program.
-}

{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TupleSections     #-}

module TooManyCells.Program.Paths where

-- Remote
import BirchBeer.ColorMap
import BirchBeer.Interactive
import BirchBeer.Load
import BirchBeer.MainDiagram
import BirchBeer.Plot
import BirchBeer.Types
import BirchBeer.Utility
import Control.Monad (when, unless, join)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Bool (bool)
import Data.Colour.SRGB (sRGB24read)
import Data.Matrix.MatrixMarket (readMatrix, writeMatrix)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Monoid ((<>))
import Data.Tree (Tree (..))
import Language.R as R
import Language.R.QQ (r)
import Math.Clustering.Hierarchical.Spectral.Types (getClusterItemsDend, EigenGroup (..))
import Math.Clustering.Spectral.Sparse (b1ToB2, B1 (..), B2 (..))
import Options.Generic
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe, readEither)
import TextShow (showt)
import qualified "find-clumpiness" Types as Clump
import qualified Control.Lens as L
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Colour.Palette.BrewerSet as D
import qualified Data.Colour.Palette.Harmony as D
import qualified Data.Csv as CSV
import qualified Data.GraphViz as G
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Vector as V
import qualified Diagrams.Backend.Cairo as D
import qualified Diagrams.Prelude as D
import qualified H.Prelude as H
import qualified Plots as D
import qualified System.Directory as FP
import qualified System.FilePath as FP
import qualified System.ProgressBar as Progress

-- Local
import TooManyCells.Differential.Differential
import TooManyCells.Differential.Types
import TooManyCells.Diversity.Diversity
import TooManyCells.Diversity.Load
import TooManyCells.Diversity.Plot
import TooManyCells.Diversity.Types
import TooManyCells.File.Types
import TooManyCells.MakeTree.Clumpiness
import TooManyCells.MakeTree.Cluster
import TooManyCells.MakeTree.Load
import TooManyCells.MakeTree.Plot
import TooManyCells.MakeTree.Print
import TooManyCells.MakeTree.Types
import TooManyCells.Matrix.Load
import TooManyCells.Matrix.Preprocess
import TooManyCells.Matrix.Types
import TooManyCells.Matrix.Utility
import TooManyCells.Paths.Distance
import TooManyCells.Paths.Plot
import TooManyCells.Paths.Types
import Options

-- | Paths path.
pathsMain :: Options -> IO ()
pathsMain opts = do
    let labelsFile'   =
            maybe (error "\nNeed a label file.") LabelFile
                . unHelpful
                . labelsFile
                $ opts
        prior'        =
            maybe (error "\nNeed a prior path containing tree.") PriorPath
                . unHelpful
                . prior
                $ opts
        delimiter'    =
            Delimiter . fromMaybe ',' . unHelpful . delimiter $ opts
        bandwidth'    = Bandwidth . fromMaybe 1 . unHelpful . bandwidth $ opts
        direction'    = FlipFlag . unHelpful . flipDirection $ opts
        pathDistance' =
            maybe PathStep read . unHelpful . pathDistance $ opts
        output'       =
            OutputDirectory . fromMaybe "out" . unHelpful . output $ opts

    -- Where to place output files.
    FP.createDirectoryIfMissing True . unOutputDirectory $ output'

    -- Get the label map from a file.
    labelMap <- loadLabelData delimiter' labelsFile'

    -- Load previous results or calculate results if first run.
    tree <- fmap (either error id . A.eitherDecode)
          . B.readFile
          . (FP.</> "cluster_tree.json")
          . unPriorPath
          $ prior'

    let gr = treeToGraph tree
        pathDistances :: [(CellInfo, Double)]
        pathDistances = linearItemDistance direction' pathDistance' gr
        labeledPathDistances =
            labelItemDistance labelMap pathDistances

    H.withEmbeddedR defaultConfig $ H.runRegion $ do
        plotPathDistanceR
            (unOutputDirectory output' FP.</> "path_distances.pdf")
            bandwidth'
            labeledPathDistances
        return ()

    return ()
