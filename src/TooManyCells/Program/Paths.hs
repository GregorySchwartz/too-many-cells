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
import BirchBeer.Load
import BirchBeer.Types
import BirchBeer.Utility
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Language.R as R
import Math.Clustering.Hierarchical.Spectral.Types (getClusterItemsDend, EigenGroup (..))
import Options.Generic
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as B
import qualified H.Prelude as H
import qualified System.Directory as FP
import qualified System.FilePath as FP

-- Local
import TooManyCells.File.Types
import TooManyCells.MakeTree.Plot
import TooManyCells.Matrix.Types
import TooManyCells.Paths.Distance
import TooManyCells.Paths.Plot
import TooManyCells.Paths.Types
import TooManyCells.Program.Options
import TooManyCells.File.Types
import TooManyCells.Matrix.Types

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
