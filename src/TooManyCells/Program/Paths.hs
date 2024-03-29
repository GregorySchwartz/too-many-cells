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
{-# LANGUAGE DuplicateRecordFields #-}

module TooManyCells.Program.Paths where

-- Remote
import BirchBeer.ColorMap (getLabelColorMap)
import BirchBeer.Load
import BirchBeer.Types
import BirchBeer.Utility
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Language.R as R
import Math.Clustering.Hierarchical.Spectral.Types (getClusterItemsDend, EigenGroup (..))
import Text.Read (readMaybe)
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
pathsMain :: Subcommand -> IO ()
pathsMain (PathsCommand opts) = do
    let readOrErr err = fromMaybe (error err) . readMaybe
        labelsFile'   =
            maybe (error "\nNeed a label file.") LabelFile
                . (labelsFile :: Paths -> Maybe String)
                $ opts
        prior'        =
            maybe (error "\nNeed a prior path containing tree.") PriorPath
                . (prior :: Paths -> Maybe String)
                $ opts
        delimiter'    = Delimiter . (delimiter :: Paths -> Char) $ opts
        bandwidth'    = Bandwidth . bandwidth $ opts
        direction'    = FlipFlag . flipDirection $ opts
        shallow'      = ShallowFlag . shallowStart $ opts
        palette'      = pathsPalette opts
        pathDistance' = pathDistance opts
        output'       =
            OutputDirectory . (output :: Paths -> String) $ opts

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
        pathDistances = linearItemDistance shallow' direction' pathDistance' gr
        labeledPathDistances =
            labelItemDistance labelMap pathDistances

    H.withEmbeddedR defaultConfig $ H.runRegion $ do
        let labelColorMap = getLabelColorMap palette' labelMap
        plotPathDistanceR
            (unOutputDirectory output' FP.</> "path_distances.pdf")
            labelColorMap
            bandwidth'
            labeledPathDistances
        return ()

    return ()
pathsMain _ = error "Wrong path in paths, contact Gregory Schwartz for this error."
