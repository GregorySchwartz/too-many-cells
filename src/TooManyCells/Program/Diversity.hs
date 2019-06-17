{- TooManyCells.Program.Diversity
Gregory W. Schwartz

Diversity entry point into program.
-}

{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TupleSections     #-}

module TooManyCells.Program.Diversity where

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

-- | Diversity path.
diversityMain :: Options -> IO ()
diversityMain opts = do
    let priors'         =
            fmap PriorPath . unHelpful . priors $ opts
        delimiter'     =
            Delimiter . fromMaybe ',' . unHelpful . delimiter $ opts
        labelsFile'       =
            fmap LabelFile . unHelpful . labelsFile $ opts
        output'         =
            OutputDirectory . fromMaybe "out" . unHelpful . output $ opts
        order'       = Order . fromMaybe 1 . unHelpful . order $ opts
        start'       = Start . fromMaybe 0 . unHelpful . start $ opts
        interval'    = Interval . fromMaybe 1 . unHelpful . interval $ opts
        endMay'      = fmap End . unHelpful . end $ opts

    -- Where to place output files.
    FP.createDirectoryIfMissing True . unOutputDirectory $ output'

    labelMap <- sequence . fmap (loadLabelData delimiter') $ labelsFile'

    pops <- fmap ( either
                    (\err -> error $ err <> "\nEncountered error in population loading, aborting process.")
                    id
                 . sequence
                 )
          . mapM (\x -> (fmap . fmap) (Label . T.pack . unPriorPath $ x,)
                      . loadPopulation labelMap
                      $ x
                 )
          $ priors'

    popDiversities <-
        mapM
            (\ (l, pop) -> getPopulationDiversity
                                l
                                order'
                                start'
                                interval'
                                endMay'
                                pop
            )
            pops

    -- Output quantifications.
    B.writeFile (unOutputDirectory output' FP.</> "diversity.csv")
      . CSV.encodeDefaultOrderedByName
      $ popDiversities

    -- D.renderCairo (unOutputDirectory output' FP.</> "diversity.pdf") D.absolute
    --     . plotDiversity
    --     $ popDiversities

    -- D.renderCairo (unOutputDirectory output' FP.</> "chao1.pdf") D.absolute
    --     . plotChao1
    --     $ popDiversities

    -- D.renderCairo (unOutputDirectory output' FP.</> "rarefaction.pdf") D.absolute
    --     . plotRarefaction
    --     $ popDiversities

    -- Output plots.
    let colors = D.colorRamp (length pops) . D.brewerSet D.Set1 $ 9

    H.withEmbeddedR defaultConfig $ H.runRegion $ do
        let divFile = unOutputDirectory output' FP.</> "diversity.pdf"
        divPlot <- plotDiversityR colors popDiversities
        [r| suppressMessages(ggsave(divPlot_hs, file = divFile_hs)) |]

        -- let chao1File = unOutputDirectory output' FP.</> "chao_r.pdf"
        -- chao1Plot <- plotChao1R colors popDiversities
        -- [r| suppressMessages(ggsave(chao1Plot_hs, file = chao1File_hs)) |]

        let rarefactionFile = unOutputDirectory output' FP.</> "rarefaction.pdf"
        rarefactionPlot <- plotRarefactionR colors popDiversities
        [r| suppressMessages(ggsave(rarefactionPlot_hs, file = rarefactionFile_hs)) |]

        return ()

    return ()
