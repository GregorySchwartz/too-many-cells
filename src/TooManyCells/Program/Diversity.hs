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
{-# LANGUAGE DuplicateRecordFields #-}

module TooManyCells.Program.Diversity where

-- Remote
import BirchBeer.Load
import BirchBeer.Types
import Control.Monad.Trans (liftIO)
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Language.R as R
import Math.Clustering.Hierarchical.Spectral.Types (getClusterItemsDend, EigenGroup (..))
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Colour.Palette.BrewerSet as D
import qualified Data.Colour.Palette.Harmony as D
import qualified Data.Csv as CSV
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as T
import qualified Diagrams.Backend.Cairo as D
import qualified Diagrams.Prelude as D
import qualified H.Prelude as H
import qualified Plots as D
import qualified System.Directory as FP
import qualified System.FilePath as FP

-- Local
import TooManyCells.Program.Options
import TooManyCells.Diversity.Diversity
import TooManyCells.Diversity.Load
import TooManyCells.Diversity.Plot
import TooManyCells.Diversity.Types
import TooManyCells.File.Types
import qualified TooManyCells.Program.Options as Opt

-- | Diversity path.
diversityMain :: Subcommand -> IO ()
diversityMain (DiversityCommand opts) = do
    let priors'         =
            fmap PriorPath . priors $ opts
        delimiter'     =
            Delimiter . (delimiter :: Opt.Diversity -> Char) $ opts
        labelsFile'       =
            fmap LabelFile . (labelsFile :: Opt.Diversity -> Maybe String) $ opts
        output'         =
            OutputDirectory . (output :: Opt.Diversity -> String) $ opts
        order'       = Order . (order :: Opt.Diversity -> Double) $ opts
        start'       = Start . start $ opts
        interval'    = Interval . interval $ opts
        endMay'      = fmap End . end $ opts

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
        [H.r| suppressMessages(ggsave(divPlot_hs, file = divFile_hs)) |]

        -- let chao1File = unOutputDirectory output' FP.</> "chao_r.pdf"
        -- chao1Plot <- plotChao1R colors popDiversities
        -- [H.r| suppressMessages(ggsave(chao1Plot_hs, file = chao1File_hs)) |]

        let rarefactionFile = unOutputDirectory output' FP.</> "rarefaction.pdf"
        rarefactionPlot <- plotRarefactionR colors popDiversities
        [H.r| suppressMessages(ggsave(rarefactionPlot_hs, file = rarefactionFile_hs)) |]

        return ()

    return ()
diversityMain _ = error "Wrong path in diversity, contact Gregory Schwartz for this error."
