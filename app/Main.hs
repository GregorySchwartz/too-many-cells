{- sc-cluster
Gregory W. Schwartz

Clusters single cell data.
-}

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

-- Remote
import Control.Monad (when, unless)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Monoid ((<>))
import H.Prelude (io)
import Language.R as R
import Language.R.QQ (r)
import Options.Generic
import System.IO (hPutStrLn, stderr)
import qualified Control.Lens as L
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Csv as CSV
import qualified Diagrams.Backend.Cairo as D
import qualified Diagrams.Prelude as D
import qualified Plots as D

-- Local
import Types
import Utility
import Load
import Preprocess
import Cluster
import Clumpiness
import Plot

-- | Command line arguments
data Options = Options { matrixFile  :: Maybe String
                               <?> "([matrix.mtx] | FILE) The input file containing the matrix output of cellranger or, if genes-file and cells-file are not specified, a csv containing gene row names and cell column names."
                       , genesFile :: Maybe String
                               <?> "([genes.tsv] | FILE) The input file containing gene information from cellranger."
                       , cellsFile :: Maybe String
                               <?> "([barcodes.tsv] | FILE) The input file containing gene information from cellranger."
                       , labelsFile :: Maybe String
                               <?> "([Nothing] | FILE) The input file containing the label for each cell, with \"cell,label\" header."
                       , dendrogramFile :: Maybe String
                               <?> "([Nothing] | STRING) The input file for a precalculated dendrogram. If specified, skip all other files (except labels-file) and continue from the dendrogram analysis."
                       , delimiter :: Maybe Char
                               <?> "([,] | CHAR) The delimiter for the csv file if using a normal csv rather than cellranger output."
                       , outputPlot :: Maybe String
                               <?> "([Nothing] | STRING) The prefix for the output plots. No plots generated if not specified."
                       , outputDendrogram :: Maybe String
                               <?> "([Nothing] | STRING) The output file for the dendrogram. No file generated if not specified."
                       }
               deriving (Generic)

modifiers :: Modifiers
modifiers = lispCaseModifiers { shortNameModifier = firstLetter }

instance ParseRecord Options where
    parseRecord = parseRecordWithModifiers modifiers

main :: IO ()
main = do
    opts <- getRecord "sc-cluster, Gregory W. Schwartz.\
                      \ Clusters single cell data."

    let matrixFile' =
            MatrixFile . fromMaybe "matrix.mtx" . unHelpful . matrixFile $ opts
        genesFile'  =
            GeneFile . fromMaybe "genes.tsv" . unHelpful . genesFile $ opts
        cellsFile'  =
            CellFile . fromMaybe "barcodes.tsv" . unHelpful . cellsFile $ opts
        labelsFile'  =
            fmap LabelFile . unHelpful . labelsFile $ opts
        dendrogramFile'  =
            fmap DendrogramFile . unHelpful . dendrogramFile $ opts
        delimiter'   = Delimiter . fromMaybe ',' . unHelpful . delimiter $ opts
        outputPlot'  = unHelpful . outputPlot $ opts
        outputDendrogram' = unHelpful . outputDendrogram $ opts
        matrixCsv    =
            any
                isNothing
                [unHelpful . genesFile $ opts, unHelpful . cellsFile $ opts]
        unFilteredSc   =
            if matrixCsv
                then loadMatrixData delimiter' matrixFile'
                else loadCellrangerData matrixFile' genesFile' cellsFile'
        sc             = unFilteredSc >>= filterMat
        processedMat   = fmap matrix sc >>= scaleMat -- >>= pcaMat
        processedSc    = do
            pMat <- processedMat
            fmap (\x -> x { matrix = pMat }) sc

    labelColorMaps <- fmap (fmap (\x -> (x, getColorMap x)))
                    . sequence
                    . fmap (loadLabelData delimiter')
                    $ labelsFile'

    R.withEmbeddedR R.defaultConfig $ R.runRegion $ do
        -- For r clustering.
        -- mat         <- scToRMat processedSc
        -- clusterRes  <- hdbscan mat
        -- clusterList <- clustersToClusterList sc clusterRes

        -- For agglomerative clustering.
        --let clusterResults = fmap hClust processedSc
        -- For divisive clustering.
        let clusterResults = fmap hSpecClust processedSc

        dend <- case dendrogramFile' of
                    Nothing  -> io . fmap clusterDend $ clusterResults
                    (Just x) -> io
                              . fmap (read . B.unpack)
                              . B.readFile
                              . unDendrogramFile
                              $ x

        -- | Plot only if needed and ignore non-tree analyses if dendrogram is
        -- supplied.
        case outputPlot' of
            Nothing  -> return ()
            (Just x) -> do
                -- Plot dendrogram.
                io
                    . D.renderCairo (x <> "_dendrogram.pdf") (D.mkWidth 1000)
                    . plotDendrogram labelColorMaps
                    $ dend

                -- Find clumpiness.
                case labelColorMaps of
                    Nothing -> io $ hPutStrLn stderr "Clumpiness requires labels for cells, skipping..."
                    (Just lcm) -> io
                                . B.writeFile (x <> "_clumpiness.csv")
                                . dendToClumpCsv (fst lcm)
                                $ dend

                io $ unless (isJust dendrogramFile') $ do
                    -- Plot clustering.
                    clusterResults
                        >>= D.renderCairo (x <> ".pdf") (D.mkWidth 1000)
                          . D.renderAxis
                          . plotClusters
                          . clusterList

            --(Just x) -> plotClusters x mat $ clusterRes

        case outputDendrogram' of
            Nothing  -> return ()
            (Just x) -> io
                      . B.writeFile x
                      . B.pack
                      . show
                      $ dend

        -- Ignore for now.
        io $ unless (isJust dendrogramFile') $ do

            -- Header
            B.putStrLn $ "cell,cluster"

            -- Body
            clusterResults
                >>= B.putStrLn
                  . CSV.encode
                  . fmap (\((Cell !x, _), Cluster !z) -> (x, z))
                  . clusterList
