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
import Control.Monad (when)
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
import Plot

-- | Command line arguments
data Options = Options { matrixFile  :: Maybe String
                               <?> "([matrix.mtx] | FILE) The input file containing the matrix output of cellranger or, if genes-file and cells-file are not specified, a csv containing gene row names and cell column names."
                       , genesFile :: Maybe String
                               <?> "([genes.tsv] | FILE) The input file containing gene information from cellranger."
                       , cellsFile :: Maybe String
                               <?> "([barcodes.tsv] | FILE) The input file containing gene information from cellranger."
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
        delimiter'   = Delimiter . fromMaybe ',' . unHelpful . delimiter $ opts
        outputPlot'  = unHelpful . outputPlot $ opts
        outputDendrogram' = unHelpful . outputDendrogram $ opts
        matrixCsv    =
            any
                isNothing
                [unHelpful . genesFile $ opts, unHelpful . cellsFile $ opts]


    unFilteredSc <-
        if matrixCsv
            then loadMatrixData delimiter' matrixFile'
            else loadCellrangerData matrixFile' genesFile' cellsFile'

    sc <- filterMat unFilteredSc

    processedMat <- scaleMat (matrix sc) >>= pcaMat

    let processedSc = sc { matrix = processedMat }

    R.withEmbeddedR R.defaultConfig $ R.runRegion $ do
        -- mat         <- scToRMat processedSc
        -- clusterRes  <- hdbscan mat
        -- clusterList <- clustersToClusterList sc clusterRes

        io . hPutStrLn stderr $ "Clustering."
        let (clusterList, dend) = hClust processedSc

        case outputPlot' of
            Nothing  -> return ()
            (Just x) -> io
                      . D.renderCairo (x <> ".pdf") (D.mkWidth 1000)
                      . D.renderAxis
                      . plotClusters
                      $ clusterList
            --(Just x) -> plotClusters x mat $ clusterRes
            
        case outputDendrogram' of
            Nothing  -> return ()
            (Just x) -> io
                      . B.writeFile x
                      . B.pack
                      . show
                      $ dend

        -- Header
        io . B.putStrLn $ "cell,cluster"

        -- Body
        io
            . B.putStrLn
            . CSV.encode
            . fmap (\((Cell !x, _), Cluster !z) -> (x, z))
            $ clusterList
