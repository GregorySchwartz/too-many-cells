{- sc-cluster
Gregory W. Schwartz

Clusters single cell data.
-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

-- Remote
import Control.Monad (when)
import Data.Maybe (fromMaybe, isJust)
import H.Prelude as H
import Language.R as R
import Language.R.QQ (r)
import Options.Generic
import qualified Control.Lens as L
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Csv as CSV

-- Local
import Types
import Utility
import Load
import Cluster
import Plot

-- | Command line arguments
data Options = Options { inputMatrix  :: Maybe String
                               <?> "([matrix.mtx] | FILE) The input file containing the matrix output of cellranger."
                       , inputGenes :: Maybe String
                               <?> "([genes.tsv] | FILE) The input file containing gene information from cellranger."
                       , inputCells :: Maybe String
                               <?> "([barcodes.tsv] | FILE) The input file containing gene information from cellranger."
                       , outputPlot :: Maybe String
                               <?> "([Nothing] | STRING) The prefix for the output plots. No plots generated if not specified."
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

    let inputMatrix' =
            MatrixFile . fromMaybe "matrix.mtx" . unHelpful . inputMatrix $ opts
        inputGenes'  =
            GeneFile . fromMaybe "genes.tsv" . unHelpful . inputGenes $ opts
        inputCells'  =
            CellFile . fromMaybe "barcodes.tsv" . unHelpful . inputCells $ opts
        outputPlot'  = unHelpful . outputPlot $ opts

    sc <- loadData inputMatrix' inputGenes' inputCells'

    R.withEmbeddedR R.defaultConfig $ R.runRegion $ do
        mat         <- scToRMat sc >>= scaleRMat >>= removeCorrelated
        clusterRes  <- hdbscan mat
        clusterList <- clustersToClusterList sc clusterRes

        case outputPlot' of
            Nothing  -> return ()
            (Just x) -> plotClusters x mat $ clusterRes

        -- Header
        H.io . B.putStrLn $ "cell,cluster"

        -- Body
        H.io
            . B.putStrLn
            . CSV.encode
            . fmap (L.over L._2 unCluster . L.over L._1 unCell)
            $ clusterList
