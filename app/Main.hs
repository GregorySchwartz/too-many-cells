{- sc-cluster
Gregory W. Schwartz

Clusters single cell data.
-}

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TupleSections     #-}

module Main where

-- Remote
import Control.Monad (when, unless, join)
import Data.Bool (bool)
import Data.Matrix.MatrixMarket (readMatrix, writeMatrix)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Monoid ((<>))
import Language.R as R
import Language.R.QQ (r)
import Math.Clustering.Hierarchical.Spectral.Sparse (B (..))
import Math.Clustering.Hierarchical.Spectral.Types (getClusterItemsDend)
import Options.Generic
import System.Directory (createDirectoryIfMissing, copyFile)
import System.IO (hPutStrLn, stderr)
import TextShow (showt)
import qualified Control.Lens as L
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Csv as CSV
import qualified Data.GraphViz as G
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Vector as V
import qualified Diagrams.Backend.Cairo as D
import qualified Diagrams.Prelude as D
import qualified H.Prelude as H
import qualified Plots as D
import qualified System.FilePath as FP

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
                               <?> "([genes.tsv] | FILE) The input file containing gene information from cellranger. Matches row order in the matrix file."
                       , cellsFile :: Maybe String
                               <?> "([barcodes.tsv] | FILE) The input file containing barcode information from cellranger. Matches column order in the matrix file."
                       , projectionFile :: Maybe String
                               <?> "([Nothing] | FILE) The input file containing positions of each cell for plotting. Format is \"barcode,x,y\" and matches column order in the matrix file. Useful for 10x where a TNSE projection is generated in \"projection.csv\". If not supplied, the resulting plot will use the first two features."
                       , labelsFile :: Maybe String
                               <?> "([Nothing] | FILE) The input file containing the label for each cell, with \"cell,label\" header."
                       , delimiter :: Maybe Char
                               <?> "([,] | CHAR) The delimiter for the csv file if using a normal csv rather than cellranger output."
                       , preNormalization :: Bool
                               <?> "Do not pre-normalize matrix before cluster normalization."
                       , minSize   :: Maybe Int
                               <?> "([1] | INT) The minimum size of a cluster. Defaults to 1."
                       , drawLeaf :: Maybe String
                               <?> "([DrawText] | DrawCell) How to draw leaves in the dendrogram. DrawText is the number of cells in that leaf if --labels-file is provided, otherwise the leaves are labeled by majority cell label in that leaf. DrawCell is the collection of cells represented by circles, consisting of: DrawCell DrawLabel, where each cell is colored by its label, and DrawCell (DrawExpression GENE), where each cell is colored by the expression of GENE (corresponding to a gene name in the input matrix, not yet implemented)."
                       , drawDendrogram :: Bool
                               <?> "Draw a dendrogram instead of a graph."
                       , drawNodeNumber :: Bool
                               <?> "Draw the node numbers on top of each vertex in the graph."
                       , prior :: Maybe String
                               <?> "([Nothing] | STRING) The input folder containing the output from a previous run. If specified, skips clustering by using the previous clustering files."
                       , output :: Maybe String
                               <?> "([out] | STRING) The folder containing output."
                       }
               deriving (Generic)

modifiers :: Modifiers
modifiers = lispCaseModifiers { shortNameModifier = short }
  where
    short "minSize"           = Just 'M'
    short "prior"             = Just 'P'
    short "preNormalization"  = Just 'n'
    short "drawLeaf"          = Just 'L'
    short "drawDendrogram"    = Just 'D'
    short "drawNodeNumber"    = Just 'N'
    short x                   = firstLetter x

instance ParseRecord Options where
    parseRecord = parseRecordWithModifiers modifiers

main :: IO ()
main = do
    opts <- getRecord "sc-cluster, Gregory W. Schwartz.\
                      \ Clusters single cell data."

    let matrixFile'     =
            MatrixFile . fromMaybe "matrix.mtx" . unHelpful . matrixFile $ opts
        genesFile'      =
            GeneFile . fromMaybe "genes.tsv" . unHelpful . genesFile $ opts
        cellsFile'      =
            CellFile . fromMaybe "barcodes.tsv" . unHelpful . cellsFile $ opts
        projectionFile' =
            fmap ProjectionFile . unHelpful . projectionFile $ opts
        labelsFile'     =
            fmap LabelFile . unHelpful . labelsFile $ opts
        prior'          =
            fmap PriorDirectory . unHelpful . prior $ opts
        delimiter'      =
            Delimiter . fromMaybe ',' . unHelpful . delimiter $ opts
        preNormalization' =
            PreNormalization . unHelpful . preNormalization $ opts
        minSize'        =
            MinClusterSize . fromMaybe 1 . unHelpful . minSize $ opts
        drawLeaf'       = maybe DrawText read . unHelpful . drawLeaf $ opts
        drawDendrogram' = unHelpful . drawDendrogram $ opts
        drawNodeNumber' = DrawNodeNumber . unHelpful . drawNodeNumber $ opts
        output'         =
            OutputDirectory . fromMaybe "out" . unHelpful . output $ opts
        matrixCsv       =
            any
                isNothing
                [unHelpful . genesFile $ opts, unHelpful . cellsFile $ opts]
        unFilteredSc   =
            if matrixCsv
                then loadSparseMatrixDataStream
                        delimiter'
                        projectionFile'
                        matrixFile'
                else loadCellrangerData
                        projectionFile'
                        genesFile'
                        cellsFile'
                        matrixFile'
        sc             = fmap filterSparseMat unFilteredSc
        processMat     = bool matrix (scaleSparseMat . matrix)
                       . unPreNormalization
                       $ preNormalization'
        processedSc    = sc >>= (\x -> return $ x { matrix = processMat x })

    -- Where to place output files.
    createDirectoryIfMissing True . unOutputDirectory $ output'

    labelMap     <- sequence . fmap (loadLabelData delimiter') $ labelsFile'

    let labelColorMap = fmap getLabelColorMap labelMap
        cellColorMap =
            case drawLeaf' of
                DrawText           ->
                    return $ do
                        lcm <- labelColorMap
                        lm  <- labelMap
                        return $ labelToCellColorMap lcm lm
                DrawCell DrawLabel ->
                    return $ do
                        lcm <- labelColorMap
                        lm  <- labelMap
                        return $ labelToCellColorMap lcm lm
                DrawCell (DrawExpression x) ->
                    fmap (Just . getCellColorMapExpression (Gene x)) processedSc

    --R.withEmbeddedR R.defaultConfig $ R.runRegion $ do
        -- For r clustering.
        -- mat         <- scToRMat processedSc
        -- clusterRes  <- hdbscan mat
        -- clusterList <- clustersToClusterList sc clusterRes

        -- For agglomerative clustering.
        --let clusterResults = fmap hClust processedSc

    -- Load previous results or write if first run.
    (clusterResults, bMat, graph) <-
        case prior' of
            Nothing -> do
                (cr, b, gr) <- fmap (hSpecClust minSize') processedSc

                B.writeFile
                    (unOutputDirectory output' FP.</> "cluster_results.json")
                    . A.encode
                    $ cr
                writeMatrix (unOutputDirectory output' FP.</> "b.mtx")
                    . spMatToMat
                    . unB
                    $ b
                T.writeFile
                    (unOutputDirectory output' FP.</> "graph.dot")
                    . G.printDotGraph
                    . G.graphToDot G.nonClusteredParams
                    . unCellGraph
                    $ gr

                return ((return cr, return b, return gr) :: (IO ClusterResults, IO B, IO CellGraph))
            (Just x) -> do
                let crInput =
                        (FP.</> "cluster_results.json") . unPriorDirectory $ x
                    bInput  = (FP.</> "b.mtx") . unPriorDirectory $ x
                    grInput  = (FP.</> "graph.dot") . unPriorDirectory $ x
                    cr :: IO ClusterResults
                    cr = fmap (either error id . A.eitherDecode)
                       . B.readFile
                       $ crInput
                    b :: IO B
                    b  = fmap (B . matToSpMat)
                       . readMatrix
                       $ bInput

                copyFile crInput
                    . (FP.</> "cluster_results.json")
                    . unOutputDirectory
                    $ output'
                copyFile bInput
                    . (FP.</> "b.mtx")
                    . unOutputDirectory
                    $ output'
                copyFile grInput
                    . (FP.</> "graph.dot")
                    . unOutputDirectory
                    $ output'

                gr <- fmap (dendrogramToGraph . clusterDend) cr

                return (cr, b, return gr)

    -- Find clumpiness.
    case labelMap of
        Nothing ->
            hPutStrLn stderr "Clumpiness requires labels for cells, skipping..."
        (Just lcm) ->
            clusterResults
              >>= B.writeFile (unOutputDirectory output' FP.</> "clumpiness.csv")
                . dendToClumpCsv lcm
                . clusterDend


    -- Header
    B.putStrLn $ "cell,cluster,path"

    -- Body
    clusterResults
        >>= B.putStrLn
          . CSV.encode
          . fmap (\ (!ci, !(c:cs))
                 -> ( unCell . barcode $ ci
                    , showt $ unCluster c
                    , T.intercalate "," . fmap (showt . unCluster) $ c:cs
                    )
                 )
          . clusterList

    -- | Plot only if needed and ignore non-tree analyses if dendrogram is
    -- supplied.
    H.withEmbeddedR defaultConfig $ H.runRegion $ do
        -- Plot dendrogram.
        H.io $ do
            cr <- clusterResults
            gr <- graph
            cm <- cellColorMap
            legend <- case drawLeaf' of
                        (DrawCell (DrawExpression g)) ->
                            fmap
                                (Just . plotExpressionLegend (Gene g))
                                processedSc
                        _ -> return $ fmap plotLabelLegend labelColorMap

            plot <- if drawDendrogram'
                    then return . plotDendrogram legend drawLeaf' cm . clusterDend $ cr
                    else do
                        plotGraph legend drawNodeNumber' drawLeaf' cm gr

            D.renderCairo
                    (unOutputDirectory output' FP.</> "dendrogram.pdf")
                    (D.mkHeight 1000)
                    plot

        -- Plot clustering.
        (H.io clusterResults)
          >>= plotClustersR (unOutputDirectory output' FP.</> "projection.pdf")
            . clusterList
            -- >>= D.renderCairo (x <> ".pdf") (D.mkWidth 1000)
            -- . D.renderAxis
            -- . plotClusters

        return ()
