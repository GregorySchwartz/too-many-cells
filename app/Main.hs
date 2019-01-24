{- too-many-cells
Gregory W. Schwartz

Clusters single cell data.
-}

{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TupleSections     #-}

module Main where

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
import Text.Read (readMaybe)
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

-- | Command line arguments
data Options
    = MakeTree { matrixPath :: [String] <?> "(PATH) The path to the input directory containing the matrix output of cellranger (matrix.mtx, genes.tsv, and barcodes.tsv) or an input csv file containing gene row names and cell column names. If given as a list (--matrixPath input1 --matrixPath input2 etc.) then will join all matrices together. Assumes the same number and order of genes in each matrix, so only cells are added."
               , projectionFile :: Maybe String <?> "([Nothing] | FILE) The input file containing positions of each cell for plotting. Format is \"barcode,x,y\" and matches column order in the matrix file. Useful for 10x where a TNSE projection is generated in \"projection.csv\". Cells without projections will not be plotted. If not supplied, no plot will be made."
               , cellWhitelistFile :: Maybe String <?> "([Nothing] | FILE) The input file containing the cells to include. No header, line separated list of barcodes."
               , labelsFile :: Maybe String <?> "([Nothing] | FILE) The input file containing the label for each cell barcode, with \"item,label\" header."
               , delimiter :: Maybe Char <?> "([,] | CHAR) The delimiter for the csv file if using a normal csv rather than cellranger output and for --labels-file."
               , normalization :: Maybe String <?> "([TfIdfNorm] | UQNorm | MedNorm | TotalMedNorm | BothNorm | NoneNorm) Type of normalization before clustering. TfIdfNorm normalizes based on the prevalence of each feature. UQNorm normalizes each observation by the upper quartile non-zero counts of that observation. MedNorm normalizes each observation by the median non-zero counts of that observation. TotalMedNorm normalized first each observation by total count then by median of non-zero counts across features. BothNorm uses both normalizations (first TotalMedNorm for all analysis then additionally TfIdfNorm during clustering). NoneNorm does not normalize. Default is TfIdfNorm for clustering and NoneNorm for differential (which instead uses the recommended edgeR single cell preprocessing including normalization and filtering, any normalization provided here will result in edgeR preprocessing on top). Cannot use TfIdfNorm for any other process as NoneNorm will become the default."
               , eigenGroup :: Maybe String <?> "([SignGroup] | KMeansGroup) Whether to group the eigenvector using the sign or kmeans while clustering. While the default is sign, kmeans may be more accurate (but starting points are arbitrary)."
               , numEigen :: Maybe Int <?> "([1] | INT) Number of eigenvectors to use while clustering with kmeans. Takes from the second to last eigenvector. Recommended to start at 1 and work up from there if needed. May help offset the possible instability and inaccuracy of SVDLIBC."
               , minSize :: Maybe Int <?> "([1] | INT) The minimum size of a cluster. Defaults to 1."
               , maxStep :: Maybe Int <?> "([Nothing] | INT) Only keep clusters that are INT steps from the root. Defaults to all steps."
               , maxProportion :: Maybe Double <?> "([Nothing] | DOUBLE) Stopping criteria to stop at the node immediate after a node with DOUBLE proportion split. So a node N with L and R children will stop with this criteria at 0.5 if |L| / |R| < 0.5 or > 2 (absolute log2 transformed), that is, if one child has over twice as many items as the other child. Includes L and R in the final result."
               , minDistance :: Maybe Double <?> "([Nothing] | DOUBLE) Stopping criteria to stop at the node immediate after a node with DOUBLE distance. So a node N with L and R children will stop with this criteria the distance at N to L and R is < DOUBLE. Includes L and R in the final result."
               , smartCutoff :: Maybe Double <?> "([Nothing] | DOUBLE) Whether to set the cutoffs for --min-size, --max-proportion, and --min-distance based off of the distributions (median + (DOUBLE * MAD)) of all nodes. To use smart cutoffs, use this argument and then set one of the three arguments to an arbitrary number, whichever cutoff type you want to use. --min-size distribution is log2 transformed."
               , dendrogramOutput :: Maybe String <?> "([dendrogram.svg] | FILE) The filename for the dendrogram. Supported formats are PNG, PS, PDF, and SVG."
               , matrixOutput :: Maybe String <?> "([Nothing] | FOLDER) Output the filtered and normalized (not including TfIdfNorm) matrix in this folder."
               , drawLeaf :: Maybe String <?> "([DrawText] | DrawItem DrawItemType) How to draw leaves in the dendrogram. DrawText is the number of items in that leaf. DrawItem is the collection of items represented by circles, consisting of: DrawItem DrawLabel, where each item is colored by its label, DrawItem (DrawContinuous FEATURE), where each item is colored by the expression of FEATURE (corresponding to a feature name in the input matrix), DrawItem (DrawThresholdContinuous [(FEATURE, DOUBLE)]), where each item is colored by the binary high / low expression of FEATURE based on DOUBLE and multiple FEATUREs can be used to combinatorically label items (FEATURE1 high / FEATURE2 low, etc.), DrawItem DrawSumContinuous, where each item is colored by the sum of the post-normalized columns (use --normalization NoneNorm for UMI counts, default), and DrawItem DrawDiversity, where each node is colored by the diversity based on the labels of each item and the color is normalized separately for the leaves and the inner nodes. The default is DrawText, unless --labels-file is provided, in which DrawItem DrawLabel is the default. If the label or feature cannot be found, the default color will be black (check your spelling!)."
               , drawCollection :: Maybe String <?> "([PieChart] | PieRing | PieNone | CollectionGraph MAXWEIGHT THRESHOLD [NODE]) How to draw item leaves in the dendrogram. PieRing draws a pie chart ring around the items. PieChart only draws a pie chart instead of items. PieNone only draws items, no pie rings or charts. (CollectionGraph MAXWEIGHT THRESHOLD [NODE]) draws the nodes and edges within leaves that are descendents of NODE (empty list [] indicates draw all leaf networks) based on the input matrix, normalizes edges based on the MAXWEIGHT, and removes edges for display less than THRESHOLD (after normalization, so for CollectionGraph 2 0.5 [26], draw the leaf graphs for all leaves under node 26, then a edge of 0.7 would be removed because (0.7 / 2) < 0.5)."
               , drawMark :: Maybe String <?> "([MarkNone] | MarkModularity) How to draw annotations around each inner node in the tree. MarkNone draws nothing and MarkModularity draws a black circle representing the modularity at that node, darker black means higher modularity for that next split."
               , drawNodeNumber :: Bool <?> "Draw the node numbers on top of each node in the graph."
               , drawMaxNodeSize :: Maybe Double <?> "([72] | DOUBLE) The max node size when drawing the graph. 36 is the theoretical default, but here 72 makes for thicker branches."
               , drawMaxLeafNodeSize :: Maybe Double <?> "([--draw-max-node-size] | DOUBLE) The max leaf node size when drawing the graph. Defaults to the value of --draw-max-node-size."
               , drawNoScaleNodes :: Bool <?> "Do not scale inner node size when drawing the graph. Instead, uses draw-max-node-size as the size of each node and is highly recommended to change as the default may be too large for this option."
               , drawLegendSep :: Maybe Double <?> "([1] | DOUBLE) The amount of space between the legend and the tree."
               , drawLegendAllLabels :: Bool <?> "Whether to show all the labels in the label file instead of only showing labels within the current tree. The program generates colors from all labels in the label file first in order to keep consistent colors. By default, this value is false, meaning that only the labels present in the tree are shown (even though the colors are the same). The subset process occurs after --draw-colors, so when using that argument make sure to account for all labels."
               , drawPalette :: Maybe String <?> "([Set1] | Hsv | Ryb) Palette to use for legend colors. With high saturation in --draw-scale-saturation, consider using Hsv to better differentiate colors."
               , drawColors :: Maybe String <?> "([Nothing] | COLORS) Custom colors for the labels or continuous features. Will repeat if more labels than provided colors. For continuous feature plots, uses first two colors [high, low], defaults to [red, white]. For instance: --draw-colors \"[\\\"#e41a1c\\\", \\\"#377eb8\\\"]\""
               , drawScaleSaturation :: Maybe Double <?> "([Nothing] | DOUBLE) Multiply the saturation value all nodes by this number in the HSV model. Useful for seeing more visibly the continuous colors by making the colors deeper against a gray scale."
               , pca :: Maybe Double <?> "([Nothing] | DOUBLE) The percent variance to retain for PCA dimensionality reduction before clustering. Default is no PCA at all in order to keep all information."
               , noFilter :: Bool <?> "Whether to bypass filtering genes and cells by low counts."
               , filterThresholds :: Maybe String <?> "([(250, 1)] | (DOUBLE, DOUBLE)) The minimum filter thresholds for (MINCELL, MINFEATURE) when filtering cells and features by low read counts. See also --no-filter."
               , prior :: Maybe String <?> "([Nothing] | STRING) The input folder containing the output from a previous run. If specified, skips clustering by using the previous clustering files."
               , order :: Maybe Double <?> "([1] | DOUBLE) The order of diversity."
               , clumpinessMethod :: Maybe String <?> "([Majority] | Exclusive | AllExclusive) The method used when calculating clumpiness: Majority labels leaves according to the most abundant label, Exclusive only looks at leaves consisting of cells solely from one label, and AllExclusive treats the leaf as containing both labels."
               , dense :: Bool <?> "Whether to use dense matrix algorithms for clustering. Should be faster for dense matrices, so if batch correction, PCA, or other algorithms are applied upstream to the input matrix, consider using this option to speed up the tree generation."
               , output :: Maybe String <?> "([out] | STRING) The folder containing output."}
    | Interactive { matrixPath :: [String] <?> "(PATH) The path to the input directory containing the matrix output of cellranger (matrix.mtx, genes.tsv, and barcodes.tsv) or an input csv file containing gene row names and cell column names. If given as a list (--matrixPath input1 --matrixPath input2 etc.) then will join all matrices together. Assumes the same number and order of genes in each matrix, so only cells are added."
                  , cellWhitelistFile :: Maybe String <?> "([Nothing] | FILE) The input file containing the cells to include. No header, line separated list of barcodes."
                  , labelsFile :: Maybe String <?> "([Nothing] | FILE) The input file containing the label for each cell barcode, with \"item,label\" header."
                  , delimiter :: Maybe Char <?> "([,] | CHAR) The delimiter for the csv file if using a normal csv rather than cellranger output and for --labels-file."
                  , normalization :: Maybe String <?> "([TfIdfNorm] | UQNorm | MedNorm | TotalMedNorm | BothNorm | NoneNorm) Type of normalization before clustering. TfIdfNorm normalizes based on the prevalence of each feature. UQNorm normalizes each observation by the upper quartile non-zero counts of that observation. MedNorm normalizes each observation by the median non-zero counts of that observation. TotalMedNorm normalized first each observation by total count then by median of non-zero counts across features. BothNorm uses both normalizations (first TotalMedNorm for all analysis then additionally TfIdfNorm during clustering). NoneNorm does not normalize. Default is TfIdfNorm for clustering and NoneNorm for differential (which instead uses the recommended edgeR single cell preprocessing including normalization and filtering, any normalization provided here will result in edgeR preprocessing on top). Cannot use TfIdfNorm for any other process as NoneNorm will become the default."
                  , pca :: Maybe Double <?> "([Nothing] | DOUBLE) The percent variance to retain for PCA dimensionality reduction before clustering. Default is no PCA at all in order to keep all information."
                  , noFilter :: Bool <?> "Whether to bypass filtering genes and cells by low counts."
                  , filterThresholds :: Maybe String <?> "([(250, 1)] | (DOUBLE, DOUBLE)) The minimum filter thresholds for (MINCELL, MINFEATURE) when filtering cells and features by low read counts. See also --no-filter."
                  , prior :: Maybe String <?> "([Nothing] | STRING) The input folder containing the output from a previous run. If specified, skips clustering by using the previous clustering files."}
    | Differential { matrixPath :: [String] <?> "(PATH) The path to the input directory containing the matrix output of cellranger (matrix.mtx, genes.tsv, and barcodes.tsv) or an input csv file containing gene row names and cell column names. If given as a list (--matrixPath input1 --matrixPath input2 etc.) then will join all matrices together. Assumes the same number and order of genes in each matrix, so only cells are added."
                   , cellWhitelistFile :: Maybe String <?> "([Nothing] | FILE) The input file containing the cells to include. No header, line separated list of barcodes."
                   , labelsFile :: Maybe String <?> "([Nothing] | FILE) The input file containing the label for each cell barcode, with \"item,label\" header."
                   , pca :: Maybe Double <?> "([Nothing] | DOUBLE) The percent variance to retain for PCA dimensionality reduction before clustering. Default is no PCA at all in order to keep all information."
                   , noFilter :: Bool <?> "Whether to bypass filtering genes and cells by low counts."
                   , filterThresholds :: Maybe String <?> "([(250, 1)] | (DOUBLE, DOUBLE)) The minimum filter thresholds for (MINCELL, MINFEATURE) when filtering cells and features by low read counts. See also --no-filter."
                   , delimiter :: Maybe Char <?> "([,] | CHAR) The delimiter for the csv file if using a normal csv rather than cellranger output and for --labels-file."
                   , normalization :: Maybe String <?> "([TfIdfNorm] | UQNorm | MedNorm | TotalMedNorm | BothNorm | NoneNorm) Type of normalization before clustering. TfIdfNorm normalizes based on the prevalence of each feature. UQNorm normalizes each observation by the upper quartile non-zero counts of that observation. MedNorm normalizes each observation by the median non-zero counts of that observation. TotalMedNorm normalized first each observation by total count then by median of non-zero counts across features. BothNorm uses both normalizations (first TotalMedNorm for all analysis then additionally TfIdfNorm during clustering). NoneNorm does not normalize. Default is TfIdfNorm for clustering and NoneNorm for differential (which instead uses the recommended edgeR single cell preprocessing including normalization and filtering, any normalization provided here will result in edgeR preprocessing on top). Cannot use TfIdfNorm for any other process as NoneNorm will become the default."
                   , prior :: Maybe String <?> "([Nothing] | STRING) The input folder containing the output from a previous run. If specified, skips clustering by using the previous clustering files."
                   , nodes :: String <?> "([NODE], [NODE]) Find the differential expression between cells belonging downstream of a list of nodes versus another list of nodes. \"([], [])\" switches the process to instead find the log2 average division between all nodes with all other cells (node / other cells) using the Kruskal-Wallis Test (--genes does not work for this, --labels works, and UQNorm for the normalization is recommended. Only returns nodes where the comparison had both groups containing at least five cells.)."
                   , labels :: Maybe String <?> "([Nothing] | ([LABEL], [LABEL])) Use --labels-file to restrict the differential analysis to cells with these labels. Same format as --nodes, so the first list in --nodes and --labels gets the cells within that list of nodes with this list of labels. The same for the second list. For instance, --nodes \"([1], [2])\" --labels \"([\\\"A\\\"], [\\\"B\\\"])\" will compare cells from node 1 of label \"A\" only with cells from node 2 of label \"B\" only. To use all cells for that set of nodes, use an empty list, i.e. --labels \"([], [\\\"A\\\"])\". When comparing all nodes with all other cells, remember that the notation would be ([Other Cells], [Node]), so to compare cells of label X in Node with cells of label Y in Other Cells, use --labels \"([\\\"Y\\\", \\\"X\\\"])\". Requires both --labels and --labels-file, otherwise will include all labels."
                   , topN :: Maybe Int <?> "([100] | INT ) The top INT differentially expressed genes."
                   , genes :: [T.Text] <?> "([Nothing] | GENE) List of genes to plot for all cells within selected nodes. Invoked by --genes CD4 --genes CD8 etc. When this argument is supplied, only the plot is outputted and edgeR differential expression is ignored. Outputs to --output."
                   , plotOutput :: Maybe String <?> "([out.pdf] | STRING) The file containing the output plot."}
    | Diversity { priors :: [String] <?> "(PATH) Either input folders containing the output from a run of too-many-cells or a csv files containing the clusters for each cell in the format \"cell,cluster\". Advanced features not available in the latter case. If --labels-file is specified, those labels designate entity type, otherwise the assigned cluster is the entity type."
                , delimiter :: Maybe Char <?> "([,] | CHAR) The delimiter for the csv file if using a normal csv rather than cellranger output and for --labels-file."
                , labelsFile :: Maybe String <?> "([Nothing] | FILE) The input file containing the label for each cell barcode, with \"item,label\" header."
                , start :: Maybe Integer <?> "([0] | INT) For the rarefaction curve, start the curve at this subsampling."
                , interval :: Maybe Integer <?> "([1] | INT) For the rarefaction curve, the amount to increase each subsampling. For instance, starting at 0 with an interval of 4, we would sampling 0, 4, 8, 12, ..."
                , end :: Maybe Integer <?> "([N] | INT) For the rarefaction curve, which subsample to stop at. By default, the curve stops at the observed number of species for each population."
                , order :: Maybe Double <?> "([1] | DOUBLE) The order of diversity."
                , output :: Maybe String <?> "([out] | STRING) The folder containing output."}
    | Paths { prior :: Maybe String <?> "([Nothing] | STRING) The input folder containing the output from a previous run. If specified, skips clustering by using the previous clustering files."
            , labelsFile :: Maybe String <?> "([Nothing] | FILE) The input file containing the label for each cell barcode, with \"item,label\" header."
            , flipDirection :: Bool <?> "Flip the starting node when calculating the distances."
            , pathDistance :: Maybe String <?> "([PathModularity] | PathStep) How to measure the distance from the starting leaf. PathModularity weighs the steps by the modularity, while PathStep counts the number of steps."
            , bandwidth :: Maybe Double <?> "([1] | DOUBLE) Bandwidth of the density plot."
            , delimiter :: Maybe Char <?> "([,] | CHAR) The delimiter for the csv file if using a normal csv rather than cellranger output and for --labels-file."
            , output :: Maybe String <?> "([out] | STRING) The folder containing output."}
    deriving (Generic)

modifiers :: Modifiers
modifiers = lispCaseModifiers { shortNameModifier = short }
  where
    short "clumpinessMethod"     = Just 'u'
    short "clusterNormalization" = Just 'C'
    short "dendrogramOutput"     = Just 'U'
    short "drawCollection"       = Just 'E'
    short "drawColors"           = Just 'R'
    short "drawDendrogram"       = Just 'D'
    short "drawLeaf"             = Just 'L'
    short "drawLegendAllLabels"  = Just 'J'
    short "drawLegendSep"        = Just 'Q'
    short "drawMark"             = Just 'K'
    short "drawMaxLeafNodeSize"  = Nothing
    short "drawMaxNodeSize"      = Just 'A'
    short "drawNoScaleNodes"     = Just 'W'
    short "drawNodeNumber"       = Just 'N'
    short "drawPalette"          = Just 'Y'
    short "drawScaleSaturation"  = Just 'V'
    short "eigenGroup"           = Just 'B'
    short "filterThresholds"     = Just 'H'
    short "labels"               = Nothing
    short "matrixOutput"         = Nothing
    short "maxDistance"          = Just 'T'
    short "maxProportion"        = Just 'X'
    short "maxStep"              = Just 'S'
    short "minDistance"          = Nothing
    short "minSize"              = Just 'M'
    short "noFilter"             = Just 'F'
    short "normalization"        = Just 'z'
    short "numEigen"             = Just 'G'
    short "order"                = Just 'O'
    short "pca"                  = Just 'a'
    short "plotOutput"           = Nothing
    short "priors"               = Just 'P'
    short "projectionFile"       = Just 'j'
    short x                      = firstLetter x

instance ParseRecord Options where
    parseRecord = parseRecordWithModifiers modifiers

-- | Notify user of limitations and error out for incompatabilities. Empty for
-- now.
limitationWarningsErrors :: Options -> IO ()
limitationWarningsErrors opts = do
    return ()

-- | Read or return an error.
readOrErr err = fromMaybe (error err) . readMaybe

-- | Normalization defaults.
getNormalization :: Options -> NormType
getNormalization opts@(MakeTree{}) =
  maybe
    TfIdfNorm
    (readOrErr "Cannot read --normalization.")
    . unHelpful
    . normalization
    $ opts
getNormalization opts@(Interactive{}) =
  maybe
    TfIdfNorm
    (readOrErr "Cannot read --normalization.")
    . unHelpful
    . normalization
    $ opts
getNormalization opts@(Differential{}) =
  maybe
    NoneNorm
    (readOrErr "Cannot read --normalization.")
    . unHelpful
    . normalization
    $ opts

-- | Load the single cell matrix.
loadSSM :: Options -> FilePath -> IO SingleCells
loadSSM opts matrixPath' = do
    fileExist      <- FP.doesFileExist matrixPath'
    directoryExist <- FP.doesDirectoryExist matrixPath'

    let matrixFile' =
            case (fileExist, directoryExist) of
                (False, False) -> error "\nMatrix path does not exist."
                (True, False)  -> Left $ MatrixFile matrixPath'
                (False, True)  ->
                    Right . MatrixFile $ matrixPath' FP.</> "matrix.mtx"
        genesFile'  = GeneFile $ matrixPath' FP.</> "genes.tsv"
        cellsFile'  = CellFile $ matrixPath' FP.</> "barcodes.tsv"
        delimiter'      =
            Delimiter . fromMaybe ',' . unHelpful . delimiter $ opts
        unFilteredSc   =
            case matrixFile' of
                (Left file) -> loadSparseMatrixDataStream
                                delimiter'
                                file
                (Right file) -> loadCellrangerData
                                    genesFile'
                                    cellsFile'
                                    file
    unFilteredSc

-- | Load all single cell matrices.
loadAllSSM :: Options -> IO (Maybe SingleCells)
loadAllSSM opts = runMaybeT $ do
    let matrixPaths'       = unHelpful . matrixPath $ opts
        cellWhitelistFile' =
            fmap CellWhitelistFile . unHelpful . cellWhitelistFile $ opts
        normalization'     = getNormalization opts
        pca'               = fmap PCAVar . unHelpful . pca $ opts
        noFilterFlag'      = NoFilterFlag . unHelpful . noFilter $ opts
        filterThresholds'  = FilterThresholds
                           . maybe (250, 1) read
                           . unHelpful
                           . filterThresholds
                           $ opts

    mats <- MaybeT
          $ if null matrixPaths'
              then return Nothing
              else fmap Just . mapM (loadSSM opts) $ matrixPaths'
    cellWhitelist <- liftIO . sequence $ fmap getCellWhitelist cellWhitelistFile'

    let whiteListFilter Nothing = id
        whiteListFilter (Just wl) = filterWhitelistSparseMat wl
        unFilteredSc = mconcat mats
        sc           =
            (\ x -> if unNoFilterFlag noFilterFlag'
                        then x
                        else filterNumSparseMat filterThresholds' x
            )
                . whiteListFilter cellWhitelist
                $ unFilteredSc
        normMat TfIdfNorm    = id -- Normalize during clustering.
        normMat UQNorm       = uqScaleSparseMat
        normMat MedNorm      = medScaleSparseMat
        normMat TotalMedNorm = scaleSparseMat
        normMat BothNorm     = scaleSparseMat
        normMat NoneNorm     = id
        processMat  = (\m -> maybe m (flip pcaDenseMat m) pca')
                    . normMat normalization'
                    . _matrix
        processedSc = sc { _matrix = processMat sc }

    return processedSc

makeTreeMain :: Options -> IO ()
makeTreeMain opts = H.withEmbeddedR defaultConfig $ do
    let readOrErr err = fromMaybe (error err) . readMaybe
        matrixPaths'      = unHelpful . matrixPath $ opts
        labelsFile'       =
            fmap LabelFile . unHelpful . labelsFile $ opts
        prior'            =
            fmap PriorPath . unHelpful . prior $ opts
        delimiter'        =
            Delimiter . fromMaybe ',' . unHelpful . delimiter $ opts
        eigenGroup'       =
            maybe SignGroup (readOrErr "Cannot read eigen-group.")
              . unHelpful
              . eigenGroup
              $ opts
        dense'            = DenseFlag . unHelpful . dense $ opts
        normalization'    = getNormalization opts
        numEigen'         = fmap NumEigen . unHelpful . numEigen $ opts
        minSize'          = fmap MinClusterSize . unHelpful . minSize $ opts
        maxStep'          = fmap MaxStep . unHelpful . maxStep $ opts
        maxProportion'    =
            fmap MaxProportion . unHelpful . maxProportion $ opts
        minDistance'      = fmap MinDistance . unHelpful . minDistance $ opts
        smartCutoff'      = fmap SmartCutoff . unHelpful . smartCutoff $ opts
        dendrogramOutput' = DendrogramFile
                          . fromMaybe "dendrogram.svg"
                          . unHelpful
                          . dendrogramOutput
                          $ opts
        matrixOutput'     = fmap (MatrixFile . (unOutputDirectory output' FP.</>))
                          . unHelpful
                          . matrixOutput
                          $ opts
        drawLeaf'         =
            maybe
              (maybe DrawText (const (DrawItem DrawLabel)) labelsFile')
              (readOrErr "Cannot read draw-leaf.")
                . unHelpful
                . drawLeaf
                $ opts
        drawCollection'   =
            maybe PieChart (readOrErr "Cannot read draw-collection.")
              . unHelpful
              . drawCollection
              $ opts
        drawMark'         = maybe MarkNone (readOrErr "Cannot read draw-mark.")
                          . unHelpful
                          . drawMark
                          $ opts
        drawNodeNumber'   = DrawNodeNumber . unHelpful . drawNodeNumber $ opts
        drawMaxNodeSize'  =
            DrawMaxNodeSize . fromMaybe 72 . unHelpful . drawMaxNodeSize $ opts
        drawMaxLeafNodeSize' = DrawMaxLeafNodeSize
                             . fromMaybe (unDrawMaxNodeSize drawMaxNodeSize')
                             . unHelpful
                             . drawMaxLeafNodeSize
                             $ opts
        drawNoScaleNodes' =
            DrawNoScaleNodesFlag . unHelpful . drawNoScaleNodes $ opts
        drawLegendSep'    = DrawLegendSep
                          . fromMaybe 1
                          . unHelpful
                          . drawLegendSep
                          $ opts
        drawLegendAllLabels' =
            DrawLegendAllLabels . unHelpful . drawLegendAllLabels $ opts
        drawPalette' = maybe
                        Set1
                        (fromMaybe (error "Cannot read palette.") . readMaybe)
                     . unHelpful
                     . drawPalette
                     $ opts
        drawColors'       = fmap ( CustomColors
                                 . fmap sRGB24read
                                 . (\x -> readOrErr "Cannot read draw-colors." x :: [String])
                                 )
                          . unHelpful
                          . drawColors
                          $ opts
        drawScaleSaturation' =
            fmap DrawScaleSaturation . unHelpful . drawScaleSaturation $ opts
        order'            = Order . fromMaybe 1 . unHelpful . order $ opts
        clumpinessMethod' =
            maybe Clump.Majority (readOrErr "Cannot read clumpiness-method.")
              . unHelpful
              . clumpinessMethod
              $ opts
        projectionFile' =
            fmap ProjectionFile . unHelpful . projectionFile $ opts
        output'           =
            OutputDirectory . fromMaybe "out" . unHelpful . output $ opts

    -- Increment  progress bar.
    Progress.autoProgressBar
        (Progress.msg "Loading matrix")
        Progress.percentage
        80
        $ Progress.Progress 0 10

    -- Load matrix once.
    processedSc <- loadAllSSM opts

    -- Notify user of limitations.
    limitationWarningsErrors opts

    -- Increment  progress bar.
    Progress.autoProgressBar
        (Progress.msg "Planning leaf colors")
        Progress.percentage
        80
        $ Progress.Progress 1 10

    -- Where to place output files.
    FP.createDirectoryIfMissing True . unOutputDirectory $ output'

    -- Get the label map from either a file or from expression thresholds.
    labelMap <- case drawLeaf' of
                    (DrawItem (DrawThresholdContinuous gs)) ->
                        return
                            . Just
                            . getLabelMapThresholdContinuous
                                (fmap (L.over L._1 Feature) gs)
                            . extractSc
                            $ processedSc
                    _ -> mapM (loadLabelData delimiter') $ labelsFile'

    -- Increment  progress bar.
    Progress.autoProgressBar
        (Progress.msg "Planting tree")
        Progress.percentage
        80
        $ Progress.Progress 2 10

    --R.withEmbeddedR R.defaultConfig $ R.runRegion $ do
        -- For r clustering.
        -- mat         <- scToRMat processedSc
        -- clusterRes  <- hdbscan mat
        -- clusterList <- clustersToClusterList sc clusterRes

        -- For agglomerative clustering.
        --let clusterResults = fmap hClust processedSc

    -- Load previous results or calculate results if first run.
    originalClusterResults <- case prior' of
        Nothing -> do
            let (fullCr, _) =
                  hSpecClust dense' eigenGroup' normalization' numEigen'
                    . extractSc
                    $ processedSc

            return fullCr :: IO ClusterResults
        (Just x) -> do
            let clInput = unPriorPath x FP.</> "cluster_list.json"
                treeInput = unPriorPath x FP.</> "cluster_tree.json"

            -- Strict loading in order to avoid locked file.
            loadClusterResultsFiles clInput treeInput

    -- Increment  progress bar.
    Progress.autoProgressBar
        (Progress.msg "Measuring roots")
        Progress.percentage
        80
        $ Progress.Progress 3 10

    let birchMat = processedSc
        birchSimMat =
            case (not . null . unHelpful . matrixPath $ opts, drawCollection') of
                (True, CollectionGraph{} )  ->
                    Just
                        . B2Matrix
                        . L.over matrix (MatObsRow . unB2 . b1ToB2 . B1 . unMatObsRow)
                        . extractSc
                        $ processedSc
                _ -> Nothing

    -- Increment  progress bar.
    Progress.autoProgressBar
        (Progress.msg "Sketching tree")
        Progress.percentage
        80
        $ Progress.Progress 4 10

    let config :: BirchBeer.Types.Config CellInfo SingleCells
        config = BirchBeer.Types.Config
                    { _birchLabelMap = labelMap
                    , _birchMinSize = minSize'
                    , _birchMaxStep = maxStep'
                    , _birchMaxProportion = maxProportion'
                    , _birchMinDistance = minDistance'
                    , _birchSmartCutoff = smartCutoff'
                    , _birchOrder = Just order'
                    , _birchDrawLeaf = drawLeaf'
                    , _birchDrawCollection = drawCollection'
                    , _birchDrawMark = drawMark'
                    , _birchDrawNodeNumber = drawNodeNumber'
                    , _birchDrawMaxNodeSize = drawMaxNodeSize'
                    , _birchDrawMaxLeafNodeSize = drawMaxLeafNodeSize'
                    , _birchDrawNoScaleNodes = drawNoScaleNodes'
                    , _birchDrawLegendSep    = drawLegendSep'
                    , _birchDrawLegendAllLabels = drawLegendAllLabels'
                    , _birchDrawPalette = drawPalette'
                    , _birchDrawColors = drawColors'
                    , _birchDrawScaleSaturation = drawScaleSaturation'
                    , _birchTree = _clusterDend originalClusterResults
                    , _birchMat = birchMat
                    , _birchSimMat = birchSimMat
                    }

    (plot, labelColorMap, itemColorMap, markColorMap, tree', gr') <- mainDiagram config

    -- Increment  progress bar.
    Progress.autoProgressBar
        (Progress.msg "Recording tree measurements")
        Progress.percentage
        80
        $ Progress.Progress 5 10

    -- Write results.
    clusterResults <- case prior' of
        Nothing -> do
            let clusterList' = treeToClusterList tree'
                cr' = ClusterResults clusterList' tree'

            return cr'

        (Just x) -> do
            let clusterList' = treeToClusterList tree'
                cr' = ClusterResults clusterList' tree'

            return cr'

    B.writeFile
        (unOutputDirectory output' FP.</> "cluster_list.json")
        . A.encode
        . _clusterList
        $ clusterResults
    B.writeFile
        (unOutputDirectory output' FP.</> "cluster_tree.json")
        . A.encode
        . _clusterDend
        $ clusterResults
    T.writeFile
        (unOutputDirectory output' FP.</> "graph.dot")
        . G.printDotGraph
        . G.graphToDot G.nonClusteredParams
        . unClusterGraph
        $ gr'
    B.writeFile
        (unOutputDirectory output' FP.</> "cluster_info.csv")
        . printClusterInfo
        $ gr'
    case matrixOutput' of
        Nothing  -> return ()
        (Just x) -> writeMatrixLike x . extractSc $ processedSc
    B.writeFile
        (unOutputDirectory output' FP.</> "node_info.csv")
        . printNodeInfo labelMap
        $ gr'
    case labelMap of
        Nothing   -> return ()
        (Just lm) ->
            case clusterDiversity order' lm clusterResults of
                (Left err) -> hPutStrLn stderr
                            $ err
                           <> "\nError in diversity, skipping cluster_diversity.csv output."
                (Right result) ->
                    B.writeFile
                        ( unOutputDirectory output'
                   FP.</> "cluster_diversity.csv"
                        )
                        . printClusterDiversity
                        $ result

    -- Increment  progress bar.
    Progress.autoProgressBar
        (Progress.msg "Counting leaves")
        Progress.percentage
        80
        $ Progress.Progress 6 10

    -- Header
    B.putStrLn $ "cell,cluster,path"

    -- Body
    B.putStrLn
        . CSV.encode
        . fmap (\ (!ci, !(c:cs))
                -> ( unCell . _barcode $ ci
                , showt $ unCluster c
                , T.intercalate "/" . fmap (showt . unCluster) $ c:cs
                )
                )
        . _clusterList
        $ clusterResults

    -- Increment  progress bar.
    Progress.autoProgressBar
        (Progress.msg "Painting sketches")
        Progress.percentage
        80
        $ Progress.Progress 7 10

    -- Plot only if needed and ignore non-tree analyses if dendrogram is
    -- supplied.
    H.runRegion $ do
        -- Calculations with plotting the label map (clumpiness).
        case labelMap of
            Nothing ->
                H.io $ hPutStrLn stderr "\nClumpiness requires labels for cells, skipping..."
            (Just lm) -> do
                -- Get clumpiness.
                case treeToClumpList clumpinessMethod' lm . _clusterDend $ clusterResults of
                    (Left err) -> H.io
                                . hPutStrLn stderr
                                $ err
                               <> "\nError in clumpiness, skipping clumpiness.* output."
                    (Right clumpList) -> do
                        -- Save clumpiness to a file.
                        H.io
                            . B.writeFile (unOutputDirectory output' FP.</> "clumpiness.csv")
                            . clumpToCsv
                            $ clumpList

                        -- Plot clumpiness.
                        either (H.io . hPutStrLn stderr) id
                            $ plotClumpinessHeatmapR
                                (unOutputDirectory output' FP.</> "clumpiness.pdf")
                                clumpList

        -- Increment  progress bar.
        H.io $ Progress.autoProgressBar
            (Progress.msg "Painting tree")
            Progress.percentage
            80
            $ Progress.Progress 8 10

        -- Plot.
        H.io $ do
            -- cr <- clusterResults
            -- gr <- graph
            -- cm <- itemColorMap

            -- plot <- if drawDendrogram'
            --         then return . plotDendrogram legend drawLeaf' cm . _clusterDend $ cr
            --         else do
            --             plotGraph legend drawConfig cm markColorMap gr

            D.renderCairo
                    ( unOutputDirectory output'
               FP.</> unDendrogramFile dendrogramOutput'
                    )
                    (D.mkHeight 1000)
                    plot

        -- Increment  progress bar.
        H.io $ Progress.autoProgressBar
            (Progress.msg "Squishing tree")
            Progress.percentage
            80
            $ Progress.Progress 9 10

        -- Plot clustering of the projections.
        case projectionFile' of
          Nothing  -> return ()
          (Just f) -> do
            -- Load projection map if provided.
            projectionMap <- H.io $ loadProjectionMap f

            plotClustersR
              (unOutputDirectory output' FP.</> "projection.pdf")
              projectionMap
              . _clusterList
              $ clusterResults

            -- Plot clustering with labels.
            case (labelMap, itemColorMap) of
                (Just lm, Just icm) ->
                    plotLabelClustersR
                        (unOutputDirectory output' FP.</> "label_projection.pdf")
                        projectionMap
                        lm
                        icm
                        (_clusterList clusterResults)
                _ -> return ()

        -- Increment  progress bar.
        H.io $ Progress.autoProgressBar
            (Progress.msg "Packing up")
            Progress.percentage
            80
            $ Progress.Progress 10 10

        return ()

-- | Interactive tree interface.
interactiveMain :: Options -> IO ()
interactiveMain opts = H.withEmbeddedR defaultConfig $ do
    let labelsFile'    =
            fmap LabelFile . unHelpful . labelsFile $ opts
        prior'         = maybe (error "\nRequires --prior") PriorPath
                       . unHelpful
                       . prior
                       $ opts
        delimiter'     =
            Delimiter . fromMaybe ',' . unHelpful . delimiter $ opts
        normalization'    = getNormalization opts

    mat <- loadAllSSM opts
    labelMap <- sequence . fmap (loadLabelData delimiter') $ labelsFile'

    tree <- fmap (either error id . A.eitherDecode)
          . B.readFile
          . (FP.</> "cluster_tree.json")
          . unPriorPath
          $ prior' :: IO (Tree (TreeNode (V.Vector CellInfo)))

    interactiveDiagram
        tree
        labelMap
        mat
        . fmap ( B2Matrix
               . L.over matrix (MatObsRow . unB2 . b1ToB2 . B1 . unMatObsRow)
               )
        $ mat

    return ()

-- | Differential path.
differentialMain :: Options -> IO ()
differentialMain opts = do
    let readOrErr err = fromMaybe (error err) . readMaybe
        delimiter'     =
            Delimiter . fromMaybe ',' . unHelpful . delimiter $ opts
        labelsFile' =
            fmap LabelFile . unHelpful . labelsFile $ opts
        nodes'    =
          DiffNodes . readOrErr "Cannot read --nodes." . unHelpful . nodes $ opts
        prior'    = PriorPath
                  . fromMaybe (error "\nRequires a previous run to get the graph.")
                  . unHelpful
                  . prior
                  $ opts
        topN'     = TopN . fromMaybe 100 . unHelpful . topN $ opts
        genes'    = fmap Gene . unHelpful . genes $ opts
        labels'   = fmap ( DiffLabels
                         . L.over L.both ( (\x -> bool (Just x) Nothing . Set.null $ x)
                                         . Set.fromList
                                         . fmap Label
                                         )
                         . readOrErr "Cannot read --labels."
                         )
                  . unHelpful
                  . labels
                  $ opts
        (combined1, combined2) = combineNodesLabels nodes' labels'
        plotOutputR = fromMaybe "out.pdf" . unHelpful . plotOutput $ opts

    processedSc <- loadAllSSM opts

    labelMap <- mapM (loadLabelData delimiter') $ labelsFile'

    let clInput = (FP.</> "cluster_list.json") . unPriorPath $ prior'
        treeInput = (FP.</> "cluster_tree.json") . unPriorPath $ prior'

        cr :: IO ClusterResults
        cr = loadClusterResultsFiles clInput treeInput

    gr <- fmap (treeToGraph . _clusterDend) cr

    H.withEmbeddedR defaultConfig $ H.runRegion $ do
      case genes' of
        [] -> do
          case nodes' of
            (DiffNodes ([], [])) -> do
              let res = getAllDEGraphKruskalWallis
                          topN'
                          labelMap
                          (maybe (DiffLabels (Nothing, Nothing)) id labels')
                          (extractSc processedSc)
                      $ gr

              H.io . B.putStrLn . getAllDEStringKruskalWallis $ res
            (DiffNodes ([], _)) -> error "Need other nodes to compare with. If every node should be compared to all other nodes using Mann-Whitney U, use \"([], [])\"."
            (DiffNodes (_, [])) -> error "Need other nodes to compare with. If every node should be compared to all other nodes using Mann-Whitney U, use \"([], [])\"."
            _ -> do
              res <- getDEGraph
                      topN'
                      labelMap
                      (extractSc processedSc)
                      combined1
                      combined2
                      gr

              H.io . B.putStrLn . getDEString $ res
        _ -> do
          diffPlot <- getSingleDiff
                       False
                       labelMap
                       (extractSc processedSc)
                       combined1
                       combined2
                       genes'
                       gr
          [r| suppressMessages(ggsave(diffPlot_hs, file = plotOutputR_hs)) |]

          let normOutputR = FP.replaceBaseName
                              plotOutputR
                             (FP.takeBaseName plotOutputR <> "_scaled")

          diffNormPlot <- getSingleDiff
                            True
                            labelMap
                            (extractSc processedSc)
                            combined1
                            combined2
                            genes'
                            gr
          [r| suppressMessages(ggsave(diffNormPlot_hs, file = normOutputR_hs)) |]

          return ()

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

main :: IO ()
main = do
    opts <- getRecord "too-many-cells, Gregory W. Schwartz.\
                      \ Clusters and analyzes single cell data."

    case opts of
        MakeTree{}       -> makeTreeMain opts
        Interactive{}    -> interactiveMain opts
        Differential{}   -> differentialMain opts
        Main.Diversity{} -> diversityMain opts
        Paths{}          -> pathsMain opts
