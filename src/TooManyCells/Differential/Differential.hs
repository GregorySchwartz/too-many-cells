{- TooManyCells.Differential.Differential
Gregory W. Schwartz

Functions for finding the differential between groups of cells.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}

module TooManyCells.Differential.Differential
    ( scToTwoD
    , getDEGraph
    , getDEGraphKruskalWallis
    , getDEString
    , getDEStringKruskalWallis
    , getSingleDiff
    , combineNodesLabels
    , getAllDEGraphKruskalWallis
    , getAllDEStringKruskalWallis
    ) where

-- Remote
import Data.Bool (bool)
import BirchBeer.Types
import BirchBeer.Utility (getGraphLeaves, getGraphLeafItems)
import Control.Monad (join, mfilter)
import Data.Function (on)
import Data.List (sort, sortBy, groupBy, genericLength, partition, foldl')
import Data.Maybe (fromMaybe, catMaybes, isJust)
import Data.Monoid ((<>))
import Language.R as R
import Language.R.QQ (r)
import TextShow (showt)
import Control.Parallel.Strategies (parMap, withStrategy, parBuffer, rdeepseq)
import qualified "differential" Differential as Diff
import qualified "differential" Plot as Diff
import qualified "differential" Types as Diff
import qualified Control.Concurrent.Async as Async
import qualified Control.Foldl as Fold
import qualified Control.Lens as L
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Csv as CSV
import qualified Data.Foldable as F
import qualified Data.Graph.Inductive as G
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Sparse.Common as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified H.Prelude as H
import qualified System.FilePath as FP
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC

-- Local
import TooManyCells.Differential.Types
import TooManyCells.MakeTree.Types
import TooManyCells.Matrix.Types

-- | Convert a single cell matrix to a two dimensional matrix.
scToTwoD :: [(Int, Cell, (Int, Diff.Status))] -> SingleCells -> Diff.TwoDMat
scToTwoD cellGroups sc =
    Diff.TwoDMat rNames cNames statuses nRows nCols . S.toListSM $ filteredMat
  where
    rNames = fmap (Diff.Name . unFeature) . V.toList . _colNames $ sc
    cNames = fmap (Diff.Name . unCell . L.view L._2) cellGroups -- We flip row and column because cells are columns here
    statuses = fmap (Diff.Status . showt . L.view (L._3 . L._1)) cellGroups
    nRows    = S.nrows filteredMat
    nCols    = S.ncols filteredMat
    filteredMat = S.fromColsL -- Here the columns should be observations.
                . fmap (S.extractRow (unMatObsRow . _matrix $ sc) . L.view L._1)
                . filter ((>) (S.nrows . unMatObsRow $ _matrix sc) . L.view L._1)
                $ cellGroups

-- | Get the indices and statuses for a list of groups of nodes.
getStatuses
    :: Maybe LabelMap
    -> [([G.Node], Maybe (Set.Set Label))]
    -> ClusterGraph CellInfo
    -> [(Int, Cell, (Int, Diff.Status))]
getStatuses lm gs (ClusterGraph gr) =
    sort
        . concatMap F.toList
        . zipWith (\x (a, b) -> collapseStatus x a b) [1..]
        $ gs
  where
    collapseStatus s vs ls =
        fmap (\ !x -> (unRow . _cellRow $ x, _barcode x, (s, Diff.Status $ statusName vs ls)))
            . mfilter (validCellInfo lm ls)
            . join
            . mconcat
            . fmap (fmap (fromMaybe mempty . snd) . getGraphLeaves gr)
            $ vs
    statusName vs Nothing = showt vs
    statusName vs (Just ls) =
      (T.intercalate " " . fmap unLabel . Set.toAscList $ ls) <> " " <> showt vs

-- | Get the indices and statuses for a list of groups of nodes and subsample if
-- desired.
subsampleGetStatuses
    :: Seed
    -> Maybe Subsample
    -> Maybe LabelMap
    -> [([G.Node], Maybe (Set.Set Label))]
    -> ClusterGraph CellInfo
    -> IO [(Int, Cell, (Int, Diff.Status))]
subsampleGetStatuses seed subN lm gs gr =
  maybe return (subsampleGroups seed) subN $ getStatuses lm gs gr

-- | Filter barcodes by labels.
validCellInfo :: Maybe LabelMap -> Maybe (Set.Set Label) -> CellInfo -> Bool
validCellInfo Nothing _ = const True
validCellInfo _ Nothing = const True
validCellInfo (Just (LabelMap lm)) (Just ls) =
  maybe False (flip Set.member ls)
    . flip Map.lookup lm
    . Id
    . unCell
    . L.view barcode

-- | Filter barcodes by labels.
validCell :: Maybe LabelMap -> Maybe (Set.Set Label) -> Cell -> Bool
validCell Nothing _ = const True
validCell _ Nothing = const True
validCell (Just (LabelMap lm)) (Just ls) =
  maybe False (flip Set.member ls)
    . flip Map.lookup lm
    . Id
    . unCell

-- | Get the differential expression of two sets of cells, filtered by labels.
getDEGraph :: Seed
           -> Maybe Subsample
           -> TopN
           -> Maybe LabelMap
           -> SingleCells
           -> ([G.Node], Maybe (Set.Set Label))
           -> ([G.Node], Maybe (Set.Set Label))
           -> ClusterGraph CellInfo
           -> R.R s [(Diff.Name, Double, Diff.PValue, Diff.FDR)]
getDEGraph seed subN (TopN topN) lm sc v1 v2 gr = do
    cellGroups <- H.io $ subsampleGetStatuses seed subN lm [v1, v2] gr

    let mat        = scToTwoD cellGroups sc

    Diff.edgeR topN mat

-- | Get the differential expression using Kruskall-Wallis of two sets of cells,
-- filtered by labels.
getDEGraphKruskalWallis
  :: Seed
  -> Maybe Subsample
  -> TopN
  -> Maybe LabelMap
  -> SingleCells
  -> ([G.Node], Maybe (Set.Set Label))
  -> ([G.Node], Maybe (Set.Set Label))
  -> ClusterGraph CellInfo
  -> IO [ ( Feature
          , Diff.Log2Diff
          , Maybe Diff.PValue
          , Maybe Diff.FDR
          , Maybe Diff.QValue
          )
        ]
getDEGraphKruskalWallis seed subN (TopN topN) lm sc v1 v2 gr = do
  cellGroups <- subsampleGetStatuses seed subN lm [v1, v2] gr

  let fastFiveCheck = (< 5) . length . take 5
      res = filter (isJust . L.view L._3)
          . zipWith
                (\name (!a, !b, !c, !d) -> (name, a, b, c, d))
                (V.toList . L.view colNames $ sc)
          . Diff.differentialMatrixFeatRow as bs
          . S.transpose
          . unMatObsRow
          . L.view matrix
          $ sc
      (as, bs) = L.over L.both (fmap (L.view L._1))
               . partition ((== 1) . L.view (L._3 . L._1))
               $ cellGroups
  if fastFiveCheck as || fastFiveCheck bs
    then error "Less than five cells in one node to compare."
    else return . take topN . sortBy (compare `on` L.view L._3) $ res

-- | Get the differential expression of each cluster to all other cells in the
-- data set using KruskalWallis.
getAllDEGraphKruskalWallis
  :: Seed
  -> Maybe Subsample
  -> TopN
  -> Maybe LabelMap
  -> DiffLabels
  -> SingleCells
  -> ClusterGraph CellInfo
  -> IO [(G.Node, Feature, Diff.Log2Diff, Maybe Diff.PValue, Maybe Diff.FDR, Maybe Diff.QValue)]
getAllDEGraphKruskalWallis seed subN topN lm ls sc gr =
  mconcat
    . catMaybes
  <$> Async.mapConcurrently (\n -> compareClusterToOthersKruskalWallis n seed subN topN lm ls sc mat gr)
        nodes
  where
    nodes = filter (/= 0) . G.nodes . unClusterGraph $ gr -- Don't want to look at root.
    mat = S.transpose . unMatObsRow . L.view matrix $ sc

-- | Get the differential expression of a cluster (n) to all other cells in the
-- data set (ns) using KruskalWallis such that n / ns.
compareClusterToOthersKruskalWallis
  :: G.Node
  -> Seed
  -> Maybe Subsample
  -> TopN
  -> Maybe LabelMap
  -> DiffLabels
  -> SingleCells
  -> S.SpMatrix Double
  -> ClusterGraph CellInfo
  -> IO (Maybe [(G.Node, Feature, Diff.Log2Diff, Maybe Diff.PValue, Maybe Diff.FDR, Maybe Diff.QValue)])
compareClusterToOthersKruskalWallis n (Seed seed) subN (TopN topN) lm (DiffLabels (ls1, ls2)) sc mat gr = do
  let fastFiveCheck = (< 5) . length . take 5
      nCells' = F.toList $ getGraphLeafItems gr n
      nCellsSet = Set.fromList . fmap (L.view barcode) $ nCells'
      nCells = fmap (unRow . L.view cellRow)
             . filter (validCellInfo lm ls2)
             $ nCells' -- All cells from node and labels
      nsCells =
        fmap fst
          . filter (\ (_, !x) -> validCell lm ls1 x && not (Set.member x nCellsSet)) -- All cells outside of node and from labels
          . zip [0..]
          . V.toList
          . L.view rowNames
          $ sc
      sample n x =
        maybe
          return
          (\s -> fmap (take s . V.toList) . flip MWC.uniformShuffle x . V.fromList)
          n
      subN' = bool (min (length nCells) (length nsCells)) (maybe 0 unSubsample subN)
            . (/= Subsample 0)
          <$> subN
  g <- MWC.restore . MWC.toSeed $ V.fromList [fromIntegral seed]
  nCellsFinal  <- sample subN' g nCells
  nsCellsFinal <- sample subN' g nsCells

  let res = filter (isJust . L.view L._4)
          . ( zipWith
                (\name (!a, !b, !c, !d) -> (n, name, a, b, c, d))
                (V.toList . L.view colNames $ sc)
            )
          $ Diff.differentialMatrixFeatRow nsCellsFinal nCellsFinal mat -- Here the matrix rows are features

  if fastFiveCheck nCellsFinal || fastFiveCheck nsCellsFinal
    then return Nothing
    else return . Just . take topN . sortBy (compare `on` (L.view L._4)) $ res

-- | Get the differential expression of two sets of cells.
getDEString :: [(Diff.Name, Double, Diff.PValue, Diff.FDR)]
            -> B.ByteString
getDEString xs = header <> "\n" <> body
  where
    header = "feature,log2FC,pVal,qVal"  -- edgeR calls q-values FDR
    body   = CSV.encode
           . fmap ( L.over L._1 Diff.unName
                  . L.over L._3 Diff.unPValue
                  . L.over L._4 Diff.unFDR
                  )
           $ xs

-- | Get the differential expression of each node to all other nodes using
-- KruskalWallis.
getAllDEStringKruskalWallis
  :: [(G.Node, Feature, Diff.Log2Diff, Maybe Diff.PValue, Maybe Diff.FDR, Maybe Diff.QValue)]
  -> B.ByteString
getAllDEStringKruskalWallis xs = header <> "\n" <> body
  where
    header = "node,feature,log2FC,pVal,qVal"
    body   = CSV.encode
           . fmap ( (\(!a, !b, !c, !d, !e, !f) -> (a,b,c,d,f))
                  . L.over L._6 (maybe "NA" (showt . Diff.unQValue))
                  . L.over L._4 (maybe "NA" (showt . Diff.unPValue))
                  . L.over L._3 Diff.unLog2Diff
                  . L.over L._2 unFeature
                  )
           $ xs

-- | Get the differential expression string between two sets of nodes using
-- KruskalWallis.
getDEStringKruskalWallis
  :: [(Feature, Diff.Log2Diff, Maybe Diff.PValue, Maybe Diff.FDR, Maybe Diff.QValue)]
  -> B.ByteString
getDEStringKruskalWallis xs = header <> "\n" <> body
  where
    header = "feature,log2FC,pVal,qVal"
    body   = CSV.encode
           . fmap ( (\(!a, !b, !c, !d, !e) -> (a,b,c,e))
                  . L.over L._5 (maybe "NA" (showt . Diff.unQValue))
                  . L.over L._4 (maybe "NA" (showt . Diff.unFDR))
                  . L.over L._3 (maybe "NA" (showt . Diff.unPValue))
                  . L.over L._2 Diff.unLog2Diff
                  . L.over L._1 unFeature
                  )
           $ xs

-- | Convert a single cell matrix to a list of Entities with the specified
-- features. Also aggregates features by average value or not.
scToEntities :: Aggregate
             -> [Feature]
             -> [(Int, Cell, (Int, Diff.Status))]
             -> SingleCells
             -> [Diff.Entity]
scToEntities aggregate features cellGroups sc =
    concatMap (\x -> toEntity aggregate x featureIdxs) cellGroups
  where
    mat = getMatrix sc
    toEntity (Aggregate False) (cellIdx, (Cell b), (_, status)) =
      fmap (\ (Feature feature, idx) -> Diff.Entity (Diff.Name feature) status (Diff.Id b)
                                $ S.lookupWD_SM mat (cellIdx, idx)
           )
    toEntity (Aggregate True) (cellIdx, (Cell b), (_, status)) =
      (:[])
        . Diff.Entity
            (Diff.Name . T.intercalate " " . fmap (unFeature . fst) $ featureIdxs)
            status
            (Diff.Id b)
        . (/ n)
        . foldl' (+) 0
        . fmap (\(_, idx) -> S.lookupWD_SM mat (cellIdx, idx))
    n = genericLength featureIdxs
    featureIdxs :: [(Feature, Int)]
    featureIdxs = fmap (\ !x -> ( x
                             , fromMaybe (err x)
                             $ V.elemIndex (unFeature x) (getColNames sc)
                             )
                    ) features
    err x = error $ "Feature " <> show x <> " not found for differential."

-- | Get the differential expression plot of features (or aggregate of features
-- by average) over statuses, filtered by labels.
getSingleDiff :: Seed
              -> Maybe Subsample
              -> Bool
              -> ViolinFlag
              -> NoOutlierFlag
              -> Aggregate
              -> SeparateNodes
              -> SeparateLabels
              -> Maybe LabelMap
              -> SingleCells
              -> ([G.Node], Maybe (Set.Set Label))
              -> ([G.Node], Maybe (Set.Set Label))
              -> [Feature]
              -> ClusterGraph CellInfo
              -> R.R s (R.SomeSEXP s)
getSingleDiff seed subN normalize (ViolinFlag vf) (NoOutlierFlag noOutlierF) aggregate sn sl lm sc v1 v2 features gr = do
  let splitNodeGroup (!ns, !ls) = fmap (\ !x -> ([x], ls)) ns
      splitLabelGroup (!ns, !ls) =
        maybe
          [(ns, ls)]
          (fmap (\ !l -> (ns, Just $ Set.singleton l)) . Set.toAscList)
          ls
      groupsAssign' = case (unSeparateNodes sn, unSeparateLabels sl) of
                        (False, False) -> [v1, v2]
                        (True, False)  -> concatMap splitNodeGroup [v1, v2]
                        (False, True)  -> concatMap splitLabelGroup [v1, v2]
                        (True, True)  -> concatMap splitNodeGroup
                                       . concatMap splitLabelGroup
                                       $ [v1, v2]

  cellGroups <- H.io $ subsampleGetStatuses seed subN lm groupsAssign' gr

  let entities = scToEntities aggregate features cellGroups sc

  Diff.plotSingleDiff normalize vf noOutlierF entities

-- | Combine nodes and labels.
combineNodesLabels
    :: DiffNodes
    -> Maybe DiffLabels
    -> (([G.Node], Maybe (Set.Set Label)), ([G.Node], Maybe (Set.Set Label)))
combineNodesLabels (DiffNodes (v1, v2)) Nothing = ((v1, Nothing), (v2, Nothing))
combineNodesLabels (DiffNodes (v1, v2)) (Just (DiffLabels (l1, l2))) =
  ((v1, l1), (v2, l2))

-- | Subsample a cell group list with provided subsampling number or the
-- smallest of the two groups if not provided.
subsampleGroups :: Seed
                -> Subsample
                -> [(Int, Cell, (Int, Diff.Status))]
                -> IO [(Int, Cell, (Int, Diff.Status))]
subsampleGroups (Seed seed) subN xs = do
  g <- MWC.restore . MWC.toSeed $ V.fromList [fromIntegral seed]

  let sample n x = fmap (take n . V.toList) . flip MWC.uniformShuffle x
      grouped = fmap V.fromList
              . groupBy ((==) `on` L.view (L._3 . L._1))
              . sortBy (compare `on` L.view (L._3 . L._1))
              $ xs
      subN' = bool (minimum . fmap V.length $ grouped) (unSubsample subN)
            . (/= 0)
            $ unSubsample subN

  fmap mconcat $ mapM (sample subN' g) grouped
