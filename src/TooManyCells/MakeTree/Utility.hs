{- TooManyCells.MakeTree.Utility
Gregory W. Schwartz

Collects helper functions in the program.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}

module TooManyCells.MakeTree.Utility
    ( matToRMat
    , scToRMat
    , getMostFrequent
    , getFractions
    , isTo
    , sparseToHMat
    , hToSparseMat
    , matToHMat
    , matToSpMat
    , spMatToMat
    , dendrogramToGraph
    , getGraphLeaves
    , getGraphLeavesWithParents
    ) where

-- Remote
import Control.Monad.State (MonadState (..), State (..), evalState, execState, modify)
import Data.Function (on)
import Data.List (maximumBy)
import Data.Maybe (fromMaybe)
import Data.Matrix.MatrixMarket (Matrix(RMatrix, IntMatrix), Structure (..))
import Data.Scientific (toRealFloat, Scientific)
import Language.R as R
import Language.R.QQ (r)
import qualified Control.Lens as L
import qualified Data.Clustering.Hierarchical as HC
import qualified Data.Graph.Inductive as G
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Sparse.Common as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as H

-- Local
import TooManyCells.MakeTree.Types

-- | Convert a mat to an RMatrix.
matToRMat :: MatObsRow -> R s (RMatObsRow s)
matToRMat (MatObsRow m) = do
    [r| library(jsonlite) |]

    let mString = show . H.toLists . sparseToHMat $ m

    -- We want rows as observations and columns as features.
    mat <- [r| as.matrix(fromJSON(mString_hs)) |]
    return . RMatObsRow $ mat

-- | Convert a sc structure to an RMatrix.
scToRMat :: SingleCells MatObsRow -> R s (RMatObsRow s)
scToRMat sc = do
    [r| library(Matrix) |]

    let rowNamesR = fmap (T.unpack . unCell) . V.toList . rowNames $ sc
        colNamesR = fmap (T.unpack . unGene) . V.toList . colNames $ sc

    mat <- fmap unRMatObsRow . matToRMat . matrix $ sc

    -- namedMat <- [r| rownames(mat_hs) = rowNamesR_hs
    --                 colnames(mat_hs) = colNamesR_hs
    --             |]

    return . RMatObsRow $ mat

-- | Get the most frequent element of a list.
getMostFrequent :: (Eq a, Ord a) => [a] -> a
getMostFrequent = fst
                . maximumBy (compare `on` snd)
                . Map.toAscList
                . Map.fromListWith (+)
                . flip zip ([1,1..] :: [Double])
                
-- | Get the fractions of each element in a list.
getFractions :: (Eq a, Ord a) => [a] -> [(a, Double)]
getFractions xs = Map.toAscList . Map.map (/ total) $ countMap
  where
    total    = Map.foldl' (+) 0 countMap
    countMap = Map.fromListWith (+) . flip zip ([1,1..] :: [Double]) $ xs

-- | Basic arithmetic to convert ratios. "x is to y as a is to b" in the order
-- of xIsToYAs x y a.
isTo :: Double -> Double -> Double -> Double
isTo x y a = a / (x / y)

-- | Convert a sparse matrix to an hmatrix.
sparseToHMat :: S.SpMatrix Double -> H.Matrix H.R
sparseToHMat mat = H.assoc (S.dimSM mat) 0
                 . fmap (\(!x, !y, !z) -> ((x, y), z))
                 . S.toDenseListSM
                 $ mat

-- | Convert a sparse matrix to an hmatrix.
hToSparseMat :: H.Matrix H.R -> S.SpMatrix Double
hToSparseMat =
    S.transposeSM . S.sparsifySM . S.fromColsL . fmap S.vr . H.toLists

-- | Convert a Matrix to an hmatrix Matrix. Assumes matrix market is 1 indexed.
matToHMat :: Matrix Scientific -> H.Matrix H.R
matToHMat (RMatrix size _ _ xs) =
    H.assoc size 0
        . fmap (\(!x, !y, !z) -> ((fromIntegral x - 1, fromIntegral y - 1), toRealFloat z))
        $ xs
matToHMat (IntMatrix size _ _ xs) =
    H.assoc size 0
        . fmap (\(!x, !y, !z) -> ((fromIntegral x - 1, fromIntegral y - 1), fromIntegral z))
        $ xs
matToHMat _ = error "Input matrix is not a Real matrix."

-- | Convert a Matrix to a sparse matrix.
matToSpMat :: Matrix Scientific -> S.SpMatrix Double
matToSpMat (RMatrix size _ _ xs) =
    S.fromListSM size
        . fmap (\(!x, !y, !z) -> (fromIntegral x - 1, fromIntegral y - 1, toRealFloat z))
        $ xs
matToSpMat (IntMatrix size _ _ xs) =
    S.fromListSM size
        . fmap (\(!x, !y, !z) -> (fromIntegral x - 1, fromIntegral y - 1, fromIntegral z))
        $ xs
matToSpMat _ = error "Input matrix is not a Real matrix."

-- | Convert a sparse matrix to a Matrix.
spMatToMat :: S.SpMatrix Double -> Matrix Double
spMatToMat mat = RMatrix (S.dimSM mat) (S.nzSM mat) General
               . fmap (\(!x, !y, !z) -> (x + 1, y + 1, z)) . S.toListSM
               $ mat

-- | Convert a dendrogram with height as accumulating Q values to a graph for
-- plotting with leaves containing all information. This also means that the
-- node must be in the label as well.
dendrogramToGraph
    :: HC.Dendrogram (V.Vector CellInfo)
    -> CellGraph
dendrogramToGraph =
    CellGraph
        . snd
        . flip execState (0, G.empty)
        . go
  where
    go :: HC.Dendrogram (V.Vector CellInfo)
       -> State (Int, G.Gr (G.Node, Maybe (Seq.Seq CellInfo)) HC.Distance) Int
    go (HC.Branch d l r) = do
        (n, gr) <- get
        modify (L.over L._1 (+ 1))

        l <- go l
        r <- go r

        let setGr = G.insEdge (n, r, d)
                  . G.insEdge (n, l, d)
                  . G.insNode (n, (n, Nothing))

        modify (L.over L._2 setGr)

        return n
    go (HC.Leaf cells) = do
        (n, gr) <- get

        modify (L.over L._1 (+ 1) . L.over L._2 (G.insNode (n, (n, Just . Seq.fromList . V.toList $ cells))))

        return n

-- | Get leaves of a tree graph given a node. Graph must not include cycles!
getGraphLeaves :: G.Graph gr => gr a b -> G.Node -> Seq.Seq a
getGraphLeaves gr n =
    case G.suc gr n of
        [] -> Seq.singleton
            . fromMaybe (error "Node has no label.")
            . G.lab gr
            $ n
        xs -> mconcat . fmap (getGraphLeaves gr) $ xs

-- | Get leaves of a tree graph given a node with a breadcrumb trail of parent
-- node IDs. The first element in the cluster list is the node the item belongs
-- to, all the way to the root (last element in the list). Graph must not
-- include cycles!
getGraphLeavesWithParents
    :: G.Graph gr
    => gr a b -> G.Node -> Seq.Seq ([G.Node], a)
getGraphLeavesWithParents gr root = go [] root
  where
    go !acc n =
        case G.suc gr n of
            [] -> Seq.singleton
                . (n:acc,)
                . fromMaybe (error "Node has no label.")
                . G.lab gr
                $ n
            xs -> mconcat . fmap (go (n:acc)) $ xs
