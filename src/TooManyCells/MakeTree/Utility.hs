{- TooManyCells.MakeTree.Utility
Gregory W. Schwartz

Collects helper functions in the program.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}

module TooManyCells.MakeTree.Utility
    ( getMostFrequent
    , getFractions
    , isTo
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
import TooManyCells.Matrix.Types

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

-- | Convert a dendrogram with height as accumulating Q values to a graph for
-- plotting with leaves containing all information. This also means that the
-- node must be in the label as well.
dendrogramToGraph
    :: (TreeItem a)
    => HC.Dendrogram (V.Vector a)
    -> ClusterGraph a
dendrogramToGraph =
    ClusterGraph
        . snd
        . flip execState (0, G.empty)
        . go
  where
    go :: (TreeItem a)
       => HC.Dendrogram (V.Vector a)
       -> State (Int, G.Gr (G.Node, Maybe (Seq.Seq a)) HC.Distance) Int
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
    go (HC.Leaf items) = do
        (n, gr) <- get

        modify (L.over L._1 (+ 1) . L.over L._2 (G.insNode (n, (n, Just . Seq.fromList . V.toList $ items))))

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
