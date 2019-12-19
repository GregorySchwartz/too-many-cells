{- TooManyCells.Paths.Distance
Gregory W. Schwartz

Collects functions pertaining to calculating distances within the tree (a type
of "pseudotime").
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module TooManyCells.Paths.Distance
    ( linearNodeDistance
    , linearItemDistance
    , labelItemDistance
    ) where

-- Remote
import BirchBeer.Types
import BirchBeer.Utility (getGraphLeavesCycles)
import Control.Monad (join)
import Data.Function (on)
import Data.List (sortBy, genericLength)
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Safe (headMay, atMay)
import qualified Control.Foldl as Fold
import qualified Control.Lens as L
import qualified Data.Foldable as F
import qualified Data.Graph.Inductive as G
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Sparse.Common as S
import qualified Data.Text as T

-- Local
import TooManyCells.MakeTree.Types
import TooManyCells.Paths.Types

-- | Convert path back to root.
pathBackToRoot :: (G.DynGraph gr) => G.Node -> gr a Double -> gr a Double
pathBackToRoot n gr = flip flipPath gr $ G.esp 0 n gr

-- | Flip the path of a graph.
flipPath :: (G.DynGraph gr) => [G.Node] -> gr a b -> gr a b
flipPath [] !gr         = gr
flipPath [n1] !gr       = gr
flipPath (n1:n2:ns) !gr = flipPath ns $ flipEdge n1 n2 gr

-- | Flip an edge in a graph, keeping the label.
flipEdge :: (G.DynGraph gr) => G.Node -> G.Node -> gr a b -> gr a b
flipEdge n1 n2 gr = G.insEdge (n2, n1, label) . G.delEdge (n1, n2) $ gr
  where
    label = maybe (error "No node found on path.") snd
          . F.find ((== n2) . fst)
          $ G.lsuc gr n1

-- | Flip edges.
flipEdges :: G.Node -> G.Context a Double -> G.Context a Double
flipEdges n1 (a1, 0, l, a2) = ((0, n1) : a2, 0, l, removeN a1)
  where
    removeN = filter (\(_, x) -> x /= n1)
flipEdges _ (a1, n, l, a2)  = (a2, n, l, a1)

-- | Get the start of the diameter of a subtree. Can specify which node.
treeDiameterStart
    :: G.Graph gr
    => PathDistance -> G.Node -> gr (G.Node, b) Double -> G.Node
treeDiameterStart pd n gr =
    fst
        . fromMaybe (error "Empty tree when finding diameter.")
        . headMay
        . reverse
        . sortBy (compare `on` snd)
        . fmap (getDistance pd gr n . fst)
        . F.toList
        . getGraphLeavesCycles [] gr
        $ n

-- | Get the shallowest leaf.
shallowLeaf :: (G.Graph gr, Show a)
            => PathDistance -> gr (G.Node, a) Double -> G.Node
shallowLeaf pd gr = fst
                  . fromMaybe (error "No nodes in tree.")
                  . headMay
                  . sortBy (compare `on` snd)
                  . fmap (getDistance pd gr 0 . fst)
                  . F.toList
                  . getGraphLeavesCycles [] gr
                  $ 0

-- | Get the distance of each item from a starting point. Make graph undirected.
linearItemDistance
    :: (TreeItem a, Show a)
    => ShallowFlag -> FlipFlag -> PathDistance -> ClusterGraph a -> [(a, Double)]
linearItemDistance shallow direction pd (ClusterGraph gr) =
    concatMap (uncurry assignItems)
        . linearNodeDistance shallow direction pd
        . G.undir
        . G.emap (fromMaybe 0 . L.view edgeDistance)
        $ gr
  where
    assignItems n v =
        fmap (, v)
            . fromMaybe []
            . fmap (maybe [] F.toList . snd)
            . G.lab gr
            $ n

-- | Flip the distances to reverse direction.
flipDistance :: (Fractional b, Ord b) => [(a, b)] -> [(a, b)]
flipDistance = Fold.fold subtractMaxAll
  where
    subtractMaxAll = (\m -> fmap (subtractMax m))
                 <$> Fold.premap snd Fold.maximum
                 <*> Fold.list
    subtractMax m = L.over L._2 (\x -> abs $ x - fromMaybe 0 m)

-- | Get the distance of each leaf from a starting point.
linearNodeDistance
    :: (G.Graph gr, Show a)
    => ShallowFlag
    -> FlipFlag
    -> PathDistance
    -> gr (G.Node, a) Double
    -> [(G.Node, Double)]
linearNodeDistance (ShallowFlag shallow) (FlipFlag direction) pd gr =
    flipDirection direction
        . fmap (getDistance pd gr (start shallow) . fst)
        . F.toList
        . getGraphLeavesCycles [] gr
        $ 0
  where
    flipDirection True  = id
    flipDirection False = flipDistance
    start True          = shallowLeaf pd gr
    start False         = treeDiameterStart pd 0 gr

-- | Get the distance of a base node n1 to another node n2.
getDistance
    :: (G.Graph gr)
    => PathDistance -> gr a Double -> G.Node -> G.Node -> (G.Node, Double)
getDistance PathStep gr n1 n2 = (n2, genericLength $ G.esp n1 n2 gr)
getDistance PathModularity gr n1 n2 = (n2, fromMaybe 0 $ G.spLength n1 n2 gr)

-- | Label path distances.
labelItemDistance
    :: TreeItem a
    => LabelMap -> [(a, Double)] -> [(Label, Double)]
labelItemDistance (LabelMap lm) =
    fmap (L.over L._1 (flip (Map.findWithDefault (Label "NA") . getId) lm))
