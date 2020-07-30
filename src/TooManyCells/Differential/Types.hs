{- TooManyCells.Differential.Types
Gregory W. Schwartz

Collects the differental types used in the program.
-}

{-# LANGUAGE StrictData #-}

module TooManyCells.Differential.Types where

-- Remote
import BirchBeer.Types
import qualified Data.Graph.Inductive as G
import qualified Data.Set as Set

-- Local

-- Basic
newtype TopN = TopN { unTopN :: Int }
newtype NoEdger = NoEdger { unNoEdger :: Bool }
newtype DiffNodes = DiffNodes {unDiffNodes :: ([G.Node], [G.Node])}
newtype DiffLabels =
  DiffLabels { unDiffLabels :: (Maybe (Set.Set Label), Maybe (Set.Set Label)) }
newtype Aggregate = Aggregate { unAggregate :: Bool }
newtype SeparateNodes = SeparateNodes { unSeparateNodes :: Bool }
newtype SeparateLabels = SeparateLabels { unSeparateLabels :: Bool }
newtype ViolinFlag = ViolinFlag { unViolinFlag :: Bool }
newtype NoOutlierFlag = NoOutlierFlag { unNoOutlierFlag :: Bool }
