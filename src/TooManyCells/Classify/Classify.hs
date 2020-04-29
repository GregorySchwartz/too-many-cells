{- TooManyCells.Classify.Classify
Gregory W. Schwartz

Collects functions pertaining to classifying cells from reference populations.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module TooManyCells.Classify.Classify
    ( classifyCells
    ) where

-- Remote
import BirchBeer.Types (Label (..))
import Data.Function (on)
import Data.List (maximumBy)
import qualified Control.Lens as L
import qualified Data.Sparse.Common as S
import qualified Data.Vector as V

-- Local
import TooManyCells.Matrix.Types
import TooManyCells.Matrix.Utility
import TooManyCells.MakeTree.Adjacency

-- | Classify cells in a SingleCells type from a list of references.
classifyCells :: SingleCells -> [AggReferenceMat] -> [(Cell, (Double, Label))]
classifyCells sc refs = fmap (\ !x -> (x, classifyCell sc' refs' x))
                      . V.toList
                      $ cells
  where
    cells = L.view rowNames sc'
    (sc', refs') = unifyFeatures sc refs

-- | Classify a cell in a SingleCells type from a list of references.
classifyCell :: SingleCells -> [AggReferenceMat] -> Cell -> (Double, Label)
classifyCell sc refs cell =
  maximumBy (compare `on` fst)
    . fmap (\ !ref -> (compareCell sc ref cell, getRefName ref))
    $ refs

-- | Get a correlation score between a cell and a reference.
compareCell :: SingleCells -> AggReferenceMat -> Cell -> Double
compareCell cell ref sc = cosineSimilaritySparse c r
  where
    c = extractCellV sc cell
    r = flip S.extractRow 0
      . unMatObsRow
      . L.view matrix
      . unAggSingleCells
      . unAggReferenceMat
      $ ref

-- | Force unification of features with reference features.
unifyFeatures :: SingleCells -> [AggReferenceMat] -> (SingleCells, [AggReferenceMat])
unifyFeatures sc refs = (newSc, newRefs)
  where
    newSc = removeCellsSc (fmap (Cell . unLabel . getRefName) refs) sc
    newRefs = AggReferenceMat
            . AggSingleCells
            . flip extractCellSc unified
            . Cell
            . unLabel
            . getRefName
          <$> refs
    unified =
      mconcat . reverse $ sc : fmap (unAggSingleCells . unAggReferenceMat) refs

-- | Get the name of a reference from an AggReferenceMat.
getRefName :: AggReferenceMat -> Label
getRefName = maybe (error "unifyFeatures: no reference data") (Label . unCell)
           . flip (V.!?) 0
           . L.view rowNames
           . unAggSingleCells
           . unAggReferenceMat
