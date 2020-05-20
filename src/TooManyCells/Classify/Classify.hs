{- TooManyCells.Classify.Classify
Gregory W. Schwartz

Collects functions pertaining to classifying cells from reference populations.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module TooManyCells.Classify.Classify
    ( classifyCells
    , classifyMat
    ) where

-- Remote
import BirchBeer.Types (getMatrix, Label (..))
import Data.Function (on)
import Data.List (maximumBy)
import qualified Control.Lens as L
import qualified Data.Sparse.Common as S
import qualified Data.Vector as V
import qualified Math.Clustering.Spectral.Sparse as Spectral

-- Local
import TooManyCells.Matrix.Types
import TooManyCells.Matrix.Utility
import TooManyCells.MakeTree.Adjacency

-- | Classify cells in a SingleCells type from a list of references.
classifyCells :: SingleCells -> [AggReferenceMat] -> [(Cell, (Label, Double))]
classifyCells sc refs = zip (V.toList . L.view rowNames $ sc')
                      . fmap (getMaxLabel . zip refNames . S.toDenseListSV)
                      . S.toRowsL
                      $ classifyMat sc' refs'
  where
    (sc', refs') = unifyFeatures sc refs
    refNames = fmap (Label . unCell)
             . V.toList
             . L.view rowNames
             . unAggSingleCells
             $ unAggReferenceMat refs'
    getMaxLabel = maximumBy (compare `on` snd)

-- | Classify matrices using cosine similarity. Results in a matrix with scores
-- (cosine similarity) for each column and observations per row.
classifyMat :: SingleCells -> AggReferenceMat -> S.SpMatrix Double
classifyMat sc refs = scMat S.## S.transpose refMat
  where
    refMat = normScMat . unAggSingleCells . unAggReferenceMat $ refs
    scMat = normScMat sc
    normScMat = Spectral.unB . Spectral.b2ToB . Spectral.B2 . getMatrix

-- | Force unification of features with reference features.
unifyFeatures :: SingleCells -> [AggReferenceMat] -> (SingleCells, AggReferenceMat)
unifyFeatures sc refs = (newSc, newRefs)
  where
    newSc = L.over rowNames (V.take nRowsSc)
          . L.over matrix (extractObsRows 0 (nRowsSc - 1))
          $ unified
    newRefs = AggReferenceMat
            . AggSingleCells
            . L.over rowNames (V.drop nRowsSc)
            . L.over matrix (extractObsRows nRowsSc (nRows - 1))
            $ unified
    nRefs = length refs
    (nRows, nCols) = S.dim . getMatrix $ unified
    (nRowsSc, nColsSc) = S.dim . getMatrix $ sc
    unified = mconcat
            $ sc : fmap (unAggSingleCells . unAggReferenceMat) refs
    extractObsRows lb ub (MatObsRow x) =
      MatObsRow $ S.extractSubmatrixRebalanceKeys x (lb, ub) (0, nCols - 1)

-- | Get the name of a reference from an AggReferenceMat.
getRefName :: AggReferenceMat -> Label
getRefName = maybe (error "unifyFeatures: no reference data") (Label . unCell)
           . flip (V.!?) 0
           . L.view rowNames
           . unAggSingleCells
           . unAggReferenceMat

-- | Get the reference vector from an AggReferenceMat.
getRefVec :: AggReferenceMat -> S.SpVector Double
getRefVec = flip S.extractRow 0
          . getMatrix
          . unAggSingleCells
          . unAggReferenceMat
