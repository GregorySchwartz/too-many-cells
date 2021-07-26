{- TooManyCells.Spatial.Utility
Gregory W. Schwartz

Collects helper functions in the program for spatial analysis.
-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module TooManyCells.Spatial.Utility
    ( scToVLData
    , subsampleProjectionMap
    , scToRPat
    , markToText
    ) where

-- Remote
import BirchBeer.Types
import Control.Monad (liftM2, liftM3)
import Data.Bool (bool)
import Data.List (zipWith3, foldl')
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Language.R as R
import Language.R.QQ (r)
import TooManyCells.Matrix.Types
import qualified Control.Lens as L
import qualified Data.Map.Strict as Map
import qualified Data.Sparse.Common as S
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Graphics.Vega.VegaLite as VL

-- Local
import TooManyCells.Matrix.Utility (sparseMatToSparseRMat, subsetCellsSc)
import TooManyCells.Spatial.Types (Mark (..))

-- | SingleCells to Vega Data
scToVLData :: Maybe LabelMap -> ProjectionMap -> SingleCells -> VL.Data
scToVLData lm pm sc =
  VL.dataFromRows []
    . foldl' (\acc x -> VL.dataRow x acc) []
    . catMaybes  -- remove missing projections
    . joinAll  -- required
        projectionOrdered
        (fmap (Just . ("item",) . VL.Str) . V.toList . getRowNames $ sc)
    . fmap Just
    . maybe id (\x -> zipWith (:) (labelOrdered x)) lm  -- optional
    . fmap (zip (V.toList . getColNames $ sc) . fmap VL.Number . S.toDenseListSV)
    . S.toRowsL
    . getMatrix
    $ sc
  where
    joinAll :: [Maybe [(T.Text, VL.DataValue)]]
            -> [Maybe (T.Text, VL.DataValue)]
            -> [Maybe [(T.Text, VL.DataValue)]]
            -> [Maybe [(T.Text, VL.DataValue)]]
    joinAll = zipWith3 (\xs y z -> liftM3 (\as b c -> (as <> (b:c))) xs y z)
    labelOrdered :: LabelMap -> [(T.Text, VL.DataValue)]
    labelOrdered (LabelMap lm') = V.toList
                                . fmap ( ("label",)
                                       . VL.Str
                                       . unLabel
                                       . fromMaybe (Label "NA")
                                       . flip Map.lookup lm'
                                       . Id
                                       )
                                . getRowNames
                                $ sc
    projectionOrdered = fmap getProjectionInfo . V.toList . L.view rowNames $ sc
    getProjectionInfo :: Cell -> Maybe [(T.Text, VL.DataValue)]
    getProjectionInfo item = Map.lookup item (unProjectionMap pm)
                         >>= pure . projectionToDataValueInfo . snd
    projectionToDataValueInfo :: (X, Y) -> [(T.Text, VL.DataValue)]
    projectionToDataValueInfo (X x, Y y) = [("x", VL.Number x), ("y", VL.Number y)]

-- | Subsample a projection map.
subsampleProjectionMap :: Maybe Sample -> ProjectionMap -> ProjectionMap
subsampleProjectionMap sample = ProjectionMap
                              . Map.filter ((== sample) . fst)
                              . unProjectionMap

-- | SingleCells to easy input for R patterns
scToRPat :: Maybe LabelMap
         -> ProjectionMap
         -> SingleCells
         -> [Mark]
         -> R.R s (R.SomeSEXP s)
scToRPat lm pm sc marks = do
  [r| suppressMessages(library(spatstat)) |]

  let cells  = V.toList . L.view rowNames $ sc
      ps     = fmap (\x -> Map.lookup x $ unProjectionMap pm) cells
      getLabel (MarkFeature _) = error "Expected MarkLabel, not MarkFeature in scToRPat."
      getLabel (MarkLabel l) = l
      labels = Set.fromList . fmap getLabel $ marks
      labelOrOther l =
        bool "Other" (T.unpack . unLabel $ l) . Set.member l $ labels
      ls     =
        fmap
          (\ lm'
          -> fmap
              (\ (Cell x)
              -> labelOrOther
               . Map.findWithDefault (Label "NA") (Id x)
               $ unLabelMap lm'
              )
            . fmap fst
            . catMaybes
            . zipWith (liftM2 (,)) (fmap Just cells)
            $ ps
          )
          lm
      subsampledSc = flip subsetCellsSc sc
                   . catMaybes
                   . zipWith (liftM2 const) (fmap Just cells)
                   $ ps
      isLabel = isJust lm
      xs = fmap (unX . fst) . fmap snd . catMaybes $ ps
      ys = fmap (unY . snd) . fmap snd . catMaybes $ ps

  mat <- fmap unRMatObsRow $ sparseMatToSparseRMat subsampledSc
  pat <- [r|
          ppp(xs_hs, ys_hs, c(min(xs_hs), max(xs_hs)), c(min(ys_hs),max(ys_hs)))
         |]

  case ls of
    Nothing -> do
      let getFeature (MarkFeature f) = T.unpack . unFeature $ f
          getFeature (MarkLabel f) = error "Expected MarkFeature, not MarkLabel in scToRPat."
          features = fmap getFeature marks
      marksDf <- [r| as.data.frame(as.matrix(mat_hs[, features_hs])) |]
      [r| p = pat_hs
          marks(p) = marksDf_hs
          p
      |]
    (Just l) -> [r| p = pat_hs
                    marks(p) = as.factor(l_hs)
                    p
                |]

-- | Get the mark to the text.
markToText (MarkFeature (Feature x)) = x
markToText (MarkLabel (Label x)) = x
