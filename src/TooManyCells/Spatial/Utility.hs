{- TooManyCells.Spatial.Utility
Gregory W. Schwartz

Collects helper functions in the program for spatial analysis.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module TooManyCells.Spatial.Utility
    ( scToVLData
    , subsampleProjectionMap
    ) where

-- Remote
import BirchBeer.Types
import Control.Monad (liftM3)
import Data.List (zipWith3, foldl')
import Data.Maybe (catMaybes, fromMaybe)
import TooManyCells.Matrix.Types
import qualified Control.Lens as L
import qualified Data.Map.Strict as Map
import qualified Data.Sparse.Common as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Graphics.Vega.VegaLite as VL

-- Local

-- | SingleCells to Vega Data
scToVLData
  :: Maybe Sample -> Maybe LabelMap -> ProjectionMap -> SingleCells -> VL.Data
scToVLData sample lm pm sc =
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
