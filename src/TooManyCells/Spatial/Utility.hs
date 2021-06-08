{- TooManyCells.Spatial.Utility
Gregory W. Schwartz

Collects helper functions in the program for spatial analysis.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module TooManyCells.Spatial.Utility
    ( scToVLData
    ) where

-- Remote
import BirchBeer.Types
import Control.Maybe (liftM3)
import Data.List (zipWith3)
import Data.Maybe (catMaybes)
import TooManyCells.Matrix.Types
import qualified Data.Map.Strict as Map
import qualified Data.Sparse.Common as S
import qualified Graphics.Vega.VegaLite as VL

-- Local

-- | SingleCells to Vega Data
scToVLData :: Maybe LabelMap -> ProjectionMap -> SingleCells -> VL.Data
scToVLData lm pm sc =
  foldl' (\acc x -> VL.dataRow x . acc) id
    . catMaybes  -- remove missing projections
    . joinAll  -- required
        projectionOrdered
        (fmap (("item",) . VL.Str) . V.toList . getRowNames $ sc)
    . maybe id (\x -> zipWith (:) (labelOrdered x)) lm  -- optional
    . fmap (zip (V.toList . getColNames $ sc) . fmap VL.Number . S.toDenseListSV)
    . S.toRowsL
    . getMatrix
    $ sc
  where
    joinAll = zipWith3 (\x y z -> liftM3 (\a b c -> (a:b:c)) x y z)
    labelOrdered lm' = fmap ( ("label",)
                            . VL.Str
                            . unLabel
                            . fromMaybe (Label "NA")
                            . flip Map.lookup lm'
                            . Id
                            )
                     . getRowNames
                     $ sc
    projectionOrdered = fmap ( getProjectionInfo . Cell) . getRowNames $ sc
    getProjectionInfo :: Cell -> Maybe [(T.Text, VL.DataValue)]
    getProjectionInfo item =
      Map.lookup item pm >>= projectionToDataValueInfo . snd
    projectionToDataValueInfo :: (X, Y) -> [(T.Text, VL.DataValue)]
    projectionToDataValueInfo (X x, Y y) = [("X", VL.Number), ("Y", VL.Number)]
