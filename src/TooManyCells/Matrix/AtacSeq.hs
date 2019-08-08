{- TooManyCells.MakeTree.AtacSeq
Gregory W. Schwartz

Collects functions pertaining to converting a feature count matrix to an AtacSeq
matrix, where each feature is a bin range.
-}

{-# LANGUAGE BangPatterns #-}

module TooManyCells.Matrix.AtacSeq
    ( rangeToBinSc
    ) where

-- Remote
import BirchBeer.Types
import Data.Maybe (fromMaybe)
import Data.List (sort, foldl')
import TextShow (showt)
import qualified Control.Lens as L
import qualified Data.Attoparsec.Text as A
import qualified Data.Foldable as F
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Sparse.Common as S
import qualified Data.Text as T
import qualified Data.Vector as V

-- Local
import TooManyCells.Matrix.Types

-- | Read a range from a text of the form LABEL:LOWERBOUND-UPPERBOUND.
readRange :: RangeIdx -> T.Text -> Range
readRange idx x = either (\ a -> error $ "Error reading " <> a <> " : " <> T.unpack x <> " (did you follow the format LABEL:LOWERBOUND-UPPERBOUND ?)") id
                . A.eitherResult
                . A.parse (rangeParser idx)
                . T.filter (/= ',')
                $ x

-- | How to parse a range.
rangeParser :: RangeIdx -> A.Parser Range
rangeParser idx = do
  l <- A.many1 (A.notChar ':')
  A.char ':'
  lb <- A.decimal
  A.char '-'
  ub <- A.decimal
  return $ Range idx (T.pack l) lb ub

-- | Convert an input range to a bin.
rangeToBin :: BinSize -> Range -> Bin
rangeToBin (BinSize b) range@(Range _ l _ _) = Bin Nothing l lb (lb + b)
  where
    lb = (div (midpoint range) b) * b

-- | Find midpoint of a range.
midpoint :: Range -> Int
midpoint (Range _ _ lb ub) = lb + div (ub - lb) 2

-- | Get the map of ranges to their bins, keeping bin order.
binsToRangeBinMapWithOrder :: BinSize
                           -> V.Vector Range
                           -> (RangeBinMap, V.Vector Bin)
binsToRangeBinMapWithOrder b rs = (rangeBinMap, V.fromList bins)
  where
    rangeBinMap = RangeBinMap
                . Map.fromList
                . zip (fmap rangeIdx . V.toList $ rs)
                . fmap (fromMaybe (error "Bin missing index in binsToRangeBinMapWithOrder") . binIdx)
                $ bins
    bins = zipWith (\i v -> v { binIdx = Just i }) (fmap BinIdx [0..]) -- Insert correct idx
         . sort
         . fmap (rangeToBin b)
         . V.toList
         $ rs

-- | Convert a range matrix to a bin matrix.
rangeToBinMat :: RangeBinMap -> MatObsRow -> MatObsRow
rangeToBinMat (RangeBinMap bm) (MatObsRow mat) =
  MatObsRow . foldl' addToMat init . S.toListSM $ mat
  where
    addToMat m val = S.fromListSM (S.dimSM init) [val] S.^+^ m
    init = S.zeroSM (S.nrows mat) . Set.size . Set.fromList . Map.elems $ bm

-- | Convert a range SingleCells matrix to a bin SingleCells matrix.
rangeToBinSc :: BinSize -> SingleCells -> SingleCells
rangeToBinSc b sc =
  SingleCells { _matrix = rangeToBinMat rangeBinMap . L.view matrix $ sc
              , _rowNames = L.view rowNames sc
              , _colNames = fmap (Feature . showt) bins
              }
  where
    (rangeBinMap, bins) =
      binsToRangeBinMapWithOrder b
        . V.fromList
        . fmap (\(!i, !v) -> readRange (RangeIdx i) . unFeature $ v)
        . zip [0..]
        . V.toList
        . L.view colNames
        $ sc
