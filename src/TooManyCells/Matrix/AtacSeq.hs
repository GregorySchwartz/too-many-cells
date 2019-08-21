{- TooManyCells.MakeTree.AtacSeq
Gregory W. Schwartz

Collects functions pertaining to converting a feature count matrix to an AtacSeq
matrix, where each feature is a bin range.
-}

{-# LANGUAGE BangPatterns #-}

module TooManyCells.Matrix.AtacSeq
    ( rangeToBin
    , rangeToBinSc
    , binarizeSc
    ) where

-- Remote
import BirchBeer.Types
import Data.Maybe (fromMaybe)
import Data.List (sort, foldl')
import TextShow (showt)
import qualified Control.Lens as L
import qualified Data.Attoparsec.Text as A
import qualified Data.Foldable as F
import qualified Data.IntMap as IMap
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Sparse.Common as S
import qualified Data.Text as T
import qualified Data.Vector as V

-- Local
import TooManyCells.Matrix.Types

-- | Read a range from a text of the form LABEL:LOWERBOUND-UPPERBOUND.
readRange :: RangeIdx -> T.Text -> Range
readRange idx x = either (\ a -> error $ "Error in readRange: " <> a <> ": " <> T.unpack x <> " (did you follow the format LABEL:LOWERBOUND-UPPERBOUND ?)") id
                . A.parseOnly (rangeParser idx)
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
  return $ Range (Just idx) (T.pack l) lb ub

-- | Convert an input range to a bin.
rangeToBin :: BinWidth -> Range -> Bin
rangeToBin (BinWidth b) range@(Range _ l _ _) = Bin Nothing l lb (lb + b)
  where
    lb = (div (midpoint range) b) * b

-- | Find midpoint of a range.
midpoint :: Range -> Int
midpoint (Range _ _ lb ub) = lb + div (ub - lb) 2

-- | Get the map of ranges to their bins, keeping bin order.
binsToRangeBinMapWithOrder :: BinWidth
                           -> V.Vector Range
                           -> (RangeBinMap, V.Vector Bin)
binsToRangeBinMapWithOrder b rs =
  (rangeBinMap, V.fromList . Set.toAscList . Set.fromList $ rangeBins)
  where
    rangeBinMap = RangeBinMap
                . IMap.fromList
                . zip ( fmap (maybe (error "Range missing index in binsToRangeBinMapWithOrder") unRangeIdx . rangeIdx)
                             . V.toList
                             $ rs
                      )
                . fmap (fromMaybe (error "Bin missing index in binsToRangeBinMapWithOrder") . binIdx)
                $ rangeBins
    rangeBins = fmap (\v -> v { binIdx = Map.lookup v binMap }) -- Insert correct idx
              $ bins
    binMap = Map.fromList
           . flip zip (fmap BinIdx [0..])
           . Set.toAscList
           . Set.fromList
           $ bins
    bins = fmap (rangeToBin b)
         . V.toList
         $ rs

-- | Convert a range matrix to a bin matrix.
rangeToBinMat :: RangeBinMap -> MatObsRow -> MatObsRow
rangeToBinMat (RangeBinMap bm) (MatObsRow mat) =
  MatObsRow . foldl' addToMat init . S.toListSM $ mat
  where
    addToMat !m val = m S.^+^ S.fromListSM (S.dimSM init) [rangeToBinVal val]
    init = S.zeroSM (S.nrows mat) . Set.size . Set.fromList . IMap.elems $ bm
    rangeToBinVal all@(!i, !j, !v) = (i, unBinIdx $ rangeToBinCol all, v)
    rangeToBinCol all@(_, !j, _) = IMap.findWithDefault (error $ "Missing range index in rangeToBinMat for: " <> show all)
                                    j
                                    bm

-- | Convert a range SingleCells matrix to a bin SingleCells matrix.
rangeToBinSc :: BinWidth -> SingleCells -> SingleCells
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

-- | Binarize a matrix.
binarizeSc :: SingleCells -> SingleCells
binarizeSc = L.over matrix (MatObsRow . fmap (const 1) . unMatObsRow)
