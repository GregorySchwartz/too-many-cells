{- TooManyCells.MakeTree.AtacSeq
Gregory W. Schwartz

Collects functions pertaining to converting a feature count matrix to an AtacSeq
matrix, where each feature is a bin range.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module TooManyCells.Matrix.AtacSeq
    ( rangeToBin
    , rangeToBinSc
    , binarizeSc
    ) where

-- Remote
import BirchBeer.Types
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Data.List (sort, foldl')
import TextShow (showt)
import qualified Control.Lens as L
import qualified Data.Attoparsec.Text as A
import qualified Data.Foldable as F
import qualified Data.IntMap as IMap
import qualified Data.IntervalMap.Interval as I
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Sparse.Common as S
import qualified Data.Text as T
import qualified Data.Vector as V

-- Local
import TooManyCells.Matrix.Types

-- | Convert an input range to a bin.
rangeToBin :: BinWidth -> ChrRegion -> ChrRegionBin
rangeToBin (BinWidth !b) region@(ChrRegion (!l, _)) =
  ChrRegionBin $ ChrRegion (l, I.ClosedInterval lb (lb + b - 1))
  where
    lb = ((div (midpoint region) b) * b) + 1

-- | Find midpoint of a range.
midpoint :: ChrRegion -> Int
midpoint (ChrRegion (_, !i)) =
  I.lowerBound i + div (I.upperBound i - I.lowerBound i) 2

-- | Get the map of ranges to their bins, keeping bin order.
binsToRangeBinMapWithOrder :: BinWidth
                           -> [ChrRegion]
                           -> (RangeBinMap, [ChrRegionBin])
binsToRangeBinMapWithOrder b rs =
  (rangeBinMap, Set.toAscList . Set.fromList $ bins)
  where
    rangeBinMap = RangeBinMap . IMap.fromList . zip [0..] . fmap fst $ rangeBins
    rangeBins = fmap (\ !v -> (Map.findWithDefault (error "Bin missing index in binsToRangeBinMapWithOrder") v binMap, v)) bins  -- Insert correct idx
    binMap = Map.fromList
           . flip zip (fmap BinIdx [0..])
           . Set.toAscList
           . Set.fromList
           $ bins
    bins = fmap (rangeToBin b) rs

-- | Convert a range matrix to a bin matrix.
rangeToBinMat :: RangeBinMap -> MatObsRow -> MatObsRow
rangeToBinMat (RangeBinMap bm) (MatObsRow mat) =
  MatObsRow . foldl' addToMat init . S.toListSM $ mat
  where
    addToMat !m val = m S.^+^ S.fromListSM (S.dimSM init) [rangeToBinVal val] -- Possible space leak with ^+^
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
              , _colNames =
                  V.fromList . fmap (Feature . showt . unChrRegionBin) $ bins
              }
  where
    (rangeBinMap, bins) =
      binsToRangeBinMapWithOrder b
        . fmap ( either (error . (<>) "Cannot parse region format `chrN:START-END` in: ") id
               . parseChrRegion
               . unFeature
               )
        . V.toList
        . L.view colNames
        $ sc

-- | Binarize a matrix.
binarizeSc :: SingleCells -> SingleCells
binarizeSc = L.over matrix (MatObsRow . fmap (bool 0 1 . (> 0)) . unMatObsRow)
