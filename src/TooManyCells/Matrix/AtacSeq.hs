{- TooManyCells.MakeTree.AtacSeq
Gregory W. Schwartz

Collects functions pertaining to converting a feature count matrix to an AtacSeq
matrix, where each feature is a bin range.
-}

module TooManyCells.Matrix.AtacSeq
    ( binRanges
    ) where

-- Remote
import TextShow (showt)
import qualified Control.Lens as L
import qualified Data.Attoparsec.Text as A
import qualified Data.Foldable as F
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Sparse.Common as S
import qualified Data.Text as T
import qualified Data.Vector as V

-- Local
import TooManyCells.Matrix.Types

-- | Read a range from a text of the form LABEL:LOWERBOUND-UPPERBOUND.
readRange :: T.Text -> Range
readRange x = either (\ a -> error $ "Error reading " <> a <> " : " <> x <> " (did you follow the format LABEL:LOWERBOUND-UPPERBOUND ?)")
            . A.eitherResult
            . A.parse rangeParser
            $ x

-- | How to parse a range.
rangeParser :: A.Parser Range
rangeParser = do
  l <- A.many1 (A.notChar ':')
  char ':'
  lb <- A.decimal
  char '-'
  ub <- A.decimal
  return $ Range l lb ub

-- | Convert an input range to a bin.
peakToBin :: BinSize -> Range -> Bin
peakToBin (BinSize b) range@(Range l _ _) = Bin l b1 (b1 + b)
  where
    bl = (div (midpoint range) b) * b

-- | Find midpoint of a range.
midpoint :: Range -> Int
midpoint (Range l u) = l + div (u - l) 2

-- | Get the map of bins to their idxs in the input range vector, no duplicates
-- in bins.
binsToBinRangeMap :: BinSize -> V.Vector Range -> BinRangeMap
binsToBinRangeMap b = BinRangeMap
                    . Map.fromListWith (Seq.><)
                    . flip (zip (fmap Seq.singleton [0,1..]))
                    . fmap (peakToBin b)
                    $ V.toList

-- | Convert a range matrix to a bin matrix.
rangeToBinMat :: BinRangeMap -> MatObsRow -> V.Vector Bin -> MatObsRow
rangeToBinMat (BinRangeMap bm) (MatObsRow mat) =
  MatObsRow . S.fromColsV . fmap getBinRow
  where
    getBinRow = sum
              . fmap (S.extractCol mat)
              . F.toList
              . (Map.findWithDefault err) bm
    err x = error $ "Cannot find " <> x <> " in BinRangeMap."

-- | Convert a range SingleCells matrix to a bin SingleCells matrix.
rangeToBinSc :: BinSize -> SingleCells -> SingleCells
rangeToBinSc b sc =
  SingleCells { _matrix = rangeToBinMat
                            binMap
                            (L.view matrix sc)
                            (V.fromList . Map.keys . unBinRangeMap $ binMap)
              , _rowNames = L.view rowNames sc
              , _colNames = V.fromList
                          . fmap (Feature . showt)
                          . Map.keys
                          . unBinRangeMap
                          $ binMap
              }
  where
    binMap =
      binsToBinRangeMap b . fmap (readRange . unFeature) . L.view colNames $ sc
