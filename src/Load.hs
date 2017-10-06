{- Load
Gregory W. Schwartz

Collects the functions pertaining to loading the single cell data output from
cellranger.
-}

{-# LANGUAGE BangPatterns #-}

module Load
    ( loadData
    ) where

-- Remote
import Data.Matrix.MatrixMarket (readMatrix, Matrix(RMatrix, IntMatrix))
import Data.Scientific (toRealFloat, Scientific)
import Data.Vector (Vector)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Csv as CSV
import qualified Data.Text as T

-- Local
import Types

-- | Convert a Matrix to a list of lists.
matToListOfList :: Matrix Scientific -> [[Double]]
matToListOfList (RMatrix _ _ _ xs) =
    fmap (\(!x, !y, !z) -> [fromIntegral x, fromIntegral y, toRealFloat z]) xs
matToListOfList (IntMatrix _ _ _ xs) =
    fmap (\(!x, !y, !z) -> [fromIntegral x, fromIntegral y, fromIntegral z]) xs
matToListOfList _ = error "Input matrix is not a Real matrix."

loadData :: MatrixFile -> GeneFile -> CellFile -> IO SingleCells
loadData mf gf cf = do
    m <- fmap matToListOfList
       . readMatrix
       . unMatrixFile
       $ mf
    g <- fmap (\ x -> either error (fmap Gene) ( CSV.decode CSV.NoHeader x
                                       :: Either String (Vector (T.Text, T.Text))
                                        )
              )
       . B.readFile
       . unGeneFile
       $ gf
    c <- fmap (\ x -> either error (fmap (Cell . head)) ( CSV.decode CSV.NoHeader x
                                       :: Either String (Vector [T.Text])
                                        )
              )
       . B.readFile
       . unCellFile
       $ cf

    return $
        SingleCells { matrix   = m
                    , rowNames = g
                    , colNames = c
                    }
