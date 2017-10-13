{- Load
Gregory W. Schwartz

Collects the functions pertaining to loading the single cell data output from
cellranger.
-}

{-# LANGUAGE BangPatterns #-}

module Load
    ( loadCellrangerData
    , loadMatrixData
    ) where

-- Remote
import Data.Char (ord)
import Data.Matrix.MatrixMarket (readMatrix, Matrix(RMatrix, IntMatrix))
import Data.Maybe (fromMaybe)
import Data.Scientific (toRealFloat, Scientific)
import Data.Vector (Vector)
import qualified Numeric.LinearAlgebra as H
import Safe
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Csv as CSV
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Vector as V

-- Local
import Types

-- | Convert a Matrix to a list of lists.
matToListOfList :: Matrix Scientific -> H.Matrix H.R
matToListOfList (RMatrix size _ _ xs) =
    H.assoc size 0
        . fmap (\(!x, !y, !z) -> ((fromIntegral x, fromIntegral y), toRealFloat z))
        $ xs
matToListOfList (IntMatrix size _ _ xs) =
    H.assoc size 0
        . fmap (\(!x, !y, !z) -> ((fromIntegral x, fromIntegral y), fromIntegral z))
        $ xs
matToListOfList _ = error "Input matrix is not a Real matrix."

loadCellrangerData :: MatrixFile -> GeneFile -> CellFile -> IO SingleCells
loadCellrangerData mf gf cf = do
    m <- fmap matToListOfList
       . readMatrix
       . unMatrixFile
       $ mf
    g <- fmap (\ x -> either error (fmap (Gene . fst)) ( CSV.decode CSV.NoHeader x
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
        SingleCells { matrix   = MatObsRow . H.tr $ m -- We want observations as rows.
                    , rowNames = g
                    , colNames = c
                    }

loadMatrixData :: Delimiter -> MatrixFile -> IO SingleCells
loadMatrixData (Delimiter delim) mf = do
    let csvOpts = CSV.defaultDecodeOptions { CSV.decDelimiter = fromIntegral (ord delim) }

    all <- fmap (\ x -> either error id ( CSV.decodeWith csvOpts CSV.NoHeader x
                                       :: Either String (Vector (Vector T.Text))
                                        )
                )
         . B.readFile
         . unMatrixFile
         $ mf

    let c = fmap Cell . V.drop 1 . V.head $ all
        g = fmap (Gene . V.head) . V.drop 1 $ all
        m = fmap (fmap (either error fst . T.double) . drop 1 . V.toList)
          . drop 1
          . V.toList
          $ all

    return $
        SingleCells { matrix   = MatObsRow . H.tr . H.fromLists $ m -- We want observations as rows
                    , rowNames = g
                    , colNames = c
                    }
