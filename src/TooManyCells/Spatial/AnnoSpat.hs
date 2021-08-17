{- TooManyCells.Spatial.AnnoSpat
Gregory W. Schwartz

Collects functions in the program for using AnnoSpat to label cells with classification.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module TooManyCells.Spatial.AnnoSpat
    ( scToAnnoSpatFile
    , runAnnoSpat
    ) where

-- Remote
import BirchBeer.Types
import Control.Monad (liftM2, liftM3, join)
import Data.Bool (bool)
import Data.List (zipWith3, foldl')
import Data.Maybe (catMaybes, fromMaybe, isJust)
import TextShow (showt)
import TooManyCells.Matrix.Types
import qualified Control.Foldl as Fold
import qualified Control.Lens as L
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Csv as CSV
import qualified Data.Map.Strict as Map
import qualified Data.Sparse.Common as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Text.Printf as TP
import qualified Turtle as TU

-- Local
import qualified TooManyCells.File.Types as Too
import qualified TooManyCells.Spatial.Types as Too

-- | SingleCells to input file for AnnoSpat.
scToAnnoSpatFile ::
  ProjectionMap -> SingleCells -> Too.TempPath -> IO (Maybe (Too.StartCol, Too.EndCol))
scToAnnoSpatFile pm sc (Too.TempPath tmpOut) = do
  let header = V.fromList ["item", "sample"]
            <> (fmap (T.encodeUtf8 . unFeature) . L.view colNames $ sc)
      startEndCols = L.over L._2 Too.EndCol
                   . L.over L._1 Too.StartCol
                   . L.over L.both unFeature
                 <$> ( L.sequenceOf L.both
                     . Fold.fold ((,) <$> Fold.head <*> Fold.last)
                     . L.view colNames
                     $ sc
                     )
      itemOrdered =
        zip (repeat "item") . fmap unCell . V.toList . L.view rowNames $ sc
      sampleOrdered =
        fmap (\ !x -> ("sample",)
                    . maybe "NA" unSample
                    . join
                    . fmap fst
                    . Map.lookup x
                    $ unProjectionMap pm
             )
          . V.toList
          . L.view rowNames
          $ sc

  BL.writeFile (T.unpack . TU.format TU.fp $ tmpOut)
    . CSV.encodeByName header
    . zipWith3 (\x y zs -> Map.fromList $ x:y:zs) itemOrdered sampleOrdered
    . fmap (zip (V.toList . getColNames $ sc) . fmap showt . S.toDenseListSV)
    . S.toRowsL
    . getMatrix
    $ sc

  pure startEndCols

-- | Run AnnoSpat to get cell labels.
runAnnoSpat :: Too.AnnoSpatCommand
            -> Too.TempPath
            -> Too.AnnoSpatMarkerFile
            -> Too.OutputDirectory
            -> Too.StartCol
            -> Too.EndCol
            -> IO (Maybe LabelFile)
runAnnoSpat defCmd tmpInput mf outDir startCol endCol = TU.reduce Fold.head $ do
  let cmd = T.pack
          $ TP.printf
              (Too.unAnnoSpatCommand defCmd)
              (T.unpack . TU.format TU.fp $ Too.unTempPath tmpInput)
              (Too.unAnnoSpatMarkerFile mf)
              (Too.unOutputDirectory outDir)
              (T.unpack $ Too.unStartCol startCol)
              (T.unpack $ Too.unEndCol endCol)
              ("sample" :: String)

  TU.mktree . TU.fromText . T.pack $ Too.unOutputDirectory outDir
  TU.stderr . TU.inshell cmd $ mempty

  labelsFile <- fmap (LabelFile . T.unpack . TU.format TU.fp)
              . TU.single
              . TU.findtree (TU.invert $ TU.has "numericLabels")
              . TU.find (TU.has "trte_labels_")
              . TU.fromText
              . T.pack
              $ Too.unOutputDirectory outDir

  pure labelsFile
