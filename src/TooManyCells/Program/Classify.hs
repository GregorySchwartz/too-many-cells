{- TooManyCells.Program.Classify
Gregory W. Schwartz

Classify entry point for command line program.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module TooManyCells.Program.Classify where

-- Remote
import BirchBeer.Types (Label (..))
import Control.Monad (unless, when)
import Data.Maybe (fromMaybe)
import Options.Generic
import Safe (headMay)
import Text.Read (readMaybe)
import TextShow (showt)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.FilePath as FP

-- Local
import TooManyCells.Classify.Classify
import TooManyCells.Classify.Types
import TooManyCells.File.Types
import TooManyCells.Matrix.Types
import TooManyCells.Matrix.Utility
import TooManyCells.Program.LoadMatrix
import TooManyCells.Program.Options

-- | Classify path.
classifyMain :: Options -> IO ()
classifyMain opts = do
  let readOrErr err = fromMaybe (error err) . readMaybe
      refFiles' = unHelpful
                . referenceFile
                $ opts
      singleRefMatFlag' = unHelpful . singleReferenceMatrix $ opts
      getRef x = AggReferenceMat
               . aggSc
               . fst
               . fromMaybe (error $ "Could not load file in required field --matrix-path:" <> show x)
             <$> loadAllSSM (opts { matrixPath = Helpful [x] })
      getRefs xs = case (xs, singleRefMatFlag') of
                    ([], _) -> error "No --matrix-path specified"
                    ([x], True) -> fmap (AggReferenceMat . AggSingleCells)
                                 . extractCellsSc
                                 . fst
                                 . fromMaybe (error $ "Could not load file in required field --matrix-path:" <> show x)
                               <$> loadAllSSM (opts { matrixPath = Helpful [x]})
                    (xs, True) -> error "Cannot use --single-reference-matrix with more than one --matrix-path."
                    (xs, False) -> mapM getRef refFiles'

  sc <- maybe (error "Requires --matrix-path") fst <$> loadAllSSM opts
  refs <- getRefs refFiles'

  let classified = classifyCells sc refs
      outputRow (Cell !c, (_, 0)) = T.putStrLn $ c <> ",unknown,0.0"
      outputRow (Cell !c, (Label !l, !s)) = T.putStrLn $ c <> "," <> l <> "," <> showt s

  putStrLn "item,label,score"
  mapM_ outputRow classified
