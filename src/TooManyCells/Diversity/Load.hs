{- TooManyCells.Diversity.Load
Gregory W. Schwartz

Collects the functions pertaining to the loading of data.
-}

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

module TooManyCells.Diversity.Load
    ( loadPopulation
    ) where

-- Remote
import BirchBeer.Types
import Control.Monad.Except (runExceptT, ExceptT (..))
import Control.Monad.Managed (with, liftIO, Managed (..))
import Data.Matrix.MatrixMarket (readMatrix)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Tuple (swap)
import Math.Clustering.Hierarchical.Spectral.Sparse (B (..))
import Safe (headMay)
import TextShow (showt)
import qualified Control.Lens as L
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Streaming.Char8 as BS
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Streaming as S
import qualified Streaming.Cassava as S
import qualified Streaming.Prelude as S
import qualified Streaming.With.Lifted as SW
import qualified System.Directory as FP
import qualified System.FilePath as FP

-- Local
import TooManyCells.Diversity.Types
import TooManyCells.File.Types
import TooManyCells.MakeTree.Types
import TooManyCells.Matrix.Utility
import TooManyCells.Matrix.Types

-- | Load a population representation from a cluster file
loadPopulationCsv :: PriorPath -> IO (Either String Population)
loadPopulationCsv file = do
    let getCols :: Map.Map T.Text T.Text -> Either String (Species, Seq.Seq Cell)
        getCols m = do
            x <- do
                    maybe (Left "\nNo cluster column.") (Right . Species)
                    . Map.lookup "cluster"
                    $ m
            y <- do
                    maybe
                        (Left "\nNo cell column.")
                        (Right . Seq.singleton . Cell)
                    . Map.lookup "cell"
                    $ m

            return (x, y)

    flip with return $ do
        contents   <- SW.withBinaryFileContents . unPriorPath $ file
        population <- fmap (\x -> do
                                x' <- either (Left . show) Right x
                                fmap (Population . Map.fromListWith (Seq.><))
                                    . sequence
                                    $ x'
                           )
                    . runExceptT
                    . S.toList_
                    . S.map getCols
                    . S.decodeByName
                    $ (contents :: BS.ByteString (ExceptT S.CsvParseException Managed) ())

        return population

-- | Convert previous cluster results to a population representation.
priorToPopulation :: [(CellInfo, [Cluster])] -> Either String Population
priorToPopulation =
    fmap (Population . Map.fromListWith (Seq.><))
        . mapM (\(!x, !y) -> do
                    let x' = Seq.singleton . _barcode $ x
                    y' <- maybe
                            (Left "\nNo cluster for cell.")
                            (Right . Species . showt . unCluster)
                        . headMay
                        $ y
                    return (y', x')
               )

-- | Convert a Population to a custom population with a label map.
popToLabelPop :: LabelMap -> Population -> Either String Population
popToLabelPop (LabelMap lm) = fmap (Population . Map.fromListWith (Seq.><))
                            . mapM getPopEntry
                            . F.toList
                            . mconcat
                            . Map.elems
                            . unPopulation
  where
    getPopEntry (Cell x) =
      ( maybe
            (Left $ "\nMissing label for " <> (show x))
            (Right . Species . unLabel)
            (Map.lookup (Id x) lm)
      )
        >>= Right . (, Seq.singleton . Cell $ x)

-- | Load the populations from a list of files. If a label map is provided, the
-- species are the labels, otherwise they are the clusters.
loadPopulation :: Maybe LabelMap
               -> PriorPath
               -> IO (Either String Population)
loadPopulation lm (PriorPath path) = do
    dirExist <- FP.doesDirectoryExist path
    fileExist <- FP.doesFileExist path
    res <- case (dirExist, fileExist) of
        (False, False) -> return $ Left "\nInput does not exist."
        (_, True)      -> runExceptT $ do
            pop <- ExceptT . loadPopulationCsv . PriorPath $ path
            return pop
        otherwise      -> runExceptT $ do
            let crInput = path FP.</> "cluster_list.json"

            pop <- ExceptT
                 . fmap ((=<<) priorToPopulation . A.eitherDecode)
                 . B.readFile
                 $ crInput

            return pop

    return . maybe res (\x -> res >>= popToLabelPop x) $ lm
