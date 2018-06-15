{- TooManyCells.Diversity.Load
Gregory W. Schwartz

Collects the functions pertaining to the loading of data.
-}

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module TooManyCells.Diversity.Load
    ( loadPopulation
    ) where

-- Remote
import BirchBeer.Types
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Control.Monad.Except (runExceptT, ExceptT (..))
import Control.Monad.Managed (with, liftIO, Managed (..))
import Data.Matrix.MatrixMarket (readMatrix)
import Math.Clustering.Hierarchical.Spectral.Sparse (B (..))
import Safe (headMay)
import qualified Control.Lens as L
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Streaming.Char8 as BS
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
loadPopulationCsv :: PriorPath -> IO Population
loadPopulationCsv file = do
    let getCols :: Map.Map T.Text T.Text -> (Cluster, Seq.Seq Cell)
        getCols m =
            ( maybe
                (error "\nNo cluster column.")
                (either error (Cluster . fst) . T.decimal)
            . Map.lookup "cluster"
            $ m
            , maybe (error "\nNo cell column.") (Seq.singleton . Cell)
            . Map.lookup "cell"
            $ m
            )

    res <- flip with return $ do
        contents   <- SW.withBinaryFileContents . unPriorPath $ file
        population <- fmap (either (error . show) (Map.fromListWith (Seq.><)))
                    . runExceptT
                    . S.toList_
                    . S.map getCols
                    . S.decodeByName
                    $ (contents :: BS.ByteString (ExceptT S.CsvParseException Managed) ())

        return $ Population population

    return res

-- | Convert previous cluster results to a population representation.
priorToPopulation :: ClusterResults -> Population
priorToPopulation = Population
                  . Map.fromListWith (Seq.><)
                  . fmap ( L.over L._1 (fromMaybe (error "\nNo cluster for cell.") . headMay)
                         . L.over L._2 (Seq.singleton . _barcode)
                         . swap
                         )
                  . _clusterList

loadPopulation :: PriorPath
               -> IO (Population, Maybe ClusterResults)
loadPopulation (PriorPath path) = do
    dirExist <- FP.doesDirectoryExist path
    fileExist <- FP.doesFileExist path
    case (dirExist, fileExist) of
        (False, False) -> error "\nInput does not exist."
        (_, True)      -> do
            pop <- loadPopulationCsv . PriorPath $ path
            return (pop, Nothing)
        otherwise      -> do
            let crInput = path FP.</> "cluster_results.json"

            cr  <-
                fmap (either error id . A.eitherDecode) . B.readFile $ crInput

            let pop = priorToPopulation cr

            return (pop, Just cr)
