{- TooManyCells.MakeTree.Load
Gregory W. Schwartz

Collects the functions pertaining to loading labels for the tree.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module TooManyCells.MakeTree.Load
    ( loadClusterResultsFiles
    , loadClusterResults
    ) where

-- Remote
import BirchBeer.Load
import BirchBeer.Types
import BirchBeer.Utility
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad.Except (runExceptT, ExceptT (..))
import Control.Monad.Managed (with, liftIO, Managed (..))
import Data.Char (ord)
import Data.Matrix.MatrixMarket (readMatrix)
import Data.Maybe (fromMaybe, maybe)
import Data.Monoid ((<>))
import Data.Vector (Vector)
import Safe
import qualified Control.Lens as L
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Streaming.Char8 as BS
import qualified Data.Csv as CSV
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Sparse.Common as HS
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as H
import qualified Streaming as S
import qualified Streaming.Cassava as S
import qualified Streaming.Prelude as S
import qualified Streaming.With.Lifted as SW

-- Local
import TooManyCells.File.Types
import TooManyCells.MakeTree.Types
import TooManyCells.Matrix.Types

-- | Load a format for the tree and results from a file.
loadClusterResultsFiles :: FilePath -> FilePath -> IO ClusterResults
loadClusterResultsFiles clusterListFile treeFile = do
  clusterList <-
    fmap (either error id . A.eitherDecode) . B.readFile $ clusterListFile
  tree        <-
    fmap (either error id . A.eitherDecode) . B.readFile $ treeFile

  return $ ClusterResults clusterList tree

-- | Load a format for the tree and results from a file.
loadClusterResults :: FilePath -> IO ClusterResults
loadClusterResults file = do
  dend <-
    fmap (fmap clusterResultConversion . A.eitherDecode) . B.readFile $ file
  tree <- fmap A.eitherDecode . B.readFile $ file

  return . either error id $ dend <> tree

-- | Convert a dendrogram type to a tree type in the cluster results.
clusterResultConversion :: ClusterResultsDend -> ClusterResults
clusterResultConversion cr =
  ClusterResults { _clusterList = _clusterList' cr
                 , _clusterDend = dendToTree $ _clusterDend' cr
                 }
