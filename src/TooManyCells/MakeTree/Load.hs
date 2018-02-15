{- TooManyCells.MakeTree.Load
Gregory W. Schwartz

Collects the functions pertaining to loading labels for the tree.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module TooManyCells.MakeTree.Load
    ( loadLabelData
    ) where

-- Remote
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
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Streaming.Char8 as BS
import qualified Data.Csv as CSV
import qualified Data.Foldable as F
import qualified Data.Map as Map
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
import TooManyCells.MakeTree.Utility
import TooManyCells.Matrix.Types

-- | Load a CSV containing the label of each cell.
loadLabelData :: Delimiter -> LabelFile -> IO LabelMap
loadLabelData (Delimiter delim) (LabelFile file) = do
    let csvOpts = CSV.defaultDecodeOptions { CSV.decDelimiter = fromIntegral (ord delim) }

    rows <- fmap (\ x -> either error snd ( CSV.decodeByNameWith csvOpts x
                                        :: Either String (CSV.Header, Vector (Map.Map T.Text T.Text))
                                         )
                 )
          . B.readFile
          $ file

    let toLabelMap :: Map.Map T.Text T.Text -> Map.Map Id Label
        toLabelMap m =
            Map.singleton
                (Id $ Map.findWithDefault (error "No \"cell\" column in label file.") "cell" m)
                (Label $ Map.findWithDefault (error "No \"label\" column in label file.") "label" m)

    return . LabelMap . Map.unions . fmap toLabelMap . V.toList $ rows
