{- Clumpiness
Gregory W. Schwartz

Collects the functions pertaining to finding the clumpiness from a single cell
dendrogram.
-}

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Clumpiness
    ( dendToClumpCsv
    ) where

-- Remote
import Data.Monoid ((<>))
import Data.Tree (Tree(..))
import qualified "find-clumpiness" Clumpiness as Clump
import qualified "find-clumpiness" Types as Clump
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Clustering.Hierarchical as HC
import qualified Data.Csv as CSV
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Vector as V

-- Local
import Types

-- | Convert a single cell dendrogram to a workable format for clumpiness.
dendToClumpDend :: LabelMap -> HC.Dendrogram (V.Vector CellInfo) -> Tree Clump.NodeLabel
dendToClumpDend (LabelMap labelMap) =
    Clump.makeWorkable
        . fmap ( Seq.fromList
               . fmap unLabel
               . V.toList
               . fmap ((\x -> Map.findWithDefault (error ("Cell has no label: " <> show x)) x labelMap) . barcode)
               )

-- | Format clumpiness output to a CSV.
clumpToCsv :: [(T.Text, T.Text, Double)] -> B.ByteString
clumpToCsv = (<>) "label1,label2,value\n" . CSV.encode

-- | Get the clumpiness of the single cell labels and return a ready to print
-- CSV string.
dendToClumpCsv :: LabelMap -> HC.Dendrogram (V.Vector CellInfo) -> B.ByteString
dendToClumpCsv labelMap = clumpToCsv
                        . Clump.getClumpiness Clump.AllExclusive False False
                        . dendToClumpDend labelMap
