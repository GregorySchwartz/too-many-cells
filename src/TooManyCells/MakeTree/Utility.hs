{- TooManyCells.MakeTree.Utility
Gregory W. Schwartz

Collects utility functions for the tree.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module TooManyCells.MakeTree.Utility
    ( updateTreeRowIndex
    , updateTreeRowBool
    ) where

-- Remote
import BirchBeer.Types
import Data.Maybe (fromMaybe)
import Data.Tree (Tree)
import qualified Control.Lens as L
import qualified Data.HashMap.Strict as HMap
import qualified Data.Vector as V

-- Local
import TooManyCells.MakeTree.Types
import TooManyCells.Matrix.Types

-- | Update the CellInfo row index with new matrix row index values.
updateTreeRowIndex :: SingleCells
                   -> Tree (TreeNode (V.Vector CellInfo))
                   -> Tree (TreeNode (V.Vector CellInfo))
updateTreeRowIndex sc = fmap (L.over item (fmap (V.mapMaybe updateCell)))
  where
    cellRowMap =
      HMap.fromList . flip zip (fmap Row [0..]) . V.toList . getRowNames $ sc
    updateCell c = do
      newRow <- flip HMap.lookup cellRowMap
                  . unCell
                  . L.view barcode
                  $ c
      return $ L.set cellRow newRow c

-- | updateTreeRowIndex meant to be called from the main program with these
-- options.
updateTreeRowBool :: UpdateTreeRowsFlag
                  -> Maybe SingleCells
                  -> Tree (TreeNode (V.Vector CellInfo))
                  -> Tree (TreeNode (V.Vector CellInfo))
updateTreeRowBool (UpdateTreeRowsFlag False) _ tree = tree
updateTreeRowBool (UpdateTreeRowsFlag True) Nothing _ =
  error "Cannot update tree for use with new matrix without a --matrix-path."
updateTreeRowBool (UpdateTreeRowsFlag True) (Just sc) tree =
  updateTreeRowIndex sc tree
