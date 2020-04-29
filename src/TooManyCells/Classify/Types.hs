{- TooManyCells.Classify.Types
Gregory W. Schwartz

Collects the types used in classification of cells.
-}

{-# LANGUAGE StrictData #-}

module TooManyCells.Classify.Types where

-- Remote

-- Local

-- Basic
newtype SingleMatrixFlag = SingleMatrixFlag
    { unSingleMatrixFlag :: Bool
    } deriving (Read,Show)
