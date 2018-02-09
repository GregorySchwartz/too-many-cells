{- TooManyCells.Differential.Types
Gregory W. Schwartz

Collects the differental types used in the program.
-}

{-# LANGUAGE StrictData #-}

module TooManyCells.Differential.Types where

-- Remote
import qualified Data.Graph.Inductive as G

-- Local

-- Basic
newtype TopN = TopN { unTopN :: Int }
newtype DiffVertices = DiffVertices
    { unDiffVertices :: ([G.Node], [G.Node])
    }
