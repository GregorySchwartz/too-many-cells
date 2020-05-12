{- TooManyCells.MakeTree.Adjacency
Gregory W. Schwartz

Collects functions pertaining to calculating similarities and adjacency
matrices.
-}

module TooManyCells.MakeTree.Adjacency
    ( cosineSimilarityDense
    , cosineSimilaritySparse
    , getDenseAdjacencyMat
    , getSparseAdjacencyMat
    ) where

-- Remote
import Data.List (foldl')
import qualified Numeric.LinearAlgebra as H
import qualified Data.Sparse.Common as S

-- Local
import TooManyCells.MakeTree.Types
import TooManyCells.Matrix.Types
import TooManyCells.Matrix.Utility

-- | Get the cosine similarity between two vectors.
cosineSimilarityDense :: H.Vector Double -> H.Vector Double -> Double
cosineSimilarityDense v w = H.dot v w / (H.norm_2 v * H.norm_2 w)

-- | Get the cosine similarity between two vectors.
cosineSimilaritySparse :: S.SpVector Double -> S.SpVector Double -> Double
cosineSimilaritySparse v w = S.dot v w / (norm2 v * norm2 w)
  where
    norm2 = sqrt . foldl' (+) 0 . fmap (** 2)

-- | Get an adjacency matrix based on a matrix where each row is an observation
-- and the adjacencies are cosine similarities.
getDenseAdjacencyMat :: MatObsRow -> AdjacencyMat
getDenseAdjacencyMat (MatObsRow sparseMat) =
    AdjacencyMat $ H.assoc (size, size) 0 assocList
  where
    mat = sparseToHMat sparseMat
    size :: Int
    size       = H.rows mat
    getSim :: Int -> Int -> Double
    getSim i j = if i == j
                    then 0
                    else cosineSimilarityDense (mat H.! i) (mat H.! j)
    assocList :: [((Int, Int), Double)]
    assocList  = (\i j -> ((i, j), getSim i j))
             <$> [0,1 .. (size - 1)]
             <*> [0,1 .. (size - 1)]

-- | Get an adjacency matrix based on a matrix where each row is an observation
-- and the adjacencies are cosine similarities.
getSparseAdjacencyMat :: MatObsRow -> AdjacencyMat
getSparseAdjacencyMat (MatObsRow mat) =
    AdjacencyMat $ H.assoc (size, size) 0 assocList
  where
    size :: Int
    size       = S.nrows mat
    getSim :: Int -> Int -> Double
    getSim i j = if i == j
                    then 0
                    else cosineSimilaritySparse (S.extractRow mat i) (S.extractRow mat j)
    assocList :: [((Int, Int), Double)]
    assocList  = (\i j -> ((i, j), getSim i j))
             <$> [0,1 .. (size - 1)]
             <*> [0,1 .. (size - 1)]
