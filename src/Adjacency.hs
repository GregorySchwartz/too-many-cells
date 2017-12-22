{- Adjacency
Gregory W. Schwartz

Collects functions pertaining to calculating similarities and adjacency
matrices.
-}

module Adjacency
    ( cosineSimilarity
    , getAdjacencyMat
    ) where

-- Remote
import qualified Numeric.LinearAlgebra as H

-- Local
import Types

-- | Get the cosine similarity between two vectors.
cosineSimilarity :: H.Vector Double -> H.Vector Double -> Double
cosineSimilarity v w = H.dot v w / (H.norm_2 v * H.norm_2 w)

-- | Get an adjacency matrix based on a matrix where each row is an observation
-- and the adjacencies are cosine similarities.
getAdjacencyMat :: MatObsRow -> AdjacencyMat
getAdjacencyMat (MatObsRow mat) =
    AdjacencyMat $ H.assoc (size, size) 0 assocList
  where
    size :: Int
    size       = H.rows mat
    getSim :: Int -> Int -> Double
    getSim i j = if i == j
                    then 0
                    else cosineSimilarity (mat H.! i) (mat H.! j)
    assocList :: [((Int, Int), Double)]
    assocList  = (\i j -> ((i, j), getSim i j))
             <$> [0,1 .. (size - 1)]
             <*> [0,1 .. (size - 1)]
