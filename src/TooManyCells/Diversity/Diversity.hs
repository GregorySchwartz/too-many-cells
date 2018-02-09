{- TooManyCells.Diversity.Diversity
Gregory W. Schwartz

Collects the functions pertaining to finding the diversity of populations.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PackageImports #-}

module TooManyCells.Diversity.Diversity
    ( getPopulationDiversity
    , getPopulationSize
    ) where

-- Remote
import Data.Maybe (fromMaybe)
import Math.Diversity.Diversity (diversityOfMap, chao1, rarefactionCurve)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

-- Local
import TooManyCells.Diversity.Types
import TooManyCells.MakeTree.Types
import TooManyCells.Matrix.Types

-- | Find the diversity, Chao1 estimator, and rarefaction curve of a population.
getPopulationDiversity :: Label
                       -> Order
                       -> Start
                       -> Interval
                       -> Maybe End
                       -> Population
                       -> IO PopulationDiversity
getPopulationDiversity
    label
    (Order order)
    (Start start)
    (Interval interval)
    end'
    (Population pop) = do
        let diversity   = Diversity . diversityOfMap order $ popCount
            chao        = Chao1 . chao1 $ popCount
            popCount    = Map.map Seq.length pop
            end         =
                fromMaybe (unSize . getPopulationSize . Population $ pop)
                    . fmap unEnd
                    $ end'
            getRarefactionPoint (!x, Just (!y, _)) = (X $ fromIntegral x, Y y)
            getRarefactionPoint (_, Nothing)       =
                error "Impossible rarefaction point."

        rarefaction <- fmap (Rarefaction . fmap getRarefactionPoint)
                     . rarefactionCurve False 0 start interval end
                     . Map.mapKeys (\ (Cluster !x) -> (show x, show x))
                     $ popCount

        return $ PopulationDiversity label diversity chao rarefaction

-- | Get the number of cells in a population.
getPopulationSize :: Population -> Size
getPopulationSize =
    Size . fromIntegral . Map.foldl' (+) 0 . Map.map Seq.length . unPopulation
