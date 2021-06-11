{- TooManyCells.Spatial.Types
Gregory W. Schwartz

Collects the spatial types used in the program.
-}

{-# LANGUAGE StrictData #-}

module TooManyCells.Spatial.Types where

-- Remote
import BirchBeer.Types (Feature)
import Data.Colour.Palette.BrewerSet (Kolor)
import qualified Data.Map.Strict as Map

-- Local

newtype ColorMap = ColorMap { unColorMap :: Map.Map Feature Kolor }

data Range = Range { minX :: Double
                   , maxX :: Double
                   , minY :: Double
                   , maxY :: Double
                   }

