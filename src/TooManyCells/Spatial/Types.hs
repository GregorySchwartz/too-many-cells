{- TooManyCells.Spatial.Types
Gregory W. Schwartz

Collects the spatial types used in the program.
-}

{-# LANGUAGE StrictData #-}

module TooManyCells.Spatial.Types where

-- Remote
import BirchBeer.Types (Feature, Label)
import Data.Colour.Palette.BrewerSet (Kolor)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

-- Local

newtype ColorMap = ColorMap { unColorMap :: Map.Map Feature Kolor }
newtype StartCol = StartCol { unStartCol :: T.Text }
newtype EndCol = EndCol { unEndCol :: T.Text }
newtype AnnoSpatMarkerFile = AnnoSpatMarkerFile { unAnnoSpatMarkerFile :: String }
newtype AnnoSpatCommand = AnnoSpatCommand { unAnnoSpatCommand :: String }
newtype PCFCrossFlag = PCFCrossFlag { unPCFCrossFlag :: Bool }

data Range = Range { minX :: Double
                   , maxX :: Double
                   , minY :: Double
                   , maxY :: Double
                   }
data Mark = MarkFeature Feature | MarkLabel Label deriving (Show, Read, Eq, Ord)
