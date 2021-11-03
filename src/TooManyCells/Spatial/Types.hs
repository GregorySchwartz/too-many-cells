{- TooManyCells.Spatial.Types
Gregory W. Schwartz

Collects the spatial types used in the program.
-}

{-# LANGUAGE StrictData #-}

module TooManyCells.Spatial.Types where

-- Remote
import Data.Colour.Palette.BrewerSet (Kolor)
import qualified BirchBeer.Types as Birch
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

-- Local

newtype ColorMap = ColorMap { unColorMap :: Map.Map Birch.Feature Kolor }
newtype StartCol = StartCol { unStartCol :: T.Text }
newtype EndCol = EndCol { unEndCol :: T.Text }
newtype AnnoSpatMarkerFile = AnnoSpatMarkerFile { unAnnoSpatMarkerFile :: String }
newtype AnnoSpatCommand = AnnoSpatCommand { unAnnoSpatCommand :: String }
newtype PCFCrossFlag = PCFCrossFlag { unPCFCrossFlag :: Bool }
newtype StateLabelsFile = StateLabelsFile { unStateLabelsFile :: String }
newtype StateLabelMap = StateLabelMap { unStateLabelMap :: Map.Map Birch.Id Birch.Label }

data Range = Range { minX :: Double
                   , maxX :: Double
                   , minY :: Double
                   , maxY :: Double
                   }
data Mark = MarkFeature Birch.Feature | MarkLabel Birch.Label deriving (Show, Read, Eq, Ord)
