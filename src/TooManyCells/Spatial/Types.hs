{- TooManyCells.Spatial.Types
Gregory W. Schwartz

Collects the spatial types used in the program.
-}

{-# LANGUAGE StrictData #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TooManyCells.Spatial.Types where

-- Remote
import Control.Applicative (Alternative (..))
import Control.Monad (Monad (..), MonadPlus (..))
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
newtype SkipFinishedFlag = SkipFinishedFlag { unSkipFinishedFlag :: Bool }
newtype IncludeOthersFlag = IncludeOthersFlag { unIncludeOthersFlag :: Bool }
newtype StateLabelsFile = StateLabelsFile { unStateLabelsFile :: String }
newtype StateLabelMap = StateLabelMap { unStateLabelMap :: Map.Map Birch.Id Birch.Label }
newtype LabelList a = LabelList { unLabelList :: [a] } deriving (Eq, Ord, Show, Functor, Monoid, Semigroup, Applicative, Alternative, Monad, MonadPlus)
newtype Compare a = Compare { unCompare :: (T.Text, [a]) } deriving (Show)
newtype VarList a = VarList { unVarList :: [a] } deriving (Show, Functor, Semigroup, Monoid, Applicative, Alternative, Monad, MonadPlus)

data Range = Range { minX :: Double
                   , maxX :: Double
                   , minY :: Double
                   , maxY :: Double
                   }
data Mark = MarkFeature Birch.Feature | MarkLabel Birch.Label deriving (Show, Read, Eq, Ord)
data TopDistances = TopQuantile Double | TopDistance Double deriving (Show, Read)
