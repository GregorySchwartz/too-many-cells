{- TooManyCells.Paths.Types
Gregory W. Schwartz

Collects the path types used in the program.
-}

{-# LANGUAGE StrictData #-}

module TooManyCells.Paths.Types where

-- Remote

-- Local


-- Basic
newtype FlipFlag  = FlipFlag { unFlipFlag :: Bool }
newtype ShallowFlag = ShallowFlag { unShallowFlag :: Bool }
newtype Bandwidth = Bandwidth { unBandwidth :: Double }

-- Advanced
data PathDistance = PathStep | PathModularity deriving (Read, Show)
