{- TooManyCells.Program.Motifs
Gregory W. Schwartz

Motif entry point for command line program.
-}

{-# LANGUAGE OverloadedStrings #-}

module TooManyCells.Program.Motifs where

-- Remote
import Control.Monad (mfilter)
import Data.Maybe (fromMaybe)
import Options.Generic
import TextShow (showt)
import qualified Data.Text as T
import qualified Turtle as TU

-- Local
import TooManyCells.File.Types
import TooManyCells.Motifs.FindMotif
import TooManyCells.Motifs.Types
import TooManyCells.Program.Options

-- | Motif path.
motifsMain :: Options -> IO ()
motifsMain opts = do
  let genome' = GenomeFile . unHelpful . motifGenome $ opts
      topN' = TopN . fromMaybe 1000 . unHelpful . topN $ opts
      motifCommand' = MotifCommand
                    . fromMaybe "meme %s -nmotifs 50 -oc %s"
                    . unHelpful
                    . motifCommand
                    $ opts
      diffFile' = DiffFile . TU.fromText . unHelpful . diffFile $ opts
      outDir = OutputDirectory
             . fromMaybe "out"
             . unHelpful
             . output
             $ opts

  TU.mktree . TU.fromText . T.pack . unOutputDirectory $ outDir

  TU.sh $ do
    nodes <- TU.liftIO $ getNodes diffFile'
    node <- if null nodes then pure Nothing else TU.select . fmap Just $ nodes

    let outPath = OutputPath
                $ (TU.fromText . T.pack $ unOutputDirectory outDir)
           TU.</> (maybe mempty (TU.fromText . ("node_" <>) . showt . unNode) node)

    TU.liftIO $ getMotif diffFile' outPath motifCommand' genome' topN' node
