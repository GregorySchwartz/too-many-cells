{- TooManyCells.Program.Motifs
Gregory W. Schwartz

Motif entry point for command line program.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module TooManyCells.Program.Motifs where

-- Remote
import Control.Monad (mfilter)
import Data.Maybe (fromMaybe)
import TextShow (showt)
import qualified Data.Text as T
import qualified Turtle as TU

-- Local
import TooManyCells.File.Types
import TooManyCells.Motifs.FindMotif
import TooManyCells.Motifs.Types
import TooManyCells.Program.Options

-- | Motif path.
motifsMain :: Subcommand -> IO ()
motifsMain (MotifsCommand opts) = do
  let genome' = GenomeFile . motifGenome $ opts
      topN' = TopN . (topN :: Motifs -> Int) $ opts
      motifCommand' = MotifCommand . motifCommand $ opts
      motifGenomeCommand' = fmap MotifGenomeCommand
                          . motifGenomeCommand
                          $ opts
      diffFile' = DiffFile . TU.fromText . diffFile $ opts
      backgroundDiffFile' = fmap (BackgroundDiffFile . TU.fromText)
                          . backgroundDiffFile
                          $ opts
      outDir = OutputDirectory
             . (output :: Motifs -> String)
             $ opts

  TU.mktree . TU.fromText . T.pack . unOutputDirectory $ outDir

  TU.sh $ do
    nodes <- TU.liftIO $ getNodes diffFile'
    node <- if null nodes then pure Nothing else TU.select . fmap Just $ nodes

    let outPath = OutputPath
                $ (TU.fromText . T.pack $ unOutputDirectory outDir)
           TU.</> (maybe mempty (TU.fromText . ("node_" <>) . showt . unNode) node)

    case motifGenomeCommand' of
      Nothing -> TU.liftIO
               $ getMotif
                   diffFile'
                   backgroundDiffFile'
                   outPath
                   motifCommand'
                   genome'
                   topN'
                   node
      (Just x) -> TU.liftIO
                $ getMotifGenome
                    diffFile'
                    backgroundDiffFile'
                    outPath
                    x
                    genome'
                    topN'
                    node
