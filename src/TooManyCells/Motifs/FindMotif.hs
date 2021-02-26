{- TooManyCells.Motifs.FindMotif
Gregory W. Schwartz

Collects functions pertaining to finding motifs per cluster.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module TooManyCells.Motifs.FindMotif
    ( getMotif
    , getNodes
    ) where

-- Remote
import Control.Monad (mfilter)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import System.Directory (getTemporaryDirectory)
import TextShow (showt)
import qualified Control.Foldl as Fold
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Csv as CSV
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Text.Printf as TP
import qualified Turtle as TU
import qualified Turtle.Line as TU

-- Local
import TooManyCells.File.Types
import TooManyCells.Motifs.Types

newtype ColMatch = ColMatch T.Text
newtype Match = Match T.Text

-- | Get the temporary directory.
getTmpDir :: IO TU.FilePath
getTmpDir = fmap (TU.fromText . T.pack) getTemporaryDirectory

-- | Read a CSV to a shell.
readCsv :: TU.FilePath -> TU.Shell (Map.Map T.Text T.Text)
readCsv file = do
  contents <- TU.liftIO . B.readFile . T.unpack . TU.format TU.fp $ file

  TU.select
    . either error snd
    . CSV.decodeByName
    $ contents

-- | Cut a shell csv.
csvCut :: T.Text -> TU.Shell (Map.Map T.Text T.Text) -> TU.Shell (Maybe TU.Line)
csvCut c = fmap (fmap TU.unsafeTextToLine . Map.lookup c)

-- | Sort a csv.
csvSort :: T.Text
        -> TU.Shell (Map.Map T.Text T.Text)
        -> TU.Shell (Map.Map T.Text T.Text)
csvSort c = (=<<) TU.select . TU.sortOn (Map.lookup c)

-- | Sort a csv by a numeric column.
csvNumSort :: T.Text
        -> TU.Shell (Map.Map T.Text T.Text)
        -> TU.Shell (Map.Map T.Text T.Text)
csvNumSort c =
  (=<<) TU.select . TU.sortOn (fmap readNum . Map.lookup c)
  where
    readNum "Infinity" = 1 / 0 :: Double
    readNum "-Infinity" = -1 / 0 :: Double
    readNum x = either (error . (<> ": " <> T.unpack x)) fst . T.double $ x

-- | Match a column for a shell csv.
csvMatch :: ColMatch
         -> Match
         -> TU.Shell (Map.Map T.Text T.Text)
         -> TU.Shell (Map.Map T.Text T.Text)
csvMatch (ColMatch c) (Match m) =
  mfilter ((== m) . Map.findWithDefault m c)

-- | Get the nodes from a differential file
getNodes :: DiffFile -> IO [Node]
getNodes (DiffFile df)
      = fmap catMaybes
      . TU.reduce Fold.nub
      . (fmap . fmap) (either error (Node . fst) . T.decimal . TU.lineToText)
      . csvCut "node"
      . readCsv
      $ df

-- | Make a temporary fasta file for input into the motif command.
mkTmpFasta :: TU.FilePath
           -> DiffFile
           -> GenomeFile
           -> TopN
           -> Maybe Node
           -> TU.Shell ()
mkTmpFasta tmpFasta diffFile gf (TopN topN) node = TU.sh $ do
  tmpDir <- TU.liftIO getTmpDir
  tmpBed <- TU.mktempfile tmpDir "motif_input.bed"

  (=<<) (TU.output tmpBed . TU.select)
    . TU.reduce (Fold.lastN topN)
    . TU.sed ("-" *> pure "\t")
    . TU.sed (":" *> pure "\t")
    . fmap (fromMaybe (error "feature column not found."))
    . csvCut "feature"
    . csvNumSort "log2FC"
    . csvMatch (ColMatch "node") (Match $ maybe "0" (showt . unNode) node)
    . readCsv
    . unDiffFile
    $ diffFile

  TU.output tmpFasta
    . TU.inproc "bedtools" [ "getfasta"
                           , "-fi", unGenomeFile gf
                           , "-bed", TU.format TU.fp tmpBed
                           ]
    $ mempty

getMotif :: DiffFile
         -> Maybe BackgroundDiffFile
         -> OutputPath
         -> MotifCommand
         -> GenomeFile
         -> TopN
         -> Maybe Node
         -> IO ()
getMotif diffFile bgDiffFile outPath mc gf topN node = TU.sh $ do
  tmpDir <- TU.liftIO getTmpDir
  tmpFasta <- TU.mktempfile tmpDir "motif_input.fasta"
  mkTmpFasta tmpFasta diffFile gf topN node

  tmpBgFasta <- TU.mktempfile tmpDir "motif_bg_input.fasta"
  maybe
    (return ())
    (\x -> mkTmpFasta tmpBgFasta (DiffFile . unBackgroundDiffFile $ x) gf topN node)
    bgDiffFile

  let cmd = if isJust bgDiffFile
              then
                T.pack $
                  TP.printf
                    (unMotifCommand mc)
                    (TU.format TU.fp tmpFasta)
                    (TU.format TU.fp . unOutputPath $ outPath)
                    (TU.format TU.fp tmpBgFasta)
              else
                T.pack $
                  TP.printf
                    (unMotifCommand mc)
                    (TU.format TU.fp tmpFasta)
                    (TU.format TU.fp . unOutputPath $ outPath)

  TU.stdout . TU.inshell cmd $ mempty
