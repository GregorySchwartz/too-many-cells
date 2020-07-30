{- TooManyCells.Program.Utility
Gregory W. Schwartz

Utility functions for the command line program.
-}

module TooManyCells.Program.Utility where

-- Remote
import Data.Bool (bool)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Options.Generic
import Text.Read (readMaybe)
import qualified System.Directory as FP
import qualified System.FilePath as FP

-- Local
import TooManyCells.File.Types
import TooManyCells.Matrix.Types
import TooManyCells.Program.Options

-- | Read or return an error.
readOrErr :: (Read a) => String -> String -> a
readOrErr err = fromMaybe (error err) . readMaybe

-- | Normalization defaults.
getNormalization :: Options -> [NormType]
getNormalization opts@(MakeTree{}) =
  (\x -> if null x then [TfIdfNorm] else x)
    . fmap (readOrErr "Cannot read --normalization.")
    . unHelpful
    . normalization
    $ opts
getNormalization opts@(Interactive{}) =
  (\x -> if null x then [TfIdfNorm] else x)
    . fmap (readOrErr "Cannot read --normalization.")
    . unHelpful
    . normalization
    $ opts
getNormalization opts =
  (\x -> if null x then [NoneNorm] else x)
    . fmap (readOrErr "Cannot read --normalization.")
    . unHelpful
    . normalization
    $ opts

-- | Get the file type of an input matrix. Returns either a left file (i.e. CSV)
-- or a right matrix market.
getMatrixFileType :: FilePath -> IO (Either MatrixFileType MatrixFileType)
getMatrixFileType path = do
  fileExist      <- FP.doesFileExist path
  directoryExist <- FP.doesDirectoryExist path
  compressedFileExist <- FP.doesFileExist $ path FP.</> "matrix.mtx.gz"

  let fragmentsFile = (\ (x, y) -> isInfixOf "fragments" x && y == ".tsv.gz")
                    . FP.splitExtensions
                    . FP.takeFileName
                    $ path
      matrixFile
        | fileExist && (FP.takeExtension path == ".bdg") = Left . BedGraph $ path
        | fileExist && (FP.takeExtension path == ".bw") = Left . BigWig $ path
        | fileExist && not fragmentsFile = Left . DecompressedMatrix . MatrixFile $ path
        | fileExist && fragmentsFile = Left . CompressedFragments . FragmentsFile $ path
        | directoryExist && not compressedFileExist = Right . DecompressedMatrix . MatrixFile $ path FP.</> "matrix.mtx"
        | directoryExist && compressedFileExist = Right . CompressedMatrix . MatrixFile $ path FP.</> "matrix.mtx.gz"
        | directoryExist = error "Cannot determine matrix pointed to, are there too many matrices here?"
        | otherwise = error "\nMatrix path does not exist."

  return matrixFile
