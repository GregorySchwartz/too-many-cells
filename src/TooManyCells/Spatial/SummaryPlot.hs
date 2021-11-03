{- TooManyCells.Spatial.SummaryPlot
Gregory W. Schwartz

Collects functions pertaining to plotting the summary of statistics for all
samples.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PackageImports #-}

module TooManyCells.Spatial.SummaryPlot
    ( plotSummary
    ) where

import Control.Monad (when)
import Data.Bool (bool)
import Data.Function (on)
import Data.List (sortBy, groupBy, foldl', sort, maximumBy, zipWith4)
import Data.Maybe (fromMaybe)
import Ploterific.Plot.Plot (labelColorScale)
import Ploterific.Plot.Types (ColorLabel (..))
import Safe (headMay)
import System.Environment (getArgs)
import qualified BirchBeer.Types as Birch
import qualified Control.Foldl as Fold
import qualified Control.Lens as L
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.Csv.Streaming as CSVStream
import qualified Data.Csv as CSV
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import qualified Graphics.Vega.VegaLite as VL
import qualified Graphics.Vega.VegaLite.Theme as VL
import qualified Statistics.Test.KruskalWallis as S
import qualified "statistics" Statistics.Quantile as S
import qualified "statistics" Statistics.Types as S
import qualified Filesystem.Path as FP
import qualified Turtle as TU

import qualified TooManyCells.File.Types as Too
import qualified TooManyCells.Spatial.Types as Too

newtype LabelList a = LabelList { unLabelList :: [a] } deriving (Eq, Ord, Show)
newtype Compare a = Compare { unCompare :: (T.Text, [a]) } deriving (Show)
newtype VarList a = VarList { unVarList :: [a] } deriving (Show)

-- | Get all pairwise comparisons.
pairwise :: (Eq a, Ord a) => [a] -> [[a]]
pairwise xs = Set.toList
            . Set.fromList
            . filter (\[x,y] -> x /= y)
            . fmap sort
            $ (\x y -> [x,y]) <$> xs <*> xs

-- | Add on the relevant p-values
pVal :: Birch.Feature
     -> [Map.Map T.Text T.Text]
     -> ([Map.Map T.Text T.Text], [Map.Map T.Text T.Text])
pVal (Birch.Feature feature) ms = (statRows, statSummary)
  where
    statRows =
      mconcat
        . zipWith3
            (\ x xs ys
            -> fmap (\ m
                    -> foldl' (\acc (!a, !b) -> Map.insert a b acc) m (("kwPValue", x):xs)
                    )
            . concatMap unLabelList
            . unVarList
            $ ys
            )
            tests
            (fmap unVarList pairwiseTests)
        $ grouped
    statSummary =
      mconcat
        . zipWith4
              (\ s x xs ys
              -> fmap (\ m
                      -> foldl'
                          (\acc (!a, !b) -> Map.insert a b acc)
                          m
                          (("highestLabel", s):("kwPValue", x):xs)
                      )
              . concatMap unLabelList
              . unVarList
              $ ys
              )
              highestLabel
              tests
              (fmap unVarList pairwiseTests)
          $ summarizedVarList
    highestLabel :: [T.Text]
    highestLabel = fmap ( snd
                        . maximumBy (compare `on` fst)
                        . concatMap
                            ( fmap
                                (\ !x
                                -> ( maybe
                                      (error "No median found")
                                      (either error fst . T.double)
                                   $ Map.lookup "median" x
                                   , Map.findWithDefault
                                      (error "No label found")
                                      "label"
                                      x
                                   )
                                )
                                . unLabelList
                            )
                        . unVarList
                        )
                 $ summarizedVarList
    summarizedVarList :: [VarList (LabelList (Map.Map T.Text T.Text))]
    summarizedVarList = VarList
                      . fmap ( LabelList
                             . (:[])
                             . getStatSummaries
                             . unLabelList
                             )
                      . unVarList
                    <$> grouped
    getStatSummaries !ms = Map.fromList
                         $ ("median", T.pack . show $ getMedianFromMaps ms)
                         : ("feature", feature)
                         : foldl'
                            (\ acc x
                            -> ( x
                               , fromMaybe "" $ Map.lookup x =<< headMay ms
                               )
                               : acc
                            )
                            []
                            ["label", "Var1", "Var2"]
    getMedianFromMaps = S.median S.s . VU.fromList . fmap getFeature
    kruskalOrBust xs = bool (S.kruskalWallisTest xs) Nothing
                     . and
                     $ fmap (== head xs) (tail xs)
    tests :: [T.Text]
    tests = fmap ( maybe "1" (T.pack . show . S.pValue . S.testSignificance)
                 . kruskalOrBust
                 . fmap (VU.fromList . fmap getFeature . unLabelList)
                 . unVarList
                 )
          $ grouped
    pairwiseTests :: [VarList (T.Text, T.Text)]
    pairwiseTests = fmap ( VarList
                         . fmap
                            ( L.over
                                L._2
                                ( maybe
                                    "1"
                                    ( T.pack
                                    . show
                                    . S.pValue
                                    . S.testSignificance
                                    )
                                . kruskalOrBust
                                . fmap
                                    (VU.fromList . fmap getFeature . unLabelList)
                                )
                                . unCompare
                            )
                         . setupComparisonVar
                         )
                   $ grouped
    setupComparisonVar :: VarList (LabelList (Map.Map T.Text T.Text))
                       -> [Compare (LabelList (Map.Map T.Text T.Text))]
    setupComparisonVar = fmap ( Compare
                              . L.over L._1 (T.intercalate "/")
                              . unzip
                              )
                       . (\x -> pairwise x :: [[(T.Text, LabelList (Map.Map T.Text T.Text))]])
                       . fmap ( L.over L._2 LabelList
                              . L.over L._1 (fromMaybe "" . headMay)
                              . unzip
                              . fmap (\ !x -> (fromMaybe "" $ compLabel x, x))
                              . unLabelList
                              )
                       . unVarList
    getFeature = maybe 0 (either error fst . T.double) . Map.lookup feature
    grouped :: [VarList (LabelList (Map.Map T.Text T.Text))]
    grouped = fmap ( VarList
                   . fmap LabelList
                   . groupBy ((==) `on` compLabel)
                   . sortBy (compare `on` compLabel)
                   )
            . groupBy ((==) `on` compVar)
            . sortBy (compare `on` compVar)
            $ ms
    compLabel = Map.lookup "label"
    compVar x = (Map.lookup "Var1" x, Map.lookup "Var2" x)

-- | Parse a file into CSV.
parseFile :: Maybe Too.StateLabelMap -> TU.FilePath -> IO [Map.Map T.Text T.Text]
parseFile slm file = do
  contents <- BL.readFile . T.unpack . TU.format TU.fp $ file
  return
    . either error (fmap (insertSampleLabel slm file) . F.toList . snd)
    . CSVStream.decodeByName
    $ contents

-- | Insert samples and labels in a row.
insertSampleLabel :: Maybe Too.StateLabelMap
                  -> TU.FilePath
                  -> Map.Map T.Text T.Text
                  -> Map.Map T.Text T.Text
insertSampleLabel slm file =
  Map.insert "label" label . Map.insert "sample" sample
  where
    sample = getSample file
    label = maybe sample Birch.unLabel
          . (=<<) (Map.lookup (Birch.Id sample) . Too.unStateLabelMap)
          $ slm

-- | Get sample from file path.
getSample :: TU.FilePath -> T.Text
getSample = TU.format TU.fp . TU.basename . p . p . p
  where
    p = TU.parent

plotSummary :: Too.OutputDirectory
            -> Maybe Too.StateLabelMap
            -> Birch.Feature
            -> IO ()
plotSummary (Too.OutputDirectory inDir) slm feature = do
  let inDir' = TU.fromText . T.pack $ inDir
  stats <- TU.reduce Fold.list . TU.find (TU.suffix "stats.csv") $ inDir'
  statsParsed <- mapM (parseFile slm) stats

  when (null statsParsed) $ print "No stats.csv files found, first generate spatial results"

  let outDir = inDir' FP.</> "summary"

  TU.mktree outDir

  let outputBasename = "summary_" <> Birch.unFeature feature
      outputPlot = outDir FP.</> TU.fromText (outputBasename <> ".html")
      outputStats = outDir FP.</> TU.fromText (outputBasename <> ".csv")
      rows' = mconcat statsParsed
      (rows, stats) = pVal feature rows'
      pValComparisons = fmap (T.intercalate "/" . fmap (fromMaybe ""))
                      . pairwise
                      . Set.toList
                      . Set.fromList
                      . fmap (Map.lookup "label")
                      $ rows'
      colorLabels = fmap ColorLabel
                  . Set.toAscList
                  . Set.fromList
                  $ fmap (fromMaybe "" . Map.lookup "label") rows
      feature' = Birch.unFeature feature
      dataSet = VL.dataFromColumns []
              . VL.dataColumn
                  "var1"
                  (VL.Strings . fmap (Map.findWithDefault "" "Var1") $ rows)
              . VL.dataColumn
                  "var2"
                  (VL.Strings . fmap (Map.findWithDefault "" "Var2") $ rows)
              . VL.dataColumn
                  "label"
                  (VL.Strings . fmap (Map.findWithDefault "" "label") $ rows)
              . VL.dataColumn
                  "sample"
                  (VL.Strings . fmap (Map.findWithDefault "" "sample") $ rows)
              . VL.dataColumn
                  "item"
                  (VL.Strings . fmap (Map.findWithDefault "" "item") $ rows)
              . VL.dataColumn
                  "kwPValue"
                  ( VL.Numbers
                  . fmap
                      ( maybe 0 (either error fst . T.double)
                      . Map.lookup "kwPValue"
                      )
                  $ rows
                  )
              . (\ y
                -> foldl'
                    (\ acc x
                    -> VL.dataColumn
                        x
                        ( VL.Numbers
                        . fmap ( maybe 0 (either error fst . T.double)
                               . Map.lookup x
                               )
                        $ rows
                        ) acc
                    )
                    y
                    pValComparisons
                )
              $ VL.dataColumn
                  feature'
                  ( VL.Numbers
                  . fmap
                      ( maybe 0 (either error fst . T.double)
                      . Map.lookup feature'
                      )
                  $ rows
                  )
                  []
      transDens = transMax
                . VL.density feature' [ VL.DnGroupBy ["var1", "var2", "label"]]
      transHref = VL.calculateAs ("'../../interactive_classified_imc_publication/' + datum.item + '.tif_mat.csv.html'") "url"
      transMax = VL.joinAggregate
                  [ VL.opAs VL.Max feature' "max" ]
                  [ VL.WGroupBy ["var1", "var2"] ]
               . VL.calculateAs ("datum." <> feature' <> "/datum.max") "maxTrans"
               . transHref
      picked = "picked"
      sel = VL.selection
          . VL.select
              picked
              VL.Interval
              [ VL.Encodings [ VL.ChX, VL.ChY ], VL.BindScales ]
      axisSize = VL.Axis [ VL.TickWidth 0.756, VL.DomainWidth 0.756 ]
      densitySpec = [ VL.mark VL.Area [ VL.MOpacity 0.5]
                    , sel []
                    , VL.encoding
                    . VL.position
                        VL.Y
                        [ VL.PName "density"
                        , VL.PmType VL.Quantitative
                        ]
                    . VL.position
                        VL.X
                        [ VL.PName "value"
                        , VL.PmType VL.Quantitative
                        ]
                    . VL.color
                        [ VL.MName "label"
                        , VL.MmType VL.Nominal
                        , labelColorScale colorLabels
                        ]
                    $ []
                    , VL.transform $ transDens []
                    ]
      tickSpec = [ VL.mark VL.Tick []
                 , VL.transform $ transMax []
                 , VL.encoding
                 . VL.position
                    VL.X
                    [VL.PName feature', VL.PmType VL.Quantitative]
                 . VL.tooltips ( fmap
                                  (\x ->  [VL.TName x, VL.TmType VL.Nominal])
                                  ( [ feature'
                                    , "sample"
                                    , "label"
                                    , "item"
                                    , "kwPValue"
                                    ]
                                 <> pValComparisons
                                  )
                               )
                 . VL.color [ VL.MName "label"
                            , VL.MmType VL.Nominal
                            , labelColorScale colorLabels
                            ]
                 . VL.hyperlink [ VL.HName "url", VL.HmType VL.Nominal ]
                 $ []
                 ]
      boxSpec = [ VL.mark
                    VL.Boxplot
                    [ VL.MNoOutliers
                    , VL.MOpacity 0.5
                    , VL.MMedian [ VL.MColor "black" ]
                    ]
                , VL.transform $ transMax []
                , VL.encoding
                . VL.position
                    VL.X
                    [VL.PName feature', VL.PmType VL.Quantitative]
                . VL.position VL.Y [ VL.PNumber 30 ]
                . VL.color
                    [ VL.MName "label"
                    , VL.MmType VL.Nominal
                    , labelColorScale colorLabels
                    ]
                $ []
                ]
      res = VL.resolve
          . VL.resolution ( VL.RAxis
                              [ (VL.ChX, VL.Independent)
                              , (VL.ChY, VL.Independent)
                              ]
                          )
      p = VL.toVegaLite [ dataSet
                        , VL.specification
                        $ VL.asSpec
                            [ VL.layer
                                (fmap VL.asSpec [densitySpec, tickSpec, boxSpec])
                            ]
                        , VL.facet [ VL.RowBy
                                      [VL.FName "var1", VL.FmType VL.Nominal]
                                   , VL.ColumnBy
                                      [VL.FName "var2", VL.FmType VL.Nominal]
                                   ]
                        , res []
                        , VL.theme
                            ( VL.defaultConfig
                                { VL.configHeight = Just 170
                                , VL.configWidth = Just 200
                                , VL.configFontSize = Just 8
                                , VL.configTitleFontSize = Just 9.33333015
                                }
                            )
                            (VL.configuration axisSize [])
                        ]

  -- Output plot
  VL.toHtmlFile (T.unpack . TU.format TU.fp $ outputPlot) p

  -- Output stats
  let header = V.fromList
             . maybe [] (fmap (B.pack . T.unpack) . Map.keys)
             . headMay
             $ stats
  BL.writeFile (T.unpack . TU.format TU.fp $ outputStats)
    . CSV.encodeByName header
    $ stats
