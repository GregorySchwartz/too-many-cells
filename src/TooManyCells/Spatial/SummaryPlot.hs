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

import Control.Exception (SomeException (..), try, evaluate)
import Control.Monad (when, mfilter, guard)
import Data.Bool (bool)
import Data.Function (on)
import Data.List (sortBy, groupBy, foldl', sort, maximumBy, zipWith4)
import Data.Maybe (fromMaybe, mapMaybe, isNothing)
import Ploterific.Plot.Plot (labelColorScale)
import Ploterific.Plot.Types (ColorLabel (..))
import Safe (headMay)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
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

-- | Get all pairwise comparisons.
pairwise :: (Eq a, Ord a) => [a] -> [[a]]
pairwise xs = Set.toList
            . Set.fromList
            . filter (\[x,y] -> x /= y)
            . fmap sort
            $ (\x y -> [x,y]) <$> xs <*> xs

-- | Safer Kruskal-Wallis test.
safeKWTest :: (VU.Unbox a, Ord a) => [VU.Vector a] -> IO Double
safeKWTest xs = do
  res <- try ( evaluate
             . maybe 1 (S.pValue . S.testSignificance)
             $ S.kruskalWallisTest xs
             ) :: IO (Either SomeException Double)
  res' <- case res of
            Left ex -> do
              hPutStrLn stderr $ "Caught exception, continuing with p = 1: " <> show ex
              return 1
            Right val -> return val
  return res'

-- | Add on the relevant p-values
pVal :: Birch.Feature
     -> [Map.Map T.Text T.Text]
     -> IO ([Map.Map T.Text T.Text], [Map.Map T.Text T.Text])
pVal (Birch.Feature feature) ms = do
  statRows' <- statRows
  statSummary' <- statSummary
  return (statRows', statSummary')
  where
    statRows = do
      tests' <- tests
      pairwiseTests' <- pairwiseTests
      return
        . mconcat
        . zipWith3
            (\ x xs ys
            -> fmap (\ m
                    -> foldl' (\acc (!a, !b) -> Map.insert a b acc) m (("kwPValue", x):xs)
                    )
            . concatMap Too.unLabelList
            . Too.unVarList
            $ ys
            )
            tests'
            (fmap Too.unVarList pairwiseTests')
        $ grouped
    statSummary = do
      tests' <- tests
      pairwiseTests' <- pairwiseTests
      return
        . mconcat
        . zipWith4
              (\ s x xs ys
              -> fmap (\ m
                      -> foldl'
                          (\acc (!a, !b) -> Map.insert a b acc)
                          m
                          (("highestLabel", s):("kwPValue", x):xs)
                      )
              . concatMap Too.unLabelList
              . Too.unVarList
              $ ys
              )
              highestLabel
              tests'
              (fmap Too.unVarList pairwiseTests')
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
                                      "statLabel"
                                      x
                                   )
                                )
                                . Too.unLabelList
                            )
                        . Too.unVarList
                        )
                 $ summarizedVarList
    summarizedVarList :: [Too.VarList (Too.LabelList (Map.Map T.Text T.Text))]
    summarizedVarList = Too.VarList
                      . fmap ( Too.LabelList
                             . (:[])
                             . getStatSummaries
                             . Too.unLabelList
                             )
                      . Too.unVarList
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
                            ["statLabel", "Var1", "Var2"]
    getMedianFromMaps = S.median S.s . VU.fromList . fmap getFeature
    kruskalOrBust = fmap (T.pack . show) . safeKWTest
    tests :: IO [T.Text]
    tests = mapM ( kruskalOrBust
                 . fmap (VU.fromList . fmap getFeature . Too.unLabelList)
                 . Too.unVarList
                 )
          $ grouped
    pairwiseTests :: IO [Too.VarList (T.Text, T.Text)]
    pairwiseTests = mapM ( fmap Too.VarList
                         . mapM
                            ( sequence
                            . L.over
                                L._2
                                ( kruskalOrBust
                                . fmap
                                    ( VU.fromList
                                    . fmap getFeature
                                    . Too.unLabelList
                                    )
                                )
                                . Too.unCompare
                            )
                         . setupComparisonVar
                         )
                   $ grouped
    setupComparisonVar :: Too.VarList (Too.LabelList (Map.Map T.Text T.Text))
                       -> [Too.Compare (Too.LabelList (Map.Map T.Text T.Text))]
    setupComparisonVar = fmap ( Too.Compare
                              . L.over L._1 (T.intercalate "/")
                              . unzip
                              )
                       . (\x -> pairwise x :: [[(T.Text, Too.LabelList (Map.Map T.Text T.Text))]])
                       . fmap ( L.over L._2 Too.LabelList
                              . L.over L._1 (fromMaybe "" . headMay)
                              . unzip
                              . fmap (\ !x -> (fromMaybe "" $ compLabel x, x))
                              . Too.unLabelList
                              )
                       . Too.unVarList
    getFeature = maybe 0 (either error fst . T.double) . Map.lookup feature
    grouped :: [Too.VarList (Too.LabelList (Map.Map T.Text T.Text))]
    grouped = addOthersToGroup
            . fmap ( Too.VarList
                   . fmap Too.LabelList
                   . groupBy ((==) `on` compLabel)
                   . sortBy (compare `on` compLabel)
                   )
            . groupBy ((==) `on` compVar)
            . sortBy (compare `on` compVar)
            $ ms
    compLabel = Map.lookup "statLabel"
    compVar x = (Map.lookup "Var1" x, Map.lookup "Var2" x)

-- | Add marksOther-markOther label if present to every group to compare with
-- all others.
addOthersToGroup :: [Too.VarList (Too.LabelList (Map.Map T.Text T.Text))]
                 -> [Too.VarList (Too.LabelList (Map.Map T.Text T.Text))]
addOthersToGroup varLists = fmap addToVarList varLists
  where
    addToVarList xs = fromMaybe xs $ do
      var1 <- getVarFromVarList "Var1" xs
      others <- Map.lookup var1 otherMap
      return $ xs <> others
    otherMap = otherComparisonsMap varLists

-- | Get marksOther-marksOther label if exists.
otherComparisonsMap
  :: [Too.VarList (Too.LabelList (Map.Map T.Text T.Text))]
  -> Map.Map T.Text (Too.VarList (Too.LabelList (Map.Map T.Text T.Text)))
otherComparisonsMap = Map.unions . mapMaybe getVarListMap
  where
    getVarListMap xs = do
      var1 <- getVarFromVarList "Var1" xs
      var2 <- getVarFromVarList "Var2" xs
      guard (var2 == "marksOther")
      return $ Map.singleton var1 xs

-- | Get a Var column value from a VarList
getVarFromVarList :: T.Text
                  -> Too.VarList (Too.LabelList (Map.Map T.Text T.Text))
                  -> Maybe T.Text
getVarFromVarList var xs = (headMay . Too.unVarList $ xs)
                       >>= headMay . Too.unLabelList
                       >>= Map.lookup var
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
insertSampleLabel slm file m =
  Map.insert "label" (fromMaybe "No label" label)
    . Map.insert "statLabel" statLabel
    . Map.insert "sample" sample $ m
  where
    sample = Birch.unSample $ getSample file
    alternateLabel = (\x y -> x <> "-" <> y)
                 <$> Map.lookup "Var1" m
                 <*> Map.lookup "Var2" m
    statLabel = maybe (fromMaybe "No label" alternateLabel) id label
    label = fmap Birch.unLabel
          . (=<<) (Map.lookup (Birch.Id sample) . Too.unStateLabelMap)
          $ slm

-- | Get sample from file path.
getSample :: TU.FilePath -> Birch.Sample
getSample = Birch.Sample . TU.format TU.fp . TU.dirname . p . p . p
  where
    p = TU.parent

-- | Remove columns with "marksOther".
removeOthers :: [Map.Map T.Text T.Text] -> [Map.Map T.Text T.Text]
removeOthers =
  fmap (Map.filterWithKey (\k _ -> not . T.isInfixOf "marksOther" $ k))

-- | Make sure all columns are represented in a row, unused for now.
fillColumns :: [Map.Map T.Text T.Text] -> [Map.Map T.Text T.Text]
fillColumns xs = fmap addDummy xs
  where
    addDummy = flip Map.union dummyColMap  -- Prefer original value, not dummy.
    dummyColMap = Map.fromList
                . fmap (\ !x -> (x, "NA"))
                . Set.toList
                . Set.fromList
                . concatMap Map.keys
                $ xs

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
      noLabelFlag = (== length rows')
                  . length
                  . filter (\x -> (x == Just "No label") || isNothing x)
                  . fmap (Map.lookup "label")
                  $ rows'

  (rows, stats) <- pVal feature rows'

  let pValComparisons = fmap (T.intercalate "/" . fmap (fromMaybe ""))
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
      transHref = VL.calculateAs ("'../' + datum.sample + '/projections/' + datum.sample + '_projection.html'") "url"
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
             . fmap (B.pack . T.unpack)
             . bool id (filter (not . T.isInfixOf "marksOther")) noLabelFlag
             . Set.toList
             . Set.fromList
             . concatMap Map.keys
             $ stats
  BL.writeFile (T.unpack . TU.format TU.fp $ outputStats)
    . CSV.encodeByName header
    . bool id removeOthers noLabelFlag
    $ stats
