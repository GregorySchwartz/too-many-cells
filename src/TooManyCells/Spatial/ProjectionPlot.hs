{- TooManyCells.Spatial.ProjectionPlot
Gregory W. Schwartz

Collects functions pertaining to plotting interactive figures of point proximity
on the left with cumulative distribution functions of features on the right.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}

module TooManyCells.Spatial.ProjectionPlot
    ( plotSpatialProjection
    ) where

-- Remote
import BirchBeer.Types (Feature (..), LabelMap (..), Label (..), getColNames, Id (..), Sample (..))
import Data.Bool (bool)
import Data.Colour.Palette.BrewerSet (Kolor, brewerSet, ColorCat (..) )
import Data.Colour.Palette.Harmony (colorRamp)
import Data.Colour.SRGB (sRGB24show)
import Data.List (genericLength)
import Data.Maybe (fromMaybe, isJust, catMaybes)
import qualified Control.Foldl as Fold
import qualified Control.Lens as L
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Vector as V
import qualified Graphics.Vega.VegaLite as VL
import qualified Graphics.Vega.VegaLite.Theme as VL
import qualified System.FilePath as FP
import qualified Turtle as TU

-- Local
import TooManyCells.File.Types (OutputDirectory (..))
import TooManyCells.Matrix.Types (SingleCells (..), ProjectionMap (..), X (..), Y (..))
import TooManyCells.Spatial.Types (ColorMap (..), Range (..))
import TooManyCells.Spatial.Utility

-- | Get the minimum and maximum values for a projection map.
getMinMax :: ProjectionMap -> ((Double, Double), (Double, Double))
getMinMax =
  fromMaybe ((0, 0), (0, 0))
    . (L.sequenceOf L.both :: (Maybe (Double, Double), Maybe (Double, Double)) -> Maybe ((Double, Double), (Double, Double)))  -- m ((a, b), (c, d))
    . (L.over L.both (L.sequenceOf L.both) :: ((Maybe Double, Maybe Double), (Maybe Double, Maybe Double)) -> (Maybe (Double, Double), Maybe (Double, Double)))  -- (m (a, b), m (b, c))
    . (L.over L.both (Fold.fold ((,) <$> Fold.minimum <*> Fold.maximum)) :: ([Double], [Double]) -> ((Maybe Double, Maybe Double), (Maybe Double, Maybe Double))) -- ((m a, m b), (m c, m d))
    . (L.over (L._2 . L.mapped) unY :: ([Double], [Y]) -> ([Double], [Double]) )
    . L.over (L._1 . L.mapped) unX
    . unzip
    . fmap snd
    . Map.elems
    . unProjectionMap

-- | Get the color mapping from feature to color.
getColorMap :: [Feature] -> ColorMap
getColorMap xs =
  ColorMap . Map.fromList . zip xs . colorRamp (length xs) . brewerSet Set1 $ 9

-- | Get the feature selection of a distribution.
featureSel :: Feature -> [VL.SelectSpec] -> VL.PropertySpec
featureSel (Feature f) = VL.selection
                       . VL.select
                           ("pick_" <> f)
                           VL.Interval [VL.Encodings [VL.ChX], VL.Empty]

-- | Color by features.
colorByFeatures
  :: Maybe LabelMap -> ColorMap -> [Feature] -> VL.BuildEncodingSpecs
colorByFeatures lm colorMap features =
        VL.color ( maybe  -- Choose whether to color by labels or features
                    [ VL.MDataCondition
                        featureSels
                        [ VL.MString "white" ]
                    ]
                    (\ x
                    -> [ VL.MName "label", VL.MmType VL.Nominal, labelColorScale x]
                    )
                    lm
                  )
  where
    featureSels = fmap
                    (\ f
                    -> ( VL.Selection ("pick_" <> unFeature f)
                       ,  [ VL.MString
                              ( maybe "white" (T.pack . sRGB24show)
                              . Map.lookup f
                              $ unColorMap colorMap
                              )
                          ]
                        )
                    )
                    features


-- | Pseudo-legend for selection.
legendSpec :: LabelMap -> VL.VLSpec
legendSpec lm =
  VL.asSpec [ VL.mark VL.Circle []
            , selection []
            , encoding []
            ]
  where
    selection = VL.selection
              . VL.select
                  "pick_legend"
                  VL.Multi
                  [ VL.Fields [ "label" ], VL.Empty ]
    labels = Set.toAscList
           . Set.fromList
           . fmap unLabel
           . Map.elems
           . unLabelMap
           $ lm
    encoding =
      VL.encoding
        . VL.position VL.Y [ VL.PName "label", VL.PmType VL.Nominal ]
        . VL.color [ VL.MName "label"
                   , VL.MmType VL.Nominal
                   , labelColorScale lm
                   ,  VL.MLegend []
                   ]

-- | Get the circle spec for plotting.
getCircleSpec :: Maybe LabelMap -> Range -> ColorMap -> [Feature] -> VL.VLSpec
getCircleSpec lm range colorMap features =
  VL.asSpec [ VL.mark VL.Circle []
            , VL.width plotWidth
            , VL.height plotHeight
            , circleEnc []
            , circleFilterTrans []
            ]
  where
    aspectRatio = (maxY range - minY range) / (maxX range - minX range)
    plotWidth = 800
    plotHeight = aspectRatio * plotWidth
    circleEnc =
      VL.encoding
        . VL.position VL.X [ VL.PName "x"
                            , VL.PmType VL.Quantitative
                            , VL.PAxis [ VL.AxTitle "X Axis" ]
                            , VL.PScale
                                [ VL.SDomain (VL.DNumbers [minX range, maxX range])
                                ]
                            ]
        . VL.position VL.Y [ VL.PName "y"
                            , VL.PmType VL.Quantitative
                            , VL.PAxis [ VL.AxTitle "Y Axis" ]
                            , VL.PScale
                                [ VL.SDomain (VL.DNumbers [minY range, maxY range])
                                ]
                            ]
        . colorByFeatures lm colorMap features
    circleFilterTrans =
      VL.transform
        . VL.filter
            ( VL.FCompose
                ( bool (VL.Expr "false") (VL.Selection "pick_legend") (isJust lm)
          `VL.Or` F.foldl'
                  (\acc x -> VL.Or acc (VL.Selection $ "pick_" <> unFeature x))
                  (VL.Expr "false")
                  features
                )
            )

-- | Get a window spec of a feature for plotting.
getWindowSpec :: ColorMap -> Feature -> VL.VLSpec
getWindowSpec colorMap (Feature feature) =
  VL.asSpec [ VL.title feature []
            , VL.height 40
            , windowEnc []
            , windowTrans []
            , featureSel (Feature feature) []
            , VL.mark VL.Area []
            ]
  where
    windowTrans =
      VL.transform
        . VL.filter (VL.FExpr $ "datum." <> feature <> " > 0")
        . VL.window
            [([VL.WOp VL.CumeDist, VL.WField feature], "window_" <> feature)]
            [VL.WFrame Nothing (Just 0), VL.WSort [VL.WAscending feature]]
    windowEnc = VL.encoding
               . VL.position VL.X [ VL.PName $  feature
                                  , VL.PmType VL.Quantitative
                                  , VL.PAxis [ VL.AxTitle "Expression"]
                                  ]
               . VL.position VL.Y [ VL.PName $ "window_" <> feature
                                  , VL.PmType VL.Quantitative
                                  , VL.PAxis [ VL.AxTitle "Probability"]
                                  ]
               . VL.color
                  [ VL.MString ( maybe "white" (T.pack . sRGB24show)
                               . Map.lookup (Feature feature)
                               $ unColorMap colorMap
                               )
                  ]

-- | The color scheme for the label field.
labelColorScale :: LabelMap -> VL.MarkChannel
labelColorScale lm = VL.MScale [ VL.SDomain (VL.DStrings labels)
                               , VL.SRange (VL.RStrings colors)
                               ]
  where
    labels =
      Set.toAscList . Set.fromList . fmap unLabel . Map.elems . unLabelMap $ lm
    colors =
      fmap (T.pack . sRGB24show) . colorRamp (length labels) . brewerSet Set1 $ 9

plotSpatialProjection :: OutputDirectory
                      -> Maybe LabelMap
                      -> ProjectionMap
                      -> SingleCells
                      -> Sample
                      -> IO ()
plotSpatialProjection outputDir' labelMap' pm sc sample = do
  let labelMap = LabelMap . Map.insert (Id "") (Label "NA") . unLabelMap
             <$> labelMap'  -- Insert dummy cell to account for NA labels.
      dataSet = scToVLData labelMap pm sc
      features = fmap Feature . V.toList . getColNames $ sc
      ((miX, maX), (miY, maY)) = getMinMax pm
      range = Range miX maX miY maY
      numWindowCols = ceiling . sqrt . genericLength $ features

      colorMap = getColorMap features
      allSelections =
        ( VL.FilterOp
        $ VL.FCompose
            ( F.foldl'
              (\acc x -> VL.And acc (VL.Selection $ "pick_" <> unFeature x))
              (VL.Expr "true")
              features
            )
        , [VL.MString "white"]
        )

      circleSpec = getCircleSpec labelMap range colorMap features
      windowSpecs = fmap (getWindowSpec colorMap) features
      allSpec = VL.hConcat
              $ [ maybe
                    circleSpec
                    (\ lm
                    -> VL.asSpec
                        [VL.columns 2, VL.vlConcat [circleSpec, legendSpec lm]]
                    )
                    labelMap
                , VL.asSpec [VL.columns numWindowCols, VL.vlConcat windowSpecs]
                ]

      p = VL.toVegaLite
            $ [ dataSet
              , VL.theme VL.defaultConfig []
              , allSpec
              ]

  TU.mktree . TU.fromText . T.pack $ unOutputDirectory outputDir'
  let fileName (Sample x) = T.unpack x <> "_projection.html"
      outputPath = unOutputDirectory outputDir' FP.</> fileName sample
  VL.toHtmlFile outputPath p
