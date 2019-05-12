{-|
Module      : Plot
Description : Abstracted plotting capabilities.

This module adds a thin layer of abstraction on top of the Chart library. It
has served well as a way to switch between Chart and gnuplot during
development of the workshop.
-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NegativeLiterals           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Plot
  ( -- * XY Charts
    -- ** Types
    Output (Screen, PNG, PGF)
  , XYChart (title, xLabel, yLabel, chartItems)
  , Title(Title)
  , XLabel(XLabel)
  , YLabel(YLabel)
  , XYOption(XRange)
  , Item(Line, Points)
    -- ** Functions
  , xyChart
  , xyChartUnits
    -- * Orbit system plot
    -- ** Types
  , OrbitSystem(OrbitSystem, planet, systemItems)
  , Planet(Planet, name, radius, color)
  , OrbitSystemItem(Trajectory, points, color, AltitudeCircle, altitude,
                    altLabel)
    -- ** Functions
  , plotOrbitSystem
  ) where

import qualified Codec.Picture.Png                         as Png
import           Control.Lens                              ((.=))
import           Control.Monad                             (forM_)
import qualified Data.ByteString.Lazy                      as LBS
import           Data.Colour                               (Colour)
import qualified Data.Metrology.Vector                     as DMV
import           Data.String                               (IsString)
import           Data.Text                                 (Text)
import qualified Data.Text                                 as Text
import           Data.VectorSpace                          (Scalar, VectorSpace)
import qualified Diagrams.Backend.PGF                      as PGF
import qualified Diagrams.Backend.Rasterific               as BR
import           Diagrams.Prelude                          (( # ))
import qualified Diagrams.Prelude                          as D
import qualified Diagrams.TwoD.Text                        as D
import qualified Graphics.Rendering.Chart.Backend.Diagrams as Chart
import qualified Graphics.Rendering.Chart.Easy             as Chart
import qualified ITermShow
import           System.IO                                 (stdout)


-- | Output location for a plot.
data Output
  = Screen        -- ^ Output to the screen (ITermShow and wshterm)
  | PNG FilePath  -- ^ Save to a PNG file
  | PGF FilePath  -- ^ Save to a PGF file (for LaTeX)


-- | Data for an xy-chart.
--
-- An @XYChart@ is a chart on x-y orthogonal axes.
data XYChart
  = XYChart
    { -- | Title for the chart.
      title        :: !Title
      -- | Label for the x-axis.
    , xLabel       :: !XLabel
      -- | Label for the y-axis.
    , yLabel       :: !YLabel
      -- | Options for the chart.
    , chartOptions :: [XYOption Double Double]
      -- | Items in the chart.
    , chartItems   :: [Item Double Double]
    }


newtype Title = Title { unTitle :: Text } deriving (IsString)
newtype XLabel = XLabel { unXLabel :: Text } deriving (IsString)
newtype YLabel = YLabel { unYLabel :: Text } deriving (IsString)


-- | Items that can appear in an 'XYChart'.
data Item x y
  = Line Text [(x, y)]     -- ^ A plotted line.
  | Points Text [(x, y)]   -- ^ A set of plotted points.


-- | Options for an XYChart.
data XYOption x y
  = XRange !x !x    -- ^ Specify a range for the x-axis.


-- | Plot an 'XYChart'.
xyChart
  :: Output                    -- ^ Output location.
  -> Title                     -- ^ Title of the chart.
  -> XLabel                    -- ^ X-axis label.
  -> YLabel                    -- ^ Y-axis label.
  -> [XYOption Double Double]  -- ^ Options for the plot.
  -> [Item Double Double]      -- ^ List of items to plot.
  -> IO ()                     -- ^ IO action.
xyChart out t x y options items =
  let chart = XYChart t x y options items
  in case out of
       Screen -> do
         pngBS <- plotXYChartPNGBS chart
         LBS.hPutStr stdout (ITermShow.displayImage pngBS)
         putStrLn ""

       PGF filePath -> plotXYChartPGF filePath chart

       PNG filePath -> do
         pngBS <- plotXYChartPNGBS chart
         LBS.writeFile filePath pngBS


-- | Plot an 'XYChart' whose plot items contain units.
xyChartUnits
  :: forall dimx dimy unitx unity l.
     ( DMV.ValidDLU dimx l unitx
     , DMV.ValidDLU dimy l unity )
  => Output           -- ^ Output location.
  -> Title            -- ^ Title of the chart.
  -> XLabel           -- ^ X-axis label.
  -> YLabel           -- ^ Y-axis label.
  -> (unitx, unity)   -- ^ Units for the x and y axes.
  -> [Item (DMV.Qu dimx l Double) (DMV.Qu dimy l Double)]
                      -- ^ List of items to plot.
  -> IO ()            -- ^ IO action.
xyChartUnits out t x y (ux, uy) items
  = xyChart out t x y [] (itemInUnits (ux, uy) <$> items)


itemInUnits
  :: forall dimx dimy unitx unity l n.
     ( DMV.ValidDLU dimx l unitx
     , DMV.ValidDLU dimy l unity
     , VectorSpace n, Fractional (Scalar n) )
  => (unitx, unity)
  -> Item (DMV.Qu dimx l n) (DMV.Qu dimy l n)
  -> Item n n
itemInUnits (ux, uy) item =
  let
    inu (x, y) = (x DMV.# ux, y DMV.# uy)
  in
    case item of
      Line t xys   -> Line t (inu <$> xys)
      Points t xys -> Points t (inu <$> xys)


plotXYChartPNGBS :: XYChart -> IO LBS.ByteString
plotXYChartPNGBS chart = do
  env <- Chart.defaultEnv Chart.vectorAlignmentFns 500 375
  let dia = fst $ Chart.runBackendR env (Chart.toRenderable (xyChartEC chart))
  let img = BR.rasterRgb8 (D.mkWidth 1200) dia
  pure (Png.encodePng img)


plotXYChartPGF :: FilePath -> XYChart -> IO ()
plotXYChartPGF filePath chart = do
  env <- Chart.defaultEnv Chart.vectorAlignmentFns 500 375
  let dia = fst $ Chart.runBackendR env (Chart.toRenderable (xyChartEC chart))
  PGF.renderPGF filePath (D.mkWidth 500) dia


xyChartEC :: XYChart -> Chart.Renderable ()
xyChartEC chart = Chart.toRenderable $ do
  Chart.layout_title .= (Text.unpack . unTitle . title $ chart)
  Chart.layout_x_axis . Chart.laxis_title .=
    (Text.unpack . unXLabel . xLabel $ chart)
  Chart.layout_y_axis . Chart.laxis_title .=
    (Text.unpack . unYLabel . yLabel $ chart)
  forM_ (chartItems chart) $ \case
    Line label pts -> Chart.plot (Chart.line (Text.unpack label) [pts])
    Points label pts -> Chart.plot (Chart.points (Text.unpack label) pts)
  forM_ (chartOptions chart) $ \case
    XRange minX maxX -> Chart.layout_x_axis . Chart.laxis_generate .=
      Chart.scaledAxis Chart.def (minX, maxX)


-- | Data for an orbital system plot.
--
-- An orbital system plot typically contains a planet or moon and trajectories.
data OrbitSystem
  = OrbitSystem
    { planet      :: Planet
    , systemItems :: [OrbitSystemItem]
    }


-- | Planet data.
data Planet
  = Planet
    { name   :: Text
    , radius :: Double
    , color  :: Colour Double
    }


-- | Items that can appear in an orbit system plot.
data OrbitSystemItem
  = Trajectory
    { points :: [(Double, Double)]
    , color  :: Colour Double
    }
  | AltitudeCircle
    { altitude :: Double
    , altLabel :: Text
    , color    :: Colour Double
    }


-- | Plots an orbit system.
plotOrbitSystem
  :: Output        -- ^ Output location.
  -> Double        -- ^ Vertical scale factor for the altitude.
  -> OrbitSystem   -- ^ OrbitSystem to plot.
  -> IO ()         -- ^ IO action.
plotOrbitSystem output vScale system =
  case output of
    Screen -> do
      let pngBS = plotOrbitSystemPNGBS vScale system
      LBS.hPutStr stdout (ITermShow.displayImage pngBS)
      putStrLn ""

    PGF filePath -> do
      let
        dia = mconcat (plotSystemItem output vScale (planet system) <$> systemItems system)
              <> plotPlanet (PGF filePath) (planet system)
      PGF.renderPGF filePath (D.mkWidth 400) dia

    PNG filePath -> do
      let pngBS = plotOrbitSystemPNGBS vScale system
      LBS.writeFile filePath pngBS


plotOrbitSystemPNGBS :: Double -> OrbitSystem -> LBS.ByteString
plotOrbitSystemPNGBS vScale system =
  let
    dia = mconcat (plotSystemItem (PNG undefined) vScale (planet system) <$> systemItems system)
          <> plotPlanet (PNG undefined) (planet system)
    diaFramed = D.bgFrame 400 D.white dia
    img = BR.rasterRgb8 (D.dims2D 1000 1000) diaFramed
  in
    Png.encodePng img


plotPlanet
  :: ( D.Renderable (D.Path D.V2 Double) b
     , D.Renderable (D.Text Double) b )
  => Output
  -> Planet
  -> D.QDiagram b D.V2 Double D.Any
plotPlanet output (Planet pName pRadius pColor)
  = D.text (Text.unpack pName)
    # D.fontSize (getFontSize output)
    # D.fc D.black
 <> D.circle pRadius
    # D.fc pColor


plotSystemItem
  :: ( D.Renderable (D.Path D.V2 Double) b
     , D.Renderable (D.Text Double) b )
  => Output
  -> Double
  -> Planet
  -> OrbitSystemItem
  -> D.QDiagram b D.V2 Double D.Any
plotSystemItem _ vScale (Planet _ pRadius _) (Trajectory pts c)
  = D.fromVertices (D.p2 . altitudeScale vScale pRadius <$> pts) # D.lc c # D.pad 1.1
plotSystemItem output vScale (Planet _ pRadius _) (AltitudeCircle alt lbl c)
  = D.circle (scalarAltitudeScale vScale pRadius alt)
    # D.lc c
    # D.lwO 1
    # D.dashingN [0.01, 0.01] 0
 <> D.alignedText 0.5 (-1.0) (Text.unpack lbl)
    # D.fontSize (0.6 * getFontSize output)
    # D.fc D.gray
    # D.translateY (scalarAltitudeScale vScale pRadius alt)


-- | Get font size for an OrbitSystem plot.
--
-- "Good" font sizes seem to depend on our output device, so here we can
-- switch between different font sizes depending on the output device.
getFontSize :: Output -> D.Measured Double Double
getFontSize Screen  = D.output 30.0
getFontSize (PNG _) = D.output 30.0
getFontSize (PGF _) = D.output 12.0


-- | Perform vertical scaling of altitude distance on a vector.
altitudeScale :: Double -> Double -> (Double, Double) -> (Double, Double)
altitudeScale vScale rad (x, y) =
  let
    r = sqrt ((x*x) + (y*y))
    theta = atan2 y x

    r' = scalarAltitudeScale vScale rad r
    x' = r' * cos theta
    y' = r' * sin theta
  in
    (x', y')


-- | Perform vertical scaling of altitude distance.
scalarAltitudeScale :: Double -> Double -> Double -> Double
scalarAltitudeScale vScale planetRadius r
  = vScale * (r - planetRadius) + planetRadius

