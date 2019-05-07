{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Plot where


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
import qualified Diagrams.Backend.SVG                      as SVG
import           Diagrams.Prelude                          (( # ))
import qualified Diagrams.Prelude                          as D
import qualified Diagrams.TwoD.Text                        as D
import qualified Graphics.Rendering.Chart.Backend.Diagrams as Chart
import qualified Graphics.Rendering.Chart.Easy             as Chart
import qualified ITermShow
import           System.IO                                 (stdout)


data Output
  = Screen
  | PNG FilePath
  | SVG FilePath
  | PGF FilePath


data XYChart
  = XYChart
    { title      :: !Title
    , xLabel     :: !XLabel
    , yLabel     :: !YLabel
    , chartItems :: [Item Double Double]
    }
newtype Title = Title { unTitle :: Text } deriving (IsString)
newtype XLabel = XLabel { unXLabel :: Text } deriving (IsString)
newtype YLabel = YLabel { unYLabel :: Text } deriving (IsString)
data Item x y
  = Line Text [(x, y)]
  | Points Text [(x, y)]


xyChartUnits
  :: forall dimx dimy unitx unity l.
     ( DMV.ValidDLU dimx l unitx
     , DMV.ValidDLU dimy l unity )
  => Output
  -> Title
  -> XLabel
  -> YLabel
  -> (unitx, unity)
  -> [Item (DMV.Qu dimx l Double) (DMV.Qu dimy l Double)]
  -> IO ()
xyChartUnits out t x y (ux, uy) items
  = xyChart out t x y (itemInUnits (ux, uy) <$> items)


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


xyChart
  :: Output
  -> Title
  -> XLabel
  -> YLabel
  -> [Item Double Double]
  -> IO ()
xyChart out t x y items =
  let
    chart = XYChart t x y items
  in case out of
       Screen -> do
         png <- plotXYChartPNGBS chart
         LBS.hPutStr stdout (ITermShow.displayImage png)
         putStrLn ""
       PGF filePath -> plotXYChartPGF filePath chart
       PNG _ -> error "Not yet implemented"
       SVG _ -> error "Not yet implemented"


plotXYChartPNGBS :: XYChart -> IO LBS.ByteString
plotXYChartPNGBS chart = do
  env <- Chart.defaultEnv Chart.vectorAlignmentFns 500 375
  let dia = fst $ Chart.runBackendR env (Chart.toRenderable (xyChartEC chart))
  let img = BR.rasterRgb8 (D.mkWidth 1600) dia
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


data OrbitSystem
  = OrbitSystem
    { planet :: Planet
    , systemItems :: [OrbitSystemItem]
    }

data Planet
  = Planet
    { name :: Text
    , radius :: Double
    , color :: Colour Double
    }
  
data OrbitSystemItem
  = Trajectory
    { points :: [(Double, Double)]
    , color :: Colour Double
    }

plotOrbitSystem :: Output -> Double -> OrbitSystem -> IO ()
plotOrbitSystem output vScale system =
  case output of
    Screen -> do
      let png = plotOrbitSystemPNGBS vScale system
      LBS.hPutStr stdout (ITermShow.displayImage png)
      putStrLn ""
    PGF filePath -> do
      let
        dia = mconcat (plotSystemItem output vScale (planet system) <$> systemItems system)
              <> plotPlanet (PGF filePath) (planet system)
        -- framed = D.bgFrame 400 D.white dia
      PGF.renderPGF filePath (D.mkWidth 400) dia
    PNG _ -> error "Not yet implemented"
    SVG filePath -> do
      let
        dia = mconcat (plotSystemItem output vScale (planet system) <$> systemItems system)
              <> plotPlanet (SVG filePath) (planet system)
        framed = D.bgFrame 400 D.white dia
      SVG.renderSVG filePath (D.mkWidth 1024) framed


plotOrbitSystemPNGBS :: Double -> OrbitSystem -> LBS.ByteString
plotOrbitSystemPNGBS vScale system =
  let
    dia = mconcat (plotSystemItem (PNG undefined) vScale (planet system) <$> systemItems system)
          <> plotPlanet (PNG undefined) (planet system)
    diaFramed = D.bgFrame 400 D.white dia
    img = BR.rasterRgb8 (D.dims2D 1200 1200) diaFramed
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
  = D.fromVertices (D.p2 . fancyScale vScale pRadius <$> pts) # D.lc c # D.pad 1.1


getFontSize :: Output -> D.Measured Double Double
getFontSize (PNG _) = D.output 30.0
getFontSize (PGF _) = D.output 12.0
getFontSize _       = error "Not Implemented"


fancyScale :: Double -> Double -> (Double, Double) -> (Double, Double)
fancyScale vScale rad (x, y) =
  let
    r = sqrt ((x*x) + (y*y))
    theta = atan2 y x

    r' = (r - rad) * vScale + rad
    x' = r' * cos theta
    y' = r' * sin theta
  in
    (x', y')
