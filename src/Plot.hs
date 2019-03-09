{-# LANGUAGE LambdaCase #-}
module Plot where


import qualified Codec.Picture.Png                         as Png
import           Control.Lens                              ((.=))
import           Control.Monad                             (forM_)
import qualified Data.ByteString.Lazy                      as LBS
import           Data.Text                                 (Text)
import qualified Data.Text                                 as Text
import qualified Diagrams.Backend.Rasterific               as BR
import qualified Diagrams.Prelude                          as D
import qualified Graphics.Rendering.Chart.Backend.Diagrams as Chart
import qualified Graphics.Rendering.Chart.Easy             as Chart
import qualified ITermShow
import           System.IO                                 (stdout)


data Output
  = Screen
  | PNG FilePath
  | SVG FilePath


newtype Title  = Title { unTitle :: Text }
newtype XLabel = XLabel { unXLabel :: Text }
newtype YLabel = YLabel { unYLabel :: Text }
data Item
  = Line Text [(Double, Double)]
  | Points Text [(Double, Double)]
data XYChart
  = XYChart
    { title  :: Title
    , xLabel :: XLabel
    , yLabel :: YLabel
    , items  :: [Item]
    }


plotXYChart :: Output -> XYChart -> IO ()
plotXYChart out chart =
  case out of
    Screen -> do
      png <- plotXYChartPNGBS chart
      LBS.hPutStr stdout (ITermShow.displayImage png)
      putStrLn ""
    PNG _ -> error "Not yet implemented" -- plotXYChartPNGFile filePath chart
    SVG _ -> error "Not yet implemented"


plotXYChartPNGBS :: XYChart -> IO LBS.ByteString
plotXYChartPNGBS chart = do
  env <- Chart.defaultEnv Chart.vectorAlignmentFns 500 375
  let dia = fst $ Chart.runBackendR env (Chart.toRenderable (xyChartEC chart))
  let img = BR.rasterRgb8 (D.mkWidth 1600) dia
  pure (Png.encodePng img)


xyChartEC :: XYChart -> Chart.Renderable ()
xyChartEC chart = Chart.toRenderable $ do
  Chart.layout_title .= (Text.unpack . unTitle . title $ chart)
  Chart.layout_x_axis . Chart.laxis_title .=
    (Text.unpack . unXLabel . xLabel $ chart)
  Chart.layout_y_axis . Chart.laxis_title .=
    (Text.unpack . unYLabel . yLabel $ chart)
  forM_ (items chart) $ \case
    Line label pts -> Chart.plot (Chart.line (Text.unpack label) [pts])
    Points label pts -> Chart.plot (Chart.points (Text.unpack label) pts)
