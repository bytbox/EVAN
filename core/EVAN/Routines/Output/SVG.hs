module EVAN.Output.SVG where

import Graphics.Rendering.Cairo (withSVGSurface, Render(..), renderWith)

import EVAN.Output.MIME
import EVAN.Output.Temp

svgMimeType = "image/svg+xml"

tempSVG :: String -> (Int, Int) -> Render () -> IO MIMEBox
tempSVG k sz r = do
  tf <- tmpfile "svg"
  withSVGSurface tf (fromIntegral $ fst sz) (fromIntegral $ snd sz) (\s -> renderWith s r)
  return $ External svgMimeType k tf

