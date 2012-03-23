module EVAN.Output.SVG where

import Graphics.Rendering.Cairo (withSVGSurface, Render(..), renderWith)

import EVAN.Output.MIME
import EVAN.Output.Temp

svgMimeType = "image/svg+xml"

tempSVG :: String -> Render () -> IO MIMEBox
tempSVG k r = do
  tf <- tmpfile
  withSVGSurface tf 72 72 (\s -> renderWith s r)
  return $ External svgMimeType k tf

