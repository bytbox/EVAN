module EVAN.Output.PNG (tempPNG, pngMimeType) where

import Graphics.Rendering.Cairo
  (surfaceWriteToPNG, withImageSurface, Render(..), Surface(..), renderWith, Format(..))

import EVAN.Output.MIME
import EVAN.Output.Temp

pngMimeType = "image/png"

writeSurfaceToPNG p r s = do
                            renderWith s r
                            surfaceWriteToPNG s p

writeToPNG :: FilePath -> Int -> Int -> Render () -> IO ()
writeToPNG p w h r =
  withImageSurface FormatARGB32 w h (\s -> writeSurfaceToPNG p r s) 

tempPNG :: String -> (Int, Int) -> Render () -> IO MIMEBox
tempPNG k sz r = do
  tf <- tmpfile "png"
  writeToPNG tf (fromIntegral $ fst sz) (fromIntegral $ snd sz) r
  return $ External pngMimeType k tf

