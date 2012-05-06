module EVAN.Output.Histogram where

import qualified Data.Packed.Vector as V

import Graphics.Rendering.Cairo (Render(..))
import Graphics.Rendering.Plot

bins :: (Num n) => [n] -> ([n], [Double])
bins ns = ([1,2,3,4], [1,3,4,0])

histogram :: [Double] -> Figure ()
histogram ns = do
  setPlots 1 1
  withPlot (1,1) $ do
    setDataset (Hist, ts, [vs])
    addAxis XAxis (Side Lower) $ return ()
    addAxis YAxis (Side Lower) $ return ()
    addAxis XAxis (Value 0) $ return ()
    setRangeFromData XAxis Lower Linear
    setRangeFromData YAxis Lower Linear
  where
    bs = bins ns
    ts = V.fromList $ fst bs
    vs = V.fromList $ snd bs

renderHist :: (Int, Int) -> [Double] -> Render ()
renderHist sz = flip render sz . histogram

