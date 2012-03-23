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

{-

module Main where

import qualified Data.Packed.Vector as V

import Graphics.Rendering.Plot

ts :: V.Vector Double
ts = V.fromList [1,2,3,4,5,7,9]

vs :: V.Vector Double
vs = V.fromList [0,1,10,40,400,6000,10430]

test_graph = do
  setPlots 1 1
  withPlot (1,1) $ do
    setDataset (Hist, ts, [vs])
    addAxis XAxis (Side Lower) $ withAxisLabel $ setText "e"
    addAxis YAxis (Side Lower) $ withAxisLabel $ setText "e / 7 GeV"
    setRangeFromData XAxis Lower Linear
    setRangeFromData YAxis Lower Log

main = writeFigure SVG "out.svg" (400, 400) test_graph

test_graph0 = do
       setPlots 1 1
       withPlot (1,1) $ do
                        setDataset (Hist, ts, [vs])
                        addAxis XAxis (Side Lower) $ withAxisLabel $ setText "time (s)"
                        addAxis YAxis (Side Lower) $ withAxisLabel $ setText "amplitude"
                        addAxis XAxis (Value 0) $ return ()
                        setRangeFromData XAxis Lower Linear
                        setRange YAxis Lower Linear (-1.25) 1.25
-}
