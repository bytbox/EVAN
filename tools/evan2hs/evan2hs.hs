module Main where

import Text.EVAN

main = putStrLn . show . parseFile "<stdin>" =<< getContents

