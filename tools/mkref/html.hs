module Main where

import EVAN.Documentation
import EVAN.Documentation.HTML
import System.IO (hPutStr, stderr)

outerr = hPutStr stderr

main :: IO ()
main = getContents >>= return . readDocs >>= output
  where
    output (Left d) = putStrLn $ showHTML d
    output (Right e) = outerr e

