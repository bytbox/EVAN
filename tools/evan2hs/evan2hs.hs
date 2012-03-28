module Main where

import Text.ParserCombinators.Parsec

skip p = p >> return ()

comment = skip $ char '['

tokEach = skip $ string "each"

statement = tokEach <|> comment

parser = statement

parseFile f c = runParser parser () f c

main = putStrLn . show . parseFile "<stdin>" =<< getContents

