module EVAN
  where

import System.IO.Unsafe (unsafePerformIO)

import Data.HEPEVT

input = []
output = return

jetCount :: Event -> Int
jetCount (_, js) = length js

-- TODO make this type-safe
select :: [a] -> [Bool] -> [a]
select as bs = fst $ unzip $ filter (\(_, b) -> b) (zip as bs)

passAll = do
  i <- input
  output i

{-# NOINLINE _Events #-}
_Events :: [Event]
_Events = unsafePerformIO $ parseEventFile "samples/events.dat"

_Return :: Show a => a -> IO ()
_Return = putStrLn . show
