module EVAN
  where

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

_Events :: IO ()
_Events = return ()

_Return :: a -> IO ()
_Return _ = putStrLn "Done"

evanMain = do
  evts <- parseEventFile "events.dat"
  putStrLn "Hello, world!"
