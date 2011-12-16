module Main where

import Data.HEPEVT

import EVAN

main = do
  evts <- parseEventFile "events.dat"
  putStrLn $ show $ length $ select evts $ do
    event <- evts
    let jc = jetCount event
    return (jc > 180)
  putStrLn "Bye"
