> module EVAN
>   where

Repulsive as the idea is, we must use unsafePerformIO in the implementation of
_Events to avoid the need for the generated code to handle the IO monad.

> import System.IO.Unsafe (unsafePerformIO)
> 
> import Data.HEPEVT
> 
> -- TODO make this type-safe
> select :: [a] -> [Bool] -> [a]
> select as bs = fst $ unzip $ filter (\(_, b) -> b) (zip as bs)
>
> _Count :: [a] -> Int
> _Count = length
> 
> {-# NOINLINE _Events #-}
> _Events :: [Event]
> _Events = unsafePerformIO $ parseEventFile "samples/events.dat"
> 
> _Return :: Show a => a -> IO ()
> _Return = putStrLn . show
