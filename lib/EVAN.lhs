> module EVAN
>   where

> import System (getArgs)
> import System.IO.Unsafe (unsafePerformIO)
> 
> import Data.HEPEVT

The first and only argument shall be the name of the data file from which to
read events.

> readEvents = do
>   args <- getArgs
>   parseEventFile (args !! 0)
> 
> -- TODO make this type-safe
> select :: [a] -> [Bool] -> [a]
> select as bs = fst $ unzip $ filter (\(_, b) -> b) (zip as bs)

TODO: each :: Streamable a b => a -> [b]

> _Count :: [a] -> Int
> _Count = length
>
> _Select :: ([a], [Bool]) -> [a]
> _Select = uncurry select

Repulsive as the idea is, we must use unsafePerformIO in the implementation of
_Events to avoid the need for the generated code to handle the IO monad. This
also implies the use of the NOINLINE pragma.

> {-# NOINLINE _Events #-}
> _Events :: [Event]
> _Events = unsafePerformIO $ readEvents
>
> _Return :: Show a => a -> IO ()
> _Return = putStrLn . show
