> module EVAN.Tracks where

> import System.Environment (getArgs)
> import System.IO.Unsafe (unsafePerformIO)
> import Text.JSON (encode, JSON)
>
> import Data.LHA
> import Data.HEPEVT

The first and only argument shall be the name of the data file from which to
read events.

> readEvents = do
>   args <- getArgs
>   parseEventFile (args !! 0)

Repulsive as the idea is, we must use unsafePerformIO in the implementation of
_Events to avoid the need for the generated code to handle the IO monad. This
also implies the use of the NOINLINE pragma.

> {-# NOINLINE _Events #-}
> _Events :: () -> [Event]
> _Events _ = unsafePerformIO $ readEvents

Returned values are JSON-encoded.

> _Return :: JSON a => a -> IO ()
> _Return = putStrLn . encode

Each and Done do not actually exist, but need to be here.

_Each :: a -> b
_Done :: a -> b

> -- TODO make this type-safe
> select :: [a] -> [Bool] -> [a]
> select as bs = fst $ unzip $ filter (\(_, b) -> b) (zip as bs)

TODO: each :: Streamable a b => a -> [b]

> _Jets :: Event -> [Particle]
> _Jets = parts
>
> _Select :: ([a], [Bool]) -> [a]
> _Select = uncurry select

> _PID :: Particle -> Int
> _PID = partPDG

> _Energy :: Particle -> Double
> _Energy = partE

> _Mass :: Particle -> Double
> _Mass = partM


