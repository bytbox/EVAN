> module EVAN.Tracks where

> import System.Environment (getArgs)
> import System.FilePath (takeExtension)
> import System.IO.Unsafe (unsafePerformIO)
>
> import Data.LHA
> import qualified Data.LHE as LHE
> import qualified Data.HEPEVT as HEPEVT

The first and only argument shall be the name of the data file from which to
read events.

> readEvents = do
>   args <- getArgs
>   let fn = args !! 0
>   case takeExtension fn of
>     ".dat"  -> HEPEVT.parseEventFile fn
>     ".lhe"  -> return . snd =<< LHE.parseFile fn
>     _       -> fail "Unrecognized file format"

Repulsive as the idea is, we must use unsafePerformIO in the implementation of
_Events to avoid the need for the generated code to handle the IO monad. This
also implies the use of the NOINLINE pragma.

> {-# NOINLINE _Events #-}
> _Events :: () -> () -> [Event]
> _Events () () = unsafePerformIO $ readEvents

> _Tracks :: () -> Event -> [Particle]
> _Tracks () = parts

> _PID :: () -> Particle -> Int
> _PID () = partPDG

> _Energy :: () -> Particle -> Double
> _Energy () = partE

> _Mass :: () -> Particle -> Double
> _Mass () = partM


