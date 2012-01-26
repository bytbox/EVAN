> module EVAN.Primitive
>   where

> import Data.HEPEVT

> _PID :: Particle -> Int
> _PID = pid

> _Energy :: Particle -> Double
> _Energy = energy

> _Mass :: Particle -> Double
> _Mass = mass


> _True :: () -> Bool
> _True = const True

> _False :: () -> Bool
> _False = const False

> _Repeat :: a -> [a]
> _Repeat = repeat

> _Count :: [a] -> Int
> _Count = length

