> module EVAN.Math
>   where

> import Data.Foldable (foldl')

> _Add :: Num a => (a, a) -> a
> _Add = uncurry (+)

> _Mul :: Num a => (a, a) -> a
> _Mul = uncurry (*)

> _Sum :: Num a => [a] -> a
> _Sum = foldl' (+) 0

> _Product :: Num a => [a] -> a
> _Product = foldl' (*) 1

> _Sub :: Num a => (a, a) -> a
> _Sub = uncurry (-)

> _Div :: Fractional a => (a, a) -> a
> _Div = uncurry (/)
