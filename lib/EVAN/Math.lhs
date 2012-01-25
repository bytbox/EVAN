> module EVAN.Math
>   where

> import Data.Foldable (foldl')

> _Add :: (Int, Int) -> Int
> _Add = uncurry (+)

> _Mul :: (Int, Int) -> Int
> _Mul = uncurry (*)

> _Sum :: [Int] -> Int
> _Sum = foldl' (+) 0

> _Product :: [Int] -> Int
> _Product = foldl' (*) 1
