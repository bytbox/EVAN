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

> _Lt :: Ord a => (a, a) -> Bool
> _Lt = uncurry (<)

> _Le :: Ord a => (a, a) -> Bool
> _Le = uncurry (<=)

> _Gt :: Ord a => (a, a) -> Bool
> _Gt = uncurry (>)

> _Ge :: Ord a => (a, a) -> Bool
> _Ge = uncurry (>=)

> _Eq :: Ord a => (a, a) -> Bool
> _Eq = uncurry (==)

> _Ne :: Ord a => (a, a) -> Bool
> _Ne = not . _Eq

