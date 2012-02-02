> module EVAN.Math
>   where

> import Data.Foldable (foldl')

! Add a b -> c
Add adds the two given numbers.

> _Add :: Num a => (a, a) -> a
> _Add = uncurry (+)

! Mul a b -> c
Mul multiplies the two given numbers.

> _Mul :: Num a => (a, a) -> a
> _Mul = uncurry (*)

! Sum l -> x
Sum sums all numbers in the given stream.

> _Sum :: Num a => [a] -> a
> _Sum = foldl' (+) 0

! Product l -> x
Product multiplies all numbers in the given stream.

> _Product :: Num a => [a] -> a
> _Product = foldl' (*) 1

! Sub a b -> c
Sub subtracts b from a.

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

