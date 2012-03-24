> module EVAN.Primitive
>   where

Each and Done do not actually exist, but need to be here.

_Each :: () -> a -> b
_Done :: () -> a -> b

> -- TODO make this type-safe
> select :: () -> [a] -> [Bool] -> [a]
> select () as bs = fst $ unzip $ filter (\(_, b) -> b) (zip as bs)

TODO: each :: () -> Streamable a b => a -> [b]

> _True :: () -> () -> Bool
> _True () = const True

> _False :: () -> () -> Bool
> _False () = const False

> _Both :: () -> (Bool, Bool) -> Bool
> _Both () = uncurry (&&)

> _Either :: () -> (Bool, Bool) -> Bool
> _Either () = uncurry (||)

> _Neither :: () -> (Bool, Bool) -> Bool
> _Neither () = not . _Either ()

> _Not :: () -> Bool -> Bool
> _Not () = not

> _Repeat :: () -> a -> [a]
> _Repeat () = repeat

> _Count :: () -> [a] -> Int
> _Count () = length

> _Const :: Num n => n -> () -> n
> _Const a () = a

