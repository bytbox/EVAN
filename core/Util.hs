module Util where

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left a) = Left $ f a
mapLeft _ (Right x) = Right x

mapRight :: (b -> c) -> Either a b -> Either a c
mapRight f (Right a) = Right $ f a
mapRight _ (Left x) = Left x

