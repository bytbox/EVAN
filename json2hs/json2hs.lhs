> {-# LANGUAGE FlexibleInstances #-}

This program converts an EVAN JSON program representation into compile-able
haskell code.

> module Main
>   where

> import Data.Map (Map)
> import qualified Data.Map as Map
> import Text.JSON (
>   Result(..), JSValue(..), JSON(..), JSObject, decode, encode, fromJSObject)

Convenience for interfacing with Text.JSON. There's no purpose to handling
errors nicely - just spit them out so the calling python (or web) code can spit
out appropriate diagnostics.

> extract (Ok i) = i
> extract (Error e) = error e

Convenience for extracting JSON objects, instead of attempting to extract a
list of pairs. I'm not sure why Text.JSON insists on the latter behaviour, but
it's most definitely not right for us.

> readJSObject :: JSON a => JSValue -> Result (JSObject a)
> readJSObject = readJSON

Transform a JSObject into the appropriate map.

> asMap :: JSObject a -> Map String a
> asMap = Map.fromList . fromJSObject

The internal storage types for the program. All of these types claim to be JSON
instances, but in reality, none bother to implement showJSON properly, because
we're not going to be outputting any JSON.

> data Program = Program (Map String Int)
>   deriving (Eq, Show)

> instance JSON Program where
>   readJSON v = do
>     return . Program . asMap =<< readJSObject v
>   showJSON p = JSNull

Parse the given JSON, returning a constructed Program object.

> readProgram :: String -> Program
> readProgram = extract . decode

Convert a program in the post-JSON data structure to a string representing a
full haskell program ready for compilation.

> asHaskell :: Program -> String
> asHaskell = show

> main = do
>   putStrLn . show . readProgram =<< getContents
