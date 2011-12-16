This program converts an EVAN JSON program representation into compile-able
haskell code.

> module Main
>   where

> import Text.JSON

Convenience for interfacing with Text.JSON. There's no purpose to handling
errors nicely - just spit them out so the calling python (or web) code can spit
out appropriate diagnostics.

> extract (Ok i) = i
> extract (Error e) = error e

The internal storage types for the program and objects. No, we don't bother to
distinguish at the data structure level amongst Blocks, Pipes, and so forth -
that can be done with equal ease during encoding.

> type Object = Int
> type Program = [(String, Object)]

Read all JSON from standard input, returning the rawest sensible data
structure. Again, this data structure is transformed directly into the haskell
string - there's no point going through a more advanced and specialized
structure if the logic can be programmed just as easily during encoding.

> doReadJSON :: IO Program
> doReadJSON =
>   (((return . extract . decode) =<< getContents)::(IO (JSObject Int)))
>   >>= (return . fromJSObject)

> main = do
>   d <- doReadJSON
>   putStrLn $ show $ d
