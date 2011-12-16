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

Read all JSON from standard input, returning the rawest sensible data
structure. This data structure is transformed directly into the haskell string
- there's no point going through a more advanced and specialized structure.

> doReadJSON :: IO [(String, Int)]
> doReadJSON =
>   (((return . extract . decode) =<< getContents)::(IO (JSObject Int)))
>   >>= (return . fromJSObject)

> main = do
>   d <- doReadJSON
>   putStrLn $ show $ d
