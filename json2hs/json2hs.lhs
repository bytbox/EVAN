This program converts an EVAN JSON program representation into compile-able
haskell code. This is a one-way transformation - the original JSON
representation cannot be reconstructed from the haskell string. (Doing
otherwise would require parsing haskell, which is just way too hard.)

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

The program is represented in JSON as a mapping from ids (strings) to objects.
The meaning of the ids is intuitively obvious only for pipes - for comments and
blocks, it's roughly arbitrary, and won't end up in the generated haskell code.

> data Program = Program (Map String Object)
>   deriving (Eq, Show)

> instance JSON Program where
>   readJSON v = return . Program . asMap =<< readJSObject v
>   showJSON = const JSNull

Each object may be a block, pipe, or a comment.

> data Object =
>     Block
>   | Pipe

Comments aren't associated with any individual object - things like that belong
in descriptions for pipes and blocks.

>   | Comment String
>   deriving (Eq, Show)

> instance JSON Object where
>   readJSON v = return $ Comment "Hello, world"
>   showJSON = const JSNull

Note that the JSON representation we are given will also contain other data,
mostly used by the frontend python etc. for display purposes. We won't bother
to handle this, since there's no reason for it to make its way into the haskell
code. (The comments should be transferred, since our goal is for the haskell to
be usable on its own.)

Parse the given JSON, returning a constructed Program object.

> readProgram :: String -> Program
> readProgram = extract . decode

Convert a program in the post-JSON data structure to a string representing a
full haskell program ready for compilation. This just means calling out to the
appropriate method on the program data structure.

> asHaskell :: Program -> String
> asHaskell = toHaskell

> class Haskell h where
>   toHaskell :: h -> String

Now we go to implement the Haskell typeclass (i.e., define the 'toHaskell'
method) on all of our relevant data structures.

> instance Haskell Program where
>   toHaskell = const "module Main where\n"

> main = putStr . asHaskell . readProgram =<< getContents
