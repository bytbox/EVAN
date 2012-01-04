This program converts an EVAN JSON program representation into compile-able
haskell code. This is a one-way transformation - the original JSON
representation cannot be reconstructed from the haskell string. (Doing
otherwise would require parsing haskell, which is just way too hard.)

TODO consider using template haskell for the entire thing - thus the program
would be compiled directly from the JSON in a more type-safe manner.

> module Main
>   where

> import Data.List (intercalate)
> import Data.Map (Map)
> import Data.Maybe (fromJust)
> import qualified Data.Map as Map
> import Text.JSON (
>   Result(..), JSValue(..), JSON(..), JSObject, decode, encode, fromJSObject)

Convenience for interfacing with Text.JSON. There's no purpose to handling
errors nicely - just spit them out so the calling python (or web) code can spit
out appropriate diagnostics.

> extract (Ok i) = i
> extract (Error e) = error e

We also want to transform the Maybe monad into the Result monad with a generic
error message when appropriate.

> asResult :: Maybe a -> Result a
> asResult (Just x) = Ok x
> asResult Nothing = Error "Nothing"

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
>   readJSON v =
>     let o = readJSObject v :: Result (JSObject Object) in
>       return . Program . asMap =<< (readJSObject v :: Result (JSObject Object))
>   showJSON = const JSNull

Each object may be a block, pipe, or a comment.

> data Object =

A block is an identity - the name of the function to be called - combined with
a list of inputs, each of which is a String identifying the source pipe. Also
provided is the number of outputs.

>     Block String [String] Int

A pipe just connects whatever points to it to the output of a block, specified
as a String (the id of the BLock) and an Int (the position of the output).

>   | Pipe (String, Int)

Comments aren't associated with any individual object - things like that belong
in descriptions for pipes and blocks.

>   | Comment String
>   deriving (Eq, Show)

> instance JSON Object where
>   readJSON v =
>     let o = readJSObject v :: Result (JSObject JSValue) in
>       fromJSObject =<< o
>     where
>       jsLookup :: JSON a => String -> JSObject JSValue -> Result a
>       jsLookup k v = readJSON =<< (asResult $ Map.lookup k $ asMap v)
>       lookupKind = jsLookup "kind"
>       fromJSObject :: JSObject JSValue -> Result Object
>       fromJSObject v = do
>         k <- lookupKind v
>         case k of
>           "comment" -> return . Comment =<< jsLookup "text" v
>           "block" -> do
>             ident <- jsLookup "ident" v
>             ins <- jsLookup "inputs" v
>             outCount <- jsLookup "output-count" v
>             return $ Block ident ins outCount
>           "pipe" -> return . Pipe =<< jsLookup "source" v
>           _ -> return $ Comment "UNKNOWN KIND"
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
> asHaskell p = toHaskell "Main" (fromJust . getOutputCount p) p
>   where
>     getOutputCount :: Program -> String -> Maybe Int
>     getOutputCount (Program m) id = do
>       Block _ _ r <- Map.lookup id m
>       return r

> class Haskell h where

Converting a data structure to haskell requires the id of the data structure,
hence the initial String in the type signature - implementations that don't
actually need this will just ignore it. Also used is the context, a map of ids
of blocks to the numbers of values returned.

>   toHaskell :: String -> (String -> Int) -> h -> String

Again, the `fromHaskell' method is here only as a formality.

>   fromHaskell :: String -> (String, h)

Now we go to implement the Haskell typeclass (i.e., define the `toHaskell'
method) on all of our relevant data structures.

Although we need go to no great lengths to prittify the haskell output, since
haskell /does/ care about indentation, we'll adopt the convention of using
one-space indentation throughout.

> instance Haskell Program where
>   fromHaskell = const ("", Program Map.empty)
>   toHaskell name c (Program p) = (++ "\n\n") $ intercalate "\n\n" $
>     [ "module " ++ name ++ " where"
>     , "import EVAN"
>     , "main = evanMain"
>     ] ++
>     do
>       (id, obj) <- (Map.toList p)
>       return $ toHaskell id c obj
>     where
>       pragma name ps =
>         "{-# " ++ name ++ " " ++ intercalate "," ps ++ " #-}"

Identifiers in EVAN are permitted to (in fact, expected to) have upper-case
names; however, for haskell, we must prefix all non-typename identifiers with
an underscore.

> instance Haskell Object where
>   fromHaskell = const ("", Comment "")
>   toHaskell _ _ (Comment str) = "{- " ++ str ++ " -}"
>   toHaskell id _ (Block ident ins oc) =
>     "_" ++ id ++ " = _" ++ ident ++ " " ++
>       intercalate " " (map (\x -> '_':x) ins) ++ ""
>   toHaskell id c (Pipe (sId, sNum)) =
>     intercalate " "
>       [ "_" ++ id
>       , "="
>       , nth (lookupReturnCount sId) sNum
>       , "_" ++ sId
>       ]
>     where
>       lookupReturnCount id = c id

> parenthesize :: String -> String
> parenthesize s = "(" ++ s ++ ")"

Creating a lambda expression to get the Nth element out of an M-tuple is ugly,
but easy.

> nth :: Int -> Int -> String
> nth m n = parenthesize $ intercalate " " $ 
>   [ "\\"
>   , (parenthesize $ intercalate "," $
>       replicate n "_" ++ ["x"] ++ replicate (m-n-1) "_")
>   , "->"
>   , "x"
>   ]

> main = putStr . asHaskell . readProgram =<< getContents
