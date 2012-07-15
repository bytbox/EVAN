module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Util

import EVAN.Parse

data Err = Err String

err = Left . Err

instance Show Err where
  show (Err s) = s

data Scope = Empty | Loop Scope String
  deriving Show

data Definition = PipeDef Ident [Param] [Ident]
                | LoopDef
  deriving Show

data Program = Program String (Map String (Scope, Definition))
  deriving Show

data Value =  IVal Int
            | DVal Double
            | SVal String
            | List [Value]
            | BVal Bool
  deriving (Eq)

instance Show Value where
  show (IVal i) = show i
  show (DVal d) = show d
  show (SVal s) = show s
  show (List l) = show l
  show (BVal b) = show b

{- TODO typecheck should do much more
 - check for duplicate idents (with context)
 - check actual types
 -}
typecheck :: [Statement] -> String -> Either Err Program
typecheck ss t = do
                  ps <- sequence $ map (tcPart Empty) ss
                  return . Program t $ Map.unions ps
  where
    tcPart :: Scope -> Statement -> Either Err (Map String (Scope, Definition))
    tcPart s (Assign (Ident i) (Pipe f ps as)) =
      return $ Map.fromList [(i, (s, PipeDef f ps as))]
    tcPart s (Each part (Ident slist) ss out) =
      let ns = Loop s slist in
        err "  Not yet implemented : each"

valueOfParam :: Param -> Value
valueOfParam (IParam i) = IVal i
valueOfParam (NParam f) = DVal f

resolve :: String -> Map String (Scope, Definition) -> Either Err Value
resolve t p = case t `Map.lookup` p of
                Nothing -> err $ concat ["undefined pipe: ", t]
                Just (s, PipeDef (Ident fn) ps as) ->
                  do  args <- sequence $ map (flip resolve p . identStr) as
                      resolvePipe fn ps args
                Just _ -> err "ho"
  where
    identStr (Ident i) = i
    resolvePipe :: String -> [Param] -> [Value] -> Either Err Value
    resolvePipe "Const" [p] [] = return $ valueOfParam p
    resolvePipe "Const" _ _ = err "Bad arguments to Const"
    resolvePipe "List" ps [] = return $ List $ map valueOfParam ps
    resolvePipe "List" _ _ = err "Bad arguments to List"
    resolvePipe "Id" [] [a] = return a
    resolvePipe "Id" _ _ = err "Bad arguments to Id"
    resolvePipe fn _ _ = err $ concat ["undefined block: ", fn]

execProgram :: Program -> Either Err (IO ())
execProgram (Program t p) = do
                              res <- resolve t p
                              return $ putStrLn $ show res

main = do
        str <- getContents
        let r = do
                  (ss, Ident i) <- mapLeft (Err . show) $ parseFile "<stdin>" str
                  p <- typecheck ss i
                  execProgram p
        case r of
          Left e -> putStrLn $ show e
          Right a -> a

