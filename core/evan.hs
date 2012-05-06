module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Text.EVAN
import Util

data Err = Err String

err = Left . Err

instance Show Err where
  show (Err s) = s

data Scope = Empty | Loop Scope String
  deriving Show

data Definition = PipeDef [Ident] [Param] [Ident]
                | LoopDef

data Program = Program String (Map String (Scope, Statement))
  deriving Show

{- TODO typecheck should do much more
 - check for duplicate idents (with context)
 - check actual types
 -}
typecheck :: [Statement] -> String -> Either Err Program
typecheck ss t = do
                  ps <- sequence $ map (tcPart Empty) ss
                  return . Program t $ Map.unions ps
  where
    tcPart :: Scope -> Statement -> Either Err (Map String (Scope, Statement))
    tcPart s stmt@(Assign (Ident i) _) = return $ Map.fromList [(i, (s, stmt))]
    tcPart s stmt@(Each _ _ _ _) =
      let ns = Loop s "" in
        err "  Not yet implemented : each"

data Value =  IVal Int
            | DVal Double
            | SVal String
            | List [Value]
  deriving (Eq)

instance Show Value where
  show (IVal i) = show i
  show (DVal d) = show d
  show (SVal s) = show s
  show (List l) = show l

resolve :: String -> Map String (Scope, Statement) -> Either Err Value
resolve t p = case t `Map.lookup` p of
                Nothing -> err $ concat ["undefined pipe: ", t]
                Just ((s, Assign (Ident i) (Pipe (Ident fn) ps as))) ->
                  do  args <- sequence $ map (flip resolve p . identStr) as
                      resolvePipe fn ps args
                Just _ -> err "ho"
  where
    identStr (Ident i) = i
    resolvePipe :: String -> [Param] -> [Value] -> Either Err Value
    resolvePipe fn ps as = err $ concat ["undefined block: ", fn]

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

