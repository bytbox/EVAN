module Main where

import Data.Maybe
import Text.Parsec

data Ident = Ident String
  deriving (Eq, Show)

data Expr = Id Ident
  deriving (Eq, Show)

data Statement = Assign Ident Expr | Each Ident Expr [Statement]
  deriving (Eq, Show)

skip p = p >> return ()

comment = do
  string "[["
  manyTill anyChar $ string "]]"
  return Nothing

tokEach = skip $ string "each"
ident = many1 letter >>= return . Ident
rarrow = skip $ string "<-"
expr = ident >>= return . Id

assign = do
  d <- ident
  skipMany space
  rarrow
  skipMany space
  s <- expr
  return $ Just (Assign d s)

each = do
  tokEach
  skipMany1 space
  d <- ident
  skipMany space
  rarrow
  skipMany space
  s <- expr
  skipMany space
  l <- between (char '{') (char '}') stmtList
  return $ Just $ Each d s l

statement = try assign <|> try each

stmtList = do
  l <- many (try $ skipMany space >> (comment <|> statement))
  skipMany space
  return $ catMaybes l

parser = do
  l <- stmtList
  eof
  return l

parseFile f c = runParser parser () f c

main = putStrLn . show . parseFile "<stdin>" =<< getContents

