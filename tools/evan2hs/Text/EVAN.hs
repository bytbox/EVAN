module Text.EVAN
  (Statement(..), Expr(..), Param(..), Ident(..), parseFile)
  where

import Data.Maybe (catMaybes)
import Text.Parsec

data Ident = Ident String
  deriving (Eq, Show)

data Param = IParam Int
  deriving (Eq, Show)

data Expr = Id Ident | Pipe Ident [Param] [Ident]
  deriving (Eq, Show)

data Statement = Assign Ident Expr | Each Ident Expr [Statement]
  deriving (Eq, Show)

number :: Parsec String () Int
number = many1 digit >>= return . read

skip p = p >> return ()

comment = try $ do
  string "[["
  manyTill anyChar $ string "]]"
  return Nothing

tokEach = skip $ string "each"
ident =
  (many1 letter <|> between (char '[') (char ']') (many $ noneOf "]"))
  >>= return . Ident
larrow = skip $ string "<-"
parenList p = between (char '(') (char ')') $ p `sepBy` (skipMany space >> char ',' >> skipMany space)
param = number >>= return . IParam
paramList = parenList param
argList = parenList ident
pipe = do
  f <- ident
  skipMany space
  ps <- paramList
  skipMany space
  as <- argList
  return $ Pipe f ps as
idExpr = ident >>= return . Id
expr = try pipe <|> try idExpr

assign = do
  d <- ident
  skipMany space
  larrow
  skipMany space
  s <- expr
  return $ Just (Assign d s)

each = do
  tokEach
  skipMany1 space
  d <- ident
  skipMany space
  larrow
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


