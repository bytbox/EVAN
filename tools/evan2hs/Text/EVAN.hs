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

data Statement = Assign Ident Expr
                |Each Ident Ident [Statement] [(Ident, Ident)]
  deriving (Eq, Show)

number :: Parsec String () Int
number = many1 digit >>= return . read

comma = try $ skipMany space >> char ',' >> skipMany space

comment = try $ do
  string "[["
  manyTill anyChar $ try $ string "]]"
  return Nothing

ident =
  (many1 letter <|> between (char '[') (char ']') (many $ noneOf "]"))
  >>= return . Ident
parenList p = between (char '(') (char ')') $
              p `sepBy` comma
param = number >>= return . IParam
paramList = try $ parenList param
argList = ident `sepBy` comma
pipe = do
        f <- ident
        skipMany space
        psm <- optionMaybe $ paramList
        skipMany space
        let ps = case psm of
                  Nothing -> []
                  Just a -> a
        as <- argList
        return $ Pipe f ps as
idExpr = ident >>= return . Id

assign = do
          d <- ident
          skipMany space
          string "<-"
          skipMany space
          s <- pipe
          skipMany space
          char '.'
          return $ Just (Assign d s)

separating = flip sepBy

each = do
        string "each"
        skipMany1 space
        d <- ident
        skipMany space
        string "<-"
        skipMany space
        s <- ident
        skipMany space
        l <- between (char '{') (char '}') stmtList
        skipMany space
        e <- comma `separating` do
                                  l <- ident
                                  skipMany space
                                  string "-<"
                                  skipMany space
                                  s <- ident
                                  return (l, s)
        skipMany space
        char '.'
        return $ Just $ Each d s l e

statement = try assign <|> try each

stmtList = do
  l <- many (try $ skipMany space >> (comment <|> statement))
  skipMany space
  return $ catMaybes l

parser = do
  l <- stmtList
  string "return"
  skipMany space
  r <- ident
  skipMany space
  char '.'
  skipMany space
  eof
  return (l, r)

parseFile f c = runParser parser () f c

