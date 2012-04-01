%{
#include <stdio.h>

void yyerror(const char *e) {
	fprintf(stderr, "parse error: %s\n", e);
}

int yywrap() {
	return 1;
}

extern int yylex();
%}

/*
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

*/

%token LARROW
%token RETURN EACH
%token DOT
%token SID LID
%%

program: 
       stmtlist RETURN id DOT {} ;

stmtlist:
	|
	stmtlist statement
	{}
	;

statement: assign | each ;

assign: id LARROW pipe DOT {} ;

pipe: {} ;

each: EACH {} ;

id: SID | LID ;

