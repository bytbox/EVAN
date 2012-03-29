module Main where

import Data.List (intercalate)
import Text.EVAN

toHS :: [Statement] -> String
toHS ss = intercalate "\n" $ map hsStatement ss

hsStatement :: Statement -> String
hsStatement (Assign i e) = intercalate " "
  [ hsIdent i
  , "="
  , hsExpr e
  ]
hsStatement (Each i e ss) = ""

hsIdent :: Ident -> String
hsIdent (Ident s) = '_' : map spaceToUnderscore s
  where
    spaceToUnderscore ' ' = '_'
    spaceToUnderscore x = x

hsExpr :: Expr -> String
hsExpr (Id i) = hsIdent i
hsExpr (Pipe i ps is) = intercalate " "
  [ hsIdent i
  , concat
    [ "("
    , ")"
    ]
  , concat
    [ "("
    , intercalate ", " $ map hsIdent is
    , ")"
    ]
  ]

main = do
  str <- getContents
  case parseFile "<stdin>" str of
    Left e -> putStrLn $ show e
    Right a -> putStrLn $ toHS a

