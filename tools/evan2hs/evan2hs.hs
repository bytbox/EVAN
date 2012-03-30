module Main where

import Data.List (intercalate)
import Text.EVAN

toHS :: ([Statement], Ident) -> String
toHS (ss, r) =
  intercalate "\n" $
  "module Main where" :
  "import EVAN" :
  intercalate " " ["main = putStrLn $ show", hsIdent r] :
  map hsStatement ss

hsStatement :: Statement -> String
hsStatement (Assign i e) = intercalate " "
  [ hsIdent i
  , "="
  , hsExpr e
  ]
hsStatement (Each i l ss es) = ""

hsIdent :: Ident -> String
hsIdent (Ident s) = '_' : map spaceToUnderscore s
  where
    spaceToUnderscore ' ' = '_'
    spaceToUnderscore x = x

hsParam :: Param -> String
hsParam (IParam i) = show i

hsExpr :: Expr -> String
hsExpr (Id i) = hsIdent i
hsExpr (Pipe i ps is) = intercalate " "
  [ hsIdent i
  , plist $ map hsParam ps
  , plist $ map hsIdent is
  ]
  where plist l = concat
                    [ "("
                    , intercalate ", " l
                    , ")"
                    ]

main = do
  str <- getContents
  case parseFile "<stdin>" str of
    Left e -> putStrLn $ show e
    Right a -> putStrLn $ toHS a

