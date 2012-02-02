{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module EVAN.Documentation.HTML where

import Data.List (intercalate)
import qualified Data.Map as M

import EVAN.Documentation

class HTMLShow d where
  showHTML :: d -> String

instance HTMLShow Docs where
  showHTML d = intercalate "\n" $ 
    [ "<!DOCTYPE html>\
      \<html><head>\
      \<link rel=\"stylesheet\" type=\"text/css\" href=\"ref.css\" />\
      \<title>EVAN Reference Documentation</title>\
      \</head><body>\n<h1>EVAN Reference Documentation</h1>"
    , generated d
    , intercalate "\n" $ map categoryLink $ M.keys $ categories d
    , showHTML $ categories d
    , "</body>\
      \</html>"
    ]

instance HTMLShow a => HTMLShow (M.Map String a) where
  showHTML = showHTML . M.toList

instance HTMLShow a => HTMLShow (String, a) where
  showHTML = showHTML . snd

instance HTMLShow a => HTMLShow [a] where
  showHTML = intercalate "\n" . map showHTML

instance HTMLShow Category where
  showHTML c = intercalate "\n"
    [ "<h2>" ++ title c ++ "</h2>"
    , showHTML $ functions c
    ]

instance HTMLShow Function where
  showHTML f = intercalate "\n"
    [ "<h3>" ++ name f ++ " :: " ++ signature f ++ "</h3>"
    , docstring f
    ]

categoryLink cn = "<a href=\"#cat-" ++ cn ++ "\">" ++ cn ++ "</a>"

