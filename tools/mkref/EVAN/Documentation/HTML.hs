module EVAN.Documentation.HTML where

import Data.List (intercalate)

import EVAN.Documentation

class HTMLShow d where
  showHTML :: d -> String

instance HTMLShow Docs where
  showHTML d = intercalate "\n" $ 
    [ "<!DOCTYPE html>\
      \<html><head>\
      \<link rel=\"stylesheet\" type=\"text/css\" href=\"ref.css\" />\
      \<title>EVAN Reference Documentation</title>\
      \</head><body>"
    , generated d
    , show $ categories d
    , "</body>\
      \</html>"
    ]

