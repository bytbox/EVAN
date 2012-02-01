module EVAN.Documentation.HTML where

import EVAN.Documentation

class HTMLShow d where
  showHTML :: d -> String

instance HTMLShow Docs where
  showHTML d = "<!DOCTYPE html>\
    \<html><head>\
    \<title>EVAN Reference Documentation</title>\
    \</head><body>" ++ published d ++
    "</body>\
    \</html>"

