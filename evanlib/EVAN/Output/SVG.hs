module EVAN.Output.SVG where

import Text.JSON (JSON(..), toJSObject)

data SVGImage = SVGImage String

instance JSON SVGImage where
  readJSON = undefined
  showJSON (SVGImage s) = showJSON $ toJSObject
    [ ("mime", "image/svg+xml")
    , ("kind", "histogram")
    , ("data", s)
    ]

