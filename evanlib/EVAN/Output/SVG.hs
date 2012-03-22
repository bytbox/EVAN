module EVAN.Output.SVG where

import EVAN.Output.Mime

data SVGImage = SVGImage String

instance MIME SVGImage where
  mimebox (SVGImage s) = MIMEBox "image/svg+xml" "histogram" s 

