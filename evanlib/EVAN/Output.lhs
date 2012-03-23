> module EVAN.Output where

> import Text.JSON (encode, JSON)

> import EVAN.Output.Histogram
> import EVAN.Output.MIME
> import EVAN.Output.SVG
> import EVAN.Output.Unsafe

> _Histogram :: () -> [Double] -> IO MIMEBox
> _Histogram () ns = tempSVG "histogram" (500, 350) $ renderHist (500, 350) ns

Returned values are JSON-encoded.

> _Return :: () -> JSON a => a -> IO ()
> _Return () = putStrLn . encode

