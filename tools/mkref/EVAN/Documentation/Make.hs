module EVAN.Documentation.Make 
  (makeMain)
  where

import EVAN.Documentation
import System.Exit
import System.IO (hPutStr, stderr)

outerr = hPutStr stderr

makeMain :: (Docs -> String) -> IO ()
makeMain showFunc = getContents >>= return . readDocs >>= output
  where
    output (Left d) = putStrLn $ showFunc d
    output (Right e) =
      outerr ("FATAL: " ++ e ++ "\n") >> exitFailure

