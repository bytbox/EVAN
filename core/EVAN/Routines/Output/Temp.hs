module EVAN.Output.Temp where

tmpfile :: String -> IO FilePath
tmpfile ext = return $ "/tmp/_evan_out." ++ ext -- TODO

