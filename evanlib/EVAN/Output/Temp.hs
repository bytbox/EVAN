module EVAN.Output.Temp where

tmpfile :: IO FilePath
tmpfile = return "/tmp/_evan_out" -- TODO

