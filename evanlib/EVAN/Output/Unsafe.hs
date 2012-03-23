module EVAN.Output.Unsafe where

import System.IO.Unsafe (unsafePerformIO)

import Text.JSON (JSON(..))

instance JSON a => JSON (IO a) where
  readJSON = undefined
  showJSON = showJSON . unsafePerformIO

