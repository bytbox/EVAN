{-# LANGUAGE TemplateHaskell #-}

module Main
 where

import EVAN
import TupleTH

main = evanMain
_Events1 = Events 

_Return1 = Return __pipe_0
{- Hello, world! -}

__pipe_0 = $(nth 0 _Events1)
