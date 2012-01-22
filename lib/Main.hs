module Main where
import Data.HEPEVT
import EVAN
main :: IO ()

a = do
  { let x = 2
  ; return 3
  ; return 4
  ; putStrLn "hi"
  }

main = do
  let _Events1 = _Events () :: [Event]
  let __pipe_5 = _Events1 :: [Event]
  let _xxx = do
              _rpsdfspgrjnu <- __pipe_5
              let __pipe_6 = _rpsdfspgrjnu
              let _lmymensnxnpt = _Jets (__pipe_6)
              let __pipe_8 = _lmymensnxnpt
              let _sxvvldpoxgyq = _Count (__pipe_8)
              let __pipe_10 = _sxvvldpoxgyq
              return __pipe_10
  let __pipe_11 = _xxx
  let _Return1 = _Return (__pipe_11)
  _Return1
