module Main where

import Elaine.Exec (exec)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> exec "run" file
    [cmd, file] -> exec cmd file
    _ -> error "Invalid usage"
