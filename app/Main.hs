module Main where

import           Prelude            (IO, getContents, putStrLn, unlines, readFile, ($),
                                     (>>=))
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)
import           Grammar.Lex
import           Grammar.Par
import           Grammar.Abs
import           Transformers

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Run interpreter on program from stdin"
    , "  filepath        Run interpreter on program from filepath"
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage -- help panel 
    _ -> do putStrLn "Too many arguments" 
            exitFailure
