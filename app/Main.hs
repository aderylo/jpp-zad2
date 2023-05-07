module Main where

import           Prelude            (IO, getContents, putStrLn, unlines, return, readFile, ($),
                                     (>>=), Integer)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)
import           Grammar.Lex
import           Grammar.Par
import           Grammar.Abs
import           Grammar.ErrM 
import           Transformers
import qualified Data.Map as Map

ini :: Integer
ini = 0

interpret = do runEval6 Map.empty initialState (eval6 exampleExp)
               return ()

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
    [] ->  interpret
    _ -> do putStrLn "Too many arguments" 
            exitFailure
