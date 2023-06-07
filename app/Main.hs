module Main where

import Data.Map qualified as Map
import Eval (Value (IntVal), evalProgram, runEval)
import Grammar.Abs (Ident (Ident))
import Grammar.Par (myLexer, pProgram)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

-- Entrypoint ------------------------------------------------------------------

interpret :: String -> IO ()
interpret input = case parsedTokens of
  Right tree -> do
    result <- runEval (Map.empty, initialState) (Map.empty, Map.empty, initialScope) (evalProgram tree)
    case result of
      Left err -> do
        putStrLn ("Eval error: " ++ err)
        exitFailure
      Right v -> do
        putStrLn ("Program returned:  " ++ show v)
        exitSuccess
  Left err -> do
    putStrLn ("Parsing error: " ++ err)
    exitFailure
  where
    initialState = IntVal 0
    initialScope = Ident "global"
    tokens = myLexer input
    parsedTokens = pProgram tokens

usage :: IO ()
usage = do
  putStrLn $
    unlines
      [ "usage: Call with one of the following argument combinations:",
        "  --help          Display this help message.",
        "  (no arguments)  Run interpreter on program from stdin",
        "  filepath        Run interpreter on program from filepath"
      ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage -- help panel
    [] -> getContents >>= interpret
    [f] -> readFile f >>= interpret
    _ -> do
      putStrLn "Too many arguments"
      exitFailure
