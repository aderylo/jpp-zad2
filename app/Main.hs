module Main where

import           Prelude                 (Either (..), FilePath, IO, Int, Show,
                                          String, getContents, mapM_, putStrLn,
                                          readFile, show, unlines, ($), (++),
                                          (>), (>>), (>>=), Maybe (Just, Nothing), Num, Integer, return, print)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure, exitSuccess)
import           Grammar.Lex
import           Grammar.Par
import           Grammar.Abs
import           Grammar.ErrM 
import           Transformers
import qualified Data.Map as Map
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
    ( MonadIO(liftIO), ReaderT(runReaderT) )
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe


evalProgram :: Program -> Eval7 Value
evalProgram program = evalExpr (EApp  Nothing (Ident "main") []) 

evalExpr :: Expr  -> Eval7 Value                        
evalExpr (EApp _ funName expr) = return $ IntVal 1



interpret:: String -> IO ()
interpret input = case parsedTokens of 
                      Right tree -> do 
                        (result, s) <-  runEval7 Map.empty initialState (evalProgram tree)
                        case result of
                          _ -> exitSuccess
                        -- case  result of
                        --   Left err -> do 
                        --     putStrLn ("Eval error: " ++ err)
                        --     exitFailure
                        --   Right _ -> exitSuccess
                      Left err -> do
                        putStrLn ("Parsing error: " ++ err)
                        exitFailure
                  where
                    tokens = myLexer input
                    parsedTokens = pProgram tokens

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
    [] -> getContents >>= interpret
    [f] ->  readFile f >>= interpret
    _ -> do putStrLn "Too many arguments" 
            exitFailure
