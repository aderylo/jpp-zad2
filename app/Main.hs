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

type Position = Maybe(Int, Int)

type Eval a = ReaderT Env (ExceptT String (StateT Integer IO)) a

runEval :: Env -> Integer -> Eval7 a -> IO(Either String a, Integer)
runEval env st ev = runStateT (runExceptT (runReaderT ev env)) st




evalProgram :: Program -> Eval Value
evalProgram program = evalExpr (EApp  Nothing (Ident "main") []) 

evalExpr :: Expr  -> Eval Value                        
evalExpr (EApp _ funName expr) = return $ IntVal 1
evalExpr _ = throwError "errr"


interpret:: String -> IO ()
interpret input = case parsedTokens of 
                      Right tree -> do 
                        (result, s) <-  runEval7 Map.empty initialState (evalProgram tree)
                        case  result of
                          Left err -> do 
                            putStrLn ("Eval error: " ++ err)
                            exitFailure
                          Right _ -> do
                            exitSuccess
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
