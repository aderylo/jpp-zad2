module Main where

import           Prelude                
import           System.Environment (getArgs)
import           System.Exit        (exitFailure, exitSuccess)
import           Grammar.Lex
import           Grammar.Par
import           Grammar.Abs
import           Grammar.ErrM 
-- import           Transformers
import qualified Data.Map as Map
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
    ( MonadIO(liftIO), ReaderT(runReaderT), MonadReader (ask) )
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import Language.Haskell.TH (valD)

-- type Name = String    -- variable names
-- data Exp = Lit Integer -- expressions
--             | Var Name
--             | Plus Exp Exp
--             | Abs Name Exp
--             | App Exp Exp
--             deriving (Show )

-- data Value = IntVal Integer
--             | FunVal Env Name Exp
--             deriving (Show )

-- type Env = Map.Map Name Value -- mapping from names to values

data Value = VoidVal
    | IntVal Integer
    | BoolVal Bool
    | StringVal String
    deriving (Show)

newtype Function = Function ([Expr] -> Eval Value)
type Position = Maybe(Int, Int)


type VarEnv = Map.Map Ident Value -- mapping from names to values
type FunEnv = Map.Map Ident Function
type Env = (VarEnv, FunEnv) 

type Eval a = ReaderT Env (ExceptT String (StateT Integer IO)) a

runEval :: Env -> Integer -> Eval a -> IO(Either String a, Integer)
runEval env st ev = runStateT (runExceptT (runReaderT ev env)) st

evalProgram :: Program -> Eval Value
evalProgram program = evalExpr (EApp  Nothing (Ident "main") []) 

evalExpr :: Expr  -> Eval Value
evalExpr (EVar _ ident) = do
  (varEnv, _) <- ask
  case Map.lookup ident varEnv of
    Just val -> return val
    Nothing -> throwError ("Variable " ++ show ident ++ " not found")
evalExpr (ELitInt _ v) =  return $ IntVal v
evalExpr (ELitTrue _) = return $ BoolVal True
evalExpr (ELitFalse _) = return $ BoolVal False
evalExpr (EApp _ ident exprs) = do
  (varEnv, funEnv) <- ask
  case Map.lookup ident funEnv of
    Just (Function f) -> f exprs
    Nothing -> throwError ("Function " ++ show ident ++ " not found")
evalExpr (EString _ str) = return $ StringVal str
evalExpr (Neg _ expr) = do
  v <- evalExpr expr
  case v of
    IntVal i -> return $ IntVal (-i)
    _ -> throwError "Type error"
evalExpr (Not _ expr) = do
  v <- evalExpr expr
  case v of
    BoolVal b -> return $ BoolVal (not b)
    _ -> throwError "Type error"
evalExpr (EMul _ expr1 op expr2) = do
  v1 <- evalExpr expr1
  v2 <- evalExpr expr2
  case (v1, v2) of
    (IntVal i1, IntVal i2) -> case op of
      Times _ -> return $ IntVal (i1 * i2)
      Div _ -> return $ IntVal (i1 `div` i2)
      Mod _ -> return $ IntVal (i1 `mod` i2)
    _ -> throwError "Type error"
evalExpr (EAdd _ expr1 op expr2) = do
  v1 <- evalExpr expr1
  v2 <- evalExpr expr2
  case (v1, v2) of
    (IntVal i1, IntVal i2) -> case op of
      Plus _ -> return $ IntVal (i1 + i2)
      Minus _ -> return $ IntVal (i1 - i2)
    _ -> throwError "Type error"
evalExpr (ERel _ expr1 op expr2) = do
  v1 <- evalExpr expr1
  v2 <- evalExpr expr2
  case (v1, v2) of
    (IntVal i1, IntVal i2) -> case op of
      LTH _ -> return $ BoolVal (i1 < i2)
      LE _ -> return $ BoolVal (i1 <= i2)
      GTH _ -> return $ BoolVal (i1 > i2)
      GE _ -> return $ BoolVal (i1 >= i2)
      EQU _ -> return $ BoolVal (i1 == i2)
      NE _ -> return $ BoolVal (i1 /= i2)
    _ -> throwError "Type error"
evalExpr (EAnd _ expr1 expr2) = do
  e1 <- evalExpr expr1
  e2 <- evalExpr expr2
  case (e1, e2) of
    (BoolVal b1, BoolVal b2) -> return $ BoolVal (b1 && b2)
    _ -> throwError "Type error"
evalExpr (EOr _ expr1 expr2) = do
  e1 <- evalExpr expr1
  e2 <- evalExpr expr2
  case (e1, e2) of
    (BoolVal b1, BoolVal b2) -> return $ BoolVal (b1 || b2)
    _ -> throwError "Type error"


interpret:: String -> IO ()
interpret input = case parsedTokens of 
                      Right tree -> do 
                        (result, s) <-  runEval (Map.empty, Map.empty) initialState (evalProgram tree)
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
                    initialState = 0
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
