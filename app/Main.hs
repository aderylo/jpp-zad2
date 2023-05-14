module Main where

-- import           Transformers

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
  ( MonadIO (liftIO),
    MonadReader (ask),
    ReaderT (runReaderT),
  )
import Control.Monad.State
import Control.Monad.Writer
import CoreArity (exprArity)
import qualified Data.Map as Map
import Data.Maybe
import Grammar.Abs
import Grammar.ErrM
import Grammar.Lex
import Grammar.Par
import Language.Haskell.TH (Dec, valD)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Prelude

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

data Value
  = VoidVal
  | IntVal Integer
  | BoolVal Bool
  | StringVal String
  deriving (Show)

newtype Function = Function ([Expr] -> Eval Value)

type Position = Maybe (Int, Int)

type VarEnv = Map.Map Ident Value -- mapping from names to values

type FunEnv = Map.Map Ident Function

type Env = (VarEnv, FunEnv)

type Store = Map.Map Ident Value

type Eval a = StateT Store (ReaderT Env (ExceptT String IO)) a

type Inter a = StateT Store (ReaderT Env (ExceptT String IO)) a

runEval :: Store -> Env -> Eval a -> IO (Either String a)
runEval store env eval =
  runExceptT $ runReaderT (evalStateT eval store) env

evalProgram :: Program -> Eval Value
evalProgram program = evalExpr (EApp Nothing (Ident "main") [])

evalBlock :: Block -> Eval ()
evalBlock (Block _ stmts) = do
  mapM_ evalStmt stmts

evalStmt :: Stmt -> Eval ()
evalStmt (Empty _) = return ()
evalStmt (BStmt _ block) = evalBlock block
evalStmt (Cond _ expr stmt) = do
  v <- evalExpr expr
  case v of
    BoolVal True -> evalStmt stmt
    BoolVal False -> return ()
    _ -> throwError "Type error"
evalStmt (CondElse _ expr stmt1 stmt2) = do
  v <- evalExpr expr
  case v of
    BoolVal True -> evalStmt stmt1
    BoolVal False -> evalStmt stmt2
    _ -> throwError "Type error"
evalStmt (While _ expr stmt) = do
  v <- evalExpr expr
  case v of
    BoolVal True -> evalStmt stmt >> evalStmt (While Nothing expr stmt)
    BoolVal False -> return ()
    _ -> throwError "Type error"
evalStmt (SExp _ expr) = do
  evalExpr expr
  return ()
evalStmt (Cont _) = do
  throwError "Not implemented"
  return ()
evalStmt (Break _) = do
  throwError "Not implemented"
  return ()
evalStmt (Decl _ t items) = do
  mapM_ (evalItem t) items
evalStmt (Ass _ ident expr) = do
  value <- evalExpr expr
  modify (Map.insert ident value)
evalStmt (Incr _ ident) = do
  store <- get
  case Map.lookup ident store of
    Just (IntVal value) -> modify (Map.insert ident (IntVal (value + 1)))
    Nothing -> throwError "No such variable!"
    _ -> throwError "Error: Increment operation only applies to integer variables"
evalStmt (Decr _ ident) = do
  store <- get
  case Map.lookup ident store of
    Just (IntVal value) -> modify (Map.insert ident (IntVal (value - 1)))
    Nothing -> throwError "No such variable!"
    _ -> throwError "Error: Increment operation only applies to integer variables"
evalStmt (Ret _ expr) = do
  throwError "Not implemented"
  return ()
evalStmt (VRet _) = do
  throwError "Not implemented"
  return ()

evalItem :: Type -> Item -> Eval ()
evalItem t (NoInit _ ident) = do
  case t of
    Int _ -> modify (Map.insert ident (IntVal 0))
    Bool _ -> modify (Map.insert ident (BoolVal False))
    Str _ -> modify (Map.insert ident (StringVal ""))
    Void _ -> modify (Map.insert ident VoidVal)
    _ -> throwError "Type error"
evalItem t (Init _ ident expr) = do
  value <- evalExpr expr
  modify (Map.insert ident value)

evalExpr :: Expr -> Eval Value
evalExpr (EVar _ ident) = do
  (varEnv, _) <- ask
  case Map.lookup ident varEnv of
    Just val -> return val
    Nothing -> throwError ("Variable " ++ show ident ++ " not found")
evalExpr (ELitInt _ v) = return $ IntVal v
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
    IntVal i -> return $ IntVal (- i)
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

interpret :: String -> IO ()
interpret input = case parsedTokens of
  Right tree -> do
    result <- runEval Map.empty (Map.empty, Map.empty) (evalProgram tree)
    case result of
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
