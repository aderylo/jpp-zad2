module Main where

-- import           Transformers

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
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

type Scope = Ident

type VarStore = Map.Map (Scope, Ident)  Value

type ReturnStore = Value

type Store = (VarStore, ReturnStore, Scope)

type Eval a = StateT Store (ReaderT Env (ExceptT String IO)) a

type Inter a = StateT Store (ReaderT Env (ExceptT String IO)) a

setReturnValue :: Value -> Eval ()
setReturnValue value = do
  (varStore, _, scope) <- get
  put (varStore, value, scope)

getReturnValue :: Eval Value
getReturnValue = do
  (_, returnStore, _) <- get
  return returnStore

updateVarStore :: Ident -> Value -> Eval ()
updateVarStore ident value = do
  (varStore, returnStore, scope) <- get
  put (Map.insert (scope, ident) value varStore, returnStore, scope)

setScope :: Ident -> Eval ()
setScope ident = do
  (varStore, returnStore, _) <- get
  put (varStore, returnStore, ident)

runEval :: Store -> Env -> Eval a -> IO (Either String a)
runEval store env eval =
  runExceptT $ runReaderT (evalStateT eval store) env

evalProgram :: Program -> Eval Value
evalProgram (Program _ topDefs) = do
  env <- evalTopDefs topDefs
  local (const env) (evalExpr (EApp Nothing (Ident "main") []))

evalArgDeclaration :: (Arg, Expr) -> Eval (Ident, Value)
evalArgDeclaration (Arg _ t ident, expr) = do
  value <- evalExpr expr
  return (ident, value)

populateEnvWithArgValue :: (Ident, Value) -> Eval Env
populateEnvWithArgValue (ident, value) = do
  (vEnv, fEnv) <- ask
  return (Map.insert ident value vEnv, fEnv)

evalArgsDeclarations :: [Arg] -> [Expr] -> Eval Env
evalArgsDeclarations args exprs = do
  argValues <- mapM evalArgDeclaration (zip args exprs)
  envs <- mapM populateEnvWithArgValue argValues
  return (mconcat envs)

evalTopDef :: TopDef -> Eval Env
evalTopDef (FnDef _ t ident args block) = do
  (vEnv, fEnv) <- ask
  let newEnv = (vEnv, Map.insert ident (Function $ evalFunDeclaration newEnv) fEnv)
  return newEnv
  where
    evalFunDeclaration :: Env -> [Expr] -> Eval Value
    evalFunDeclaration localEnv exprs =
      do
        callEnv <- ask
        scoped <- evalArgsDeclarations args exprs
        local (const scoped) (evalBlock block)
        getReturnValue
evalTopDefs :: [TopDef] -> Eval Env
evalTopDefs topDefs = do
  envs <- mapM evalTopDef topDefs
  return (mconcat envs)

evalBlock :: Block -> Eval ()
evalBlock (Block _ stmts) = evalStmts stmts
  where
    evalStmts [] = return () -- No more statements to evaluate
    evalStmts (stmt:rest) = do
      evalStmt stmt
      checkForReturn stmt rest

    checkForReturn :: Stmt -> [Stmt] -> Eval ()
    checkForReturn (Ret _ _) _ = return () -- Stop evaluating after encountering a `Ret` statement
    checkForReturn _ [] = return () -- Stop evaluating at the end of the statement list
    checkForReturn _ (stmt:rest) = evalStmt stmt >> checkForReturn stmt rest -- Continue evaluating

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
  updateVarStore ident value
evalStmt (Incr _ ident) = do
  (varStore, _, scope) <- get
  case Map.lookup (scope, ident) varStore of
    Just (IntVal value) -> updateVarStore ident (IntVal (value + 1))
    Nothing -> throwError ("Variable " ++ show ident ++ " not in scope")
    _ -> throwError "Error: Increment operation only applies to integer variables"
evalStmt (Decr _ ident) = do
  (varStore, _, scope) <- get
  case Map.lookup (scope, ident) varStore of
    Just (IntVal value) -> updateVarStore ident (IntVal (value - 1))
    Nothing -> throwError ("Variable " ++ show ident ++ " not in scope")
    _ -> throwError "Error: Decrement operation only applies to integer variables"
  return ()
evalStmt (Ret _ expr) = do
  value <- evalExpr expr
  setReturnValue value
evalStmt (VRet _) = do
  setReturnValue VoidVal

evalItem :: Type -> Item -> Eval ()
evalItem t (NoInit _ ident) = do
  case t of
    Int _ -> updateVarStore ident (IntVal 0)
    Bool _ -> updateVarStore ident (BoolVal False)
    Str _ -> updateVarStore ident (StringVal "")
    Void _ -> updateVarStore ident VoidVal
    _ -> throwError "Type error"
evalItem t (Init _ ident expr) = do
  value <- evalExpr expr
  updateVarStore ident value

evalExpr :: Expr -> Eval Value
evalExpr (EVar _ ident) = do
  (varStore, _, scope) <- get
  case Map.lookup (scope, ident) varStore of
    Just value -> return value
    Nothing -> throwError ("Variable " ++ show ident ++ " not in scope")
evalExpr (ELitInt _ v) = return $ IntVal v
evalExpr (ELitTrue _) = return $ BoolVal True
evalExpr (ELitFalse _) = return $ BoolVal False
evalExpr (EApp _ ident exprs) = do
  (varEnv, funEnv) <- ask
  setScope ident
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

evalExprs :: [Expr] -> Eval [Value]
evalExprs exprs = mapM evalExpr exprs

interpret :: String -> IO ()
interpret input = case parsedTokens of
  Right tree -> do
    result <- runEval (Map.empty, initialState, initialScope) (Map.empty, Map.empty) (evalProgram tree)
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
