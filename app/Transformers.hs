module Transformers where
    
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map
import Panic (throwGhcException)

type Name = String    -- variable names
data Exp = Lit Integer -- expressions
            | Var Name
            | Plus Exp Exp
            | Abs Name Exp
            | App Exp Exp
            deriving (Show )

data Value = IntVal Integer
            | FunVal Env Name Exp
            deriving (Show )

type Env = Map.Map Name Value -- mapping from names to values

exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
wrongExp = Lit 12 `Plus` (App (Abs "y" (Var "x")) (Lit 4 `Plus` Lit 2))

eval0 :: Env -> Exp -> Value 
eval0 env (Lit i) = IntVal i
eval0 env (Var n) = fromJust (Map.lookup n env)
eval0 env (Plus e1 e2) = let IntVal x = eval0 env e1
                             IntVal y = eval0 env e2
                            in IntVal (x + y)
eval0 env (Abs n e) = FunVal env n e
eval0 env (App e1 e2) = let val1 = eval0 env e1
                            val2 = eval0 env e2
                        in case val1 of 
                            FunVal env' n body -> eval0 (Map.insert n val2 env') body




type Eval1 a = Identity a
runEval1 :: Eval1 a -> a
runEval1 ev = runIdentity ev

eval1 env (Lit i) = return $ IntVal i
eval1 env (Var n) = Map.lookup n env
eval1 env (Plus e1 e2) = do IntVal x <- eval1 env e1 
                            IntVal y <- eval1 env e2 
                            return $ IntVal (y + x)
eval1 env (Abs n e) = return $ FunVal env n e
eval1 env (App e1 e2) = do val1 <- eval1 env e1 
                           val2 <- eval1 env e2
                           case val1 of 
                                FunVal env' n body -> eval1 (Map.insert n val2 env') body


type Eval2 a = ExceptT String Identity a 
runEval2 ev = runIdentity (runExceptT ev)


eval2 :: Env -> Exp -> Eval2 Value
eval2 env (Lit i) = return $ IntVal i
eval2 env (Var n) = case Map.lookup n env of
                    Nothing -> throwError ("ubound variable,  " ++ n)
                    Just val -> return val
eval2 env (Plus e1 e2) = do e1' <- eval2 env e1 
                            e2' <- eval2 env e2 
                            case (e1', e2') of 
                                (IntVal x, IntVal y) -> 
                                    return $ IntVal (x + y)
                                _ -> throwError "type error in addition"
eval2 env (Abs n e) = return $ FunVal env n e
eval2 env (App e1 e2) = do val1 <- eval2 env e1
                           val2 <- eval2 env e2
                           case val1 of
                                FunVal env' n body ->
                                  eval2 (Map.insert n val2 env') body
                                _ -> throwError "type error in application"



tick :: (Num s, MonadState s m ) => m ()
tick = do st <- get
          put (st + 1)

initialState :: Integer
initialState = 0

type Eval6 a = ReaderT Env (ExceptT String 
                            (WriterT [String](StateT Integer IO))) a

runEval6 :: Env -> Integer -> Eval6 a -> IO((Either String a, [String]), Integer)
runEval6 env st ev = 
    runStateT (runWriterT (runExceptT (runReaderT ev env))) st

eval6 :: Exp -> Eval6 Value
eval6 (Lit i) = do tick
                   liftIO $ print i
                   return $ IntVal i
eval6 (Var n) = do tick
                   tell [n]
                   env <- ask
                   case Map.lookup n env of 
                    Nothing -> throwError ("ubound variable,  " ++ n)
                    Just val -> return val
eval6 (Plus e1 e2) = do tick
                        e1' <- eval6 e1 
                        e2' <- eval6 e2 
                        case (e1', e2') of 
                            (IntVal x, IntVal y) -> 
                                return $ IntVal (x + y)
                            _ -> throwError "type error in addition"
eval6 (Abs n e) = do tick
                     env <- ask
                     return $ FunVal env n e
eval6 (App e1 e2) = do tick
                       val1 <- eval6 e1
                       val2 <- eval6 e2
                       case val1 of
                        FunVal env' n body ->
                            local (const (Map.insert n val2 env'))
                            (eval6 body)
                        _ -> throwError "type error in application"



type Eval7 a = ReaderT Env (ExceptT String 
                            (StateT Integer IO)) a

runEval7 :: Env -> Integer -> Eval7 a -> IO(Either String a, Integer)
runEval7 env st ev = 
    runStateT (runExceptT (runReaderT ev env)) st

eval7 :: Exp -> Eval7 Value
eval7 (Lit i) = do tick
                   liftIO $ print i
                   return $ IntVal i
eval7 (Var n) = do tick
                   env <- ask
                   case Map.lookup n env of 
                    Nothing -> throwError ("ubound variable,  " ++ n)
                    Just val -> return val
eval7 (Plus e1 e2) = do tick
                        e1' <- eval7 e1 
                        e2' <- eval7 e2 
                        case (e1', e2') of 
                            (IntVal x, IntVal y) -> 
                                return $ IntVal (x + y)
                            _ -> throwError "type error in addition"
eval7 (Abs n e) = do tick
                     env <- ask
                     return $ FunVal env n e
eval7 (App e1 e2) = do tick
                       val1 <- eval7 e1
                       val2 <- eval7 e2
                       case val1 of
                        FunVal env' n body ->
                            local (const (Map.insert n val2 env'))
                            (eval7 body)
                        _ -> throwError "type error in application"


run :: IO (Either String Value, Integer)
run = do
    runEval7 Map.empty initialState (eval7 exampleExp)