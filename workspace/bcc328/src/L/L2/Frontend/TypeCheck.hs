module L.L2.Frontend.TypeCheck where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer

import Data.List ((\\))

import L.L2.Frontend.Syntax
import Utils.Var

typeCheck :: L2 -> Either String L2
typeCheck prog@(L2 stmts) = 
  case fst $ fst $ runTcM initTcEnv (checkStmts stmts) of
    Left err -> Left err
    Right _ -> Right prog

checkStmts :: [S2] -> TcM ()
checkStmts = mapM_ checkStmt

checkStmt :: S2 -> TcM ()
checkStmt (Def var expr stmts) = do
  checkExpr expr
  insertVar var
  checkStmts stmts
  removeVar var

checkStmt (LRead _ var) = do
  checkVarNotImmutable var

checkStmt (LPrint expr) = do
  checkExpr expr

checkStmt (LAssign var expr) = do
  checkVarNotImmutable var
  checkExpr expr

checkExpr :: E2 -> TcM ()
checkExpr (LVal _) = return ()
checkExpr (LVar _) = return () 
checkExpr (LAdd e1 e2) = checkExpr e1 >> checkExpr e2
checkExpr (LMinus e1 e2) = checkExpr e1 >> checkExpr e2
checkExpr (LMul e1 e2) = checkExpr e1 >> checkExpr e2
checkExpr (LDiv e1 e2) = checkExpr e1 >> checkExpr e2

checkVarNotImmutable :: Var -> TcM ()
checkVarNotImmutable var = do
  env <- get
  if var `elem` context env
    then throwError $ "Cannot assign to immutable variable: " ++ show var
    else return ()

-- basic monad infrastructure

type TcM a = ExceptT String (WriterT [String] (StateT TcEnv Identity)) a

data TcEnv
  = TcEnv {
      context :: [Var] -- imutable variable list
    }

initTcEnv :: TcEnv
initTcEnv = TcEnv []

insertVar :: Var -> TcM ()
insertVar v = modify (\ env -> env{context = v : context env})

removeVar :: Var -> TcM ()
removeVar v = modify (\ env -> env {context = (context env) \\ [v]})

runTcM :: TcEnv -> TcM a -> (((Either String a), [String]), TcEnv)
runTcM env m
  = runIdentity (runStateT (runWriterT (runExceptT m)) env)


