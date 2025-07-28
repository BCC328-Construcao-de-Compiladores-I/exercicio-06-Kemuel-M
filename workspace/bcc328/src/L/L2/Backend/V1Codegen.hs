module L.L2.Backend.V1Codegen where

import L.L2.Frontend.Syntax
import V.V1.Instr
import Utils.Value
import Utils.Var
import qualified Data.Map as Map

type Env = Map.Map Var Value

v1Codegen :: L2 -> Code
v1Codegen (L2 stmts) = codegenStmts stmts ++ [Halt]

codegenStmts :: [S2] -> Code
codegenStmts = concatMap codegenStmt

codegenStmt :: S2 -> Code
codegenStmt (Def var expr stmts) =
  codegenExpr expr ++        -- evaluate the expression
  [Store var] ++             -- store the value in the immutable variable
  codegenStmts stmts ++      -- generate code for the block
  []                         -- immutable variables don't need explicit cleanup

codegenStmt (LRead _ var) =
  [Input, Store var]

codegenStmt (LPrint expr) =
  codegenExpr expr ++ [Print]

codegenStmt (LAssign var expr) =
  codegenExpr expr ++ [Store var]

codegenExpr :: E2 -> Code
codegenExpr (LVal val) = [Push val]
codegenExpr (LVar var) = [Load var]
codegenExpr (LAdd e1 e2) = codegenExpr e1 ++ codegenExpr e2 ++ [Add]
codegenExpr (LMinus e1 e2) = codegenExpr e1 ++ codegenExpr e2 ++ [Sub]
codegenExpr (LMul e1 e2) = codegenExpr e1 ++ codegenExpr e2 ++ [Mul]
codegenExpr (LDiv e1 e2) = codegenExpr e1 ++ codegenExpr e2 ++ [Div]

