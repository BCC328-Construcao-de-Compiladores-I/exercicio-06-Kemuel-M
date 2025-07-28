module L.L2.Backend.CCodegen where

import L.L2.Frontend.Syntax
import Utils.Pretty
import Utils.Value
import Utils.Var
import qualified Data.Set as Set
import Data.Set (Set)

cL2Codegen :: L2 -> String
cL2Codegen (L2 stmts) = 
  unlines $ [ "#include <stdio.h>"
            , "#include <string.h>"
            , "#include <stdlib.h>"
            , ""
            , "int main() {"
            ] ++
            map (nest 4) (generateBody Set.empty stmts) ++
            [ nest 4 "return 0;"
            , "}"
            ]
  where
    nest n v = replicate n ' ' ++ v

generateBody :: Set Var -> [S2] -> [String]
generateBody declaredVars stmts = 
  let (_, result) = foldl processStmt (declaredVars, []) stmts
  in concat (reverse result)
  where
    processStmt (vars, acc) stmt =
      let newVars = extractDeclaredVar stmt vars
          stmtCode = generateStmt vars stmt
      in (newVars, stmtCode : acc)
    
    extractDeclaredVar (LAssign var _) vars = Set.insert var vars
    extractDeclaredVar (LRead _ var) vars = Set.insert var vars
    extractDeclaredVar (Def var _ _) vars = Set.insert var vars
    extractDeclaredVar _ vars = vars

generateStmt :: Set Var -> S2 -> [String]
generateStmt declaredVars (Def var expr stmts) =
  [ "// immutable variable definition"
  , "{"
  , "    int " ++ varName var ++ " = " ++ generateExpr expr ++ ";"
  ] ++
  map (nest 4) (generateBody (Set.insert var declaredVars) stmts) ++
  [ "}"
  ]
  where
    nest n v = replicate n ' ' ++ v

generateStmt declaredVars (LRead s var) =
  if Set.member var declaredVars
  then [ "// read operation"
       , "printf(\"" ++ s ++ "\");"
       , "scanf(\"%d\", &" ++ varName var ++ ");"
       ]
  else [ "// read operation"
       , "int " ++ varName var ++ ";"
       , "printf(\"" ++ s ++ "\");"
       , "scanf(\"%d\", &" ++ varName var ++ ");"
       ]

generateStmt declaredVars (LPrint expr) =
  case expr of
    LVal (VStr _) -> [ "// print operation"
                    , "printf(\"%s\\n\", " ++ generateExpr expr ++ ");"
                    ]
    _ -> [ "// print operation"
         , "printf(\"%d\\n\", " ++ generateExpr expr ++ ");"
         ]

generateStmt declaredVars (LAssign var expr) =
  if Set.member var declaredVars
  then [ "// assignment"
       , varName var ++ " = " ++ generateExpr expr ++ ";"
       ]
  else [ "// assignment"
       , "int " ++ varName var ++ " = " ++ generateExpr expr ++ ";"
       ]

varName :: Var -> String
varName (Var name) = name

generateExpr :: E2 -> String
generateExpr (LVal (VInt n)) = show n
generateExpr (LVal (VStr s)) = "\"" ++ s ++ "\""
generateExpr (LVal (VBool b)) = if b then "1" else "0"
generateExpr (LVar var) = varName var
generateExpr (LAdd e1 e2) = "(" ++ generateExpr e1 ++ " + " ++ generateExpr e2 ++ ")"
generateExpr (LMinus e1 e2) = "(" ++ generateExpr e1 ++ " - " ++ generateExpr e2 ++ ")"
generateExpr (LMul e1 e2) = "(" ++ generateExpr e1 ++ " * " ++ generateExpr e2 ++ ")"
generateExpr (LDiv e1 e2) = "(" ++ generateExpr e1 ++ " / " ++ generateExpr e2 ++ ")"
