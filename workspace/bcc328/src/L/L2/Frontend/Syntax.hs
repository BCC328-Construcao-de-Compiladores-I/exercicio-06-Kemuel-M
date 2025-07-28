module L.L2.Frontend.Syntax where

import Utils.Value
import Utils.Var
import Utils.Pretty

-- definition of the syntax of L2
-- programs
-- each L2 program is just a sequence of
-- statements

data L2
  = L2 [S2]
    deriving (Eq, Ord, Show)

-- statements can be a read, print or
-- an assignment.

data S2
  = Def Var E2 [S2]
  | LRead String Var
  | LPrint E2
  | LAssign Var E2
  deriving (Eq, Ord, Show)

-- expressions

data E2
  = LVal Value
  | LVar Var
  | LAdd E2 E2
  | LMinus E2 E2
  | LMul E2 E2
  | LDiv E2 E2
  deriving (Eq, Ord, Show)

instance Pretty L2 where
  ppr (L2 stmts) = vcat (map ppr stmts)

instance Pretty S2 where
  ppr (Def var expr stmts) = 
    hsep [text "def", ppr var, text ":=", ppr expr, text "in"] $$
    nest 2 (vcat (map ppr stmts)) $$
    text "end"
  ppr (LRead s var) = 
    hsep [text "read(", doubleQuotes (text s), comma, ppr var, text ");"]
  ppr (LPrint expr) = 
    hsep [text "print(", ppr expr, text ");"]
  ppr (LAssign var expr) = 
    hsep [ppr var, text ":=", ppr expr, text ";"]

instance Pretty E2 where
  ppr = pprAdd

pprAdd :: E2 -> Doc
pprAdd (LAdd e1 e2) = hsep [pprMinus e1, text "+", pprAdd e2]
pprAdd other = pprMinus other

pprMinus :: E2 -> Doc
pprMinus (LMinus e1 e2) = hsep [pprMul e1, text "-", pprMinus e2]
pprMinus other = pprMul other

pprMul :: E2 -> Doc
pprMul (LMul e1 e2) = hsep [pprDiv e1, text "*", pprMul e2]
pprMul other = pprDiv other

pprDiv :: E2 -> Doc
pprDiv (LDiv e1 e2) = hsep [pprFact e1, text "/", pprDiv e2]
pprDiv other = pprFact other

pprFact :: E2 -> Doc
pprFact (LVal v) = ppr v
pprFact (LVar v) = ppr v
pprFact other = parens (ppr other)


