{--  Happy parser for L1 - LALR style --}
{
module L.L1.Frontend.LALRParser (parseLALRL1) where

import L.L1.Frontend.Lexer
import L.L1.Frontend.Syntax
import Utils.Value
import Utils.Var
}

%name parser Program
%tokentype { Token }
%monad { Parser }
%lexer { happyLexer } { Token _ TEOF }
%error { parseError }

%token
    num     { Token _ (TNumber $$) }
    str     { Token _ (TString $$) }
    id      { Token _ (TId $$) }
    '('     { Token _ TLParen }
    ')'     { Token _ TRParen }
    '+'     { Token _ TPlus }
    '-'     { Token _ TMinus }
    '*'     { Token _ TTimes }
    '/'     { Token _ TDiv }
    ':='    { Token _ TAssign }
    ';'     { Token _ TSemicolon }
    ','     { Token _ TComma }
    read    { Token _ TRead }
    print   { Token _ TPrint }

%right ';'
%left '+' '-'
%left '*' '/'

%%

Program :: { L1 }
Program : StmtList               { L1 $1 }

StmtList :: { [S1] }
StmtList : Stmt ';' StmtList     { $1 : $3 }
         | {- empty -}           { [] }

Stmt :: { S1 }
Stmt : id ':=' Exp              { LAssign (Var $1) $3 }
     | read '(' str ',' id ')'  { LRead $3 (Var $5) }
     | print '(' Exp ')'        { LPrint $3 }

Exp :: { E1 }
Exp : Exp '+' Term              { LAdd $1 $3 }
    | Exp '-' Term              { LMinus $1 $3 }
    | Term                      { $1 }

Term :: { E1 }
Term : Term '*' Factor          { LMul $1 $3 }
     | Term '/' Factor          { LDiv $1 $3 }
     | Factor                   { $1 }

Factor :: { E1 }
Factor : '(' Exp ')'            { $2 }
       | num                    { LVal (VInt $1) }
       | str                    { LVal (VStr $1) }
       | id                     { LVar (Var $1) }

{
-- Parser monad storing remaining tokens
newtype Parser a = Parser { runParser :: [Token] -> Either String (a, [Token]) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \ts -> case p ts of
    Left err -> Left err
    Right (x, ts') -> Right (f x, ts')

instance Applicative Parser where
  pure x = Parser $ \ts -> Right (x, ts)
  Parser pf <*> Parser px = Parser $ \ts -> case pf ts of
    Left err -> Left err
    Right (f, ts') -> case px ts' of
      Left err -> Left err
      Right (x, ts'') -> Right (f x, ts'')

instance Monad Parser where
  Parser p >>= f = Parser $ \ts -> case p ts of
    Left err -> Left err
    Right (x, ts') -> runParser (f x) ts'

getToken :: Parser Token
getToken = Parser $ \ts -> case ts of
  [] -> Left "Unexpected end of input"
  (t:ts') -> Right (t, ts')

happyLexer :: (Token -> Parser a) -> Parser a
happyLexer f = do
  t <- getToken
  f t

parseError :: Token -> Parser a
parseError (Token (l,c) lx) = Parser $ \_ ->
  Left ("Parse error while processing lexeme: " ++ show lx ++
        "\n at line " ++ show l ++ ", column " ++ show c)

parseLALRL1 :: String -> Either String L1
parseLALRL1 src = fmap fst $ runParser parser tokens
  where tokens = lexer src ++ [Token (0,0) TEOF]
}
