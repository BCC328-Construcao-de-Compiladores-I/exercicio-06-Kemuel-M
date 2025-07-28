module L.L2.Frontend.Parser where 

import Control.Applicative (empty)

import L.L2.Frontend.Syntax
import Utils.Value
import Utils.Var

import Control.Monad.Combinators.Expr

import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- definition of a type for parsers

type Parser = Parsec Void String

-- definition of a type for parser errors

type ParserError = ParseErrorBundle String Void

-- | Space consumer that skips spaces and line comments starting with "//".
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "//") empty

-- | Parse a lexeme using 'spaceConsumer'.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- | Parse a fixed symbol.
symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

-- | Parse content surrounded by parentheses.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Reserved words that cannot be used as identifiers.
rwords :: [String]
rwords = ["read", "print", "def", "in", "end"]

-- | Parse an identifier that is not a reserved word.
identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rwords
                 then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                 else return x

-- | Parse an integer value.
integer :: Parser Int
integer = lexeme L.decimal

-- | Parse a string literal using Megaparsec's 'charLiteral'.
stringLiteral :: Parser String
stringLiteral = lexeme (char '"' >> manyTill L.charLiteral (char '"'))

-- | Expression parser.
expr :: Parser E2
expr = makeExprParser term operatorTable

-- | Parse a factor of an expression.
term :: Parser E2
term = choice
  [ LVal . VInt <$> integer
  , LVal . VStr <$> stringLiteral
  , LVar . Var  <$> identifier
  , parens expr
  ]

binary :: String -> (E2 -> E2 -> E2) -> Operator Parser E2
binary name f = InfixL (f <$ symbol name)

operatorTable :: [[Operator Parser E2]]
operatorTable =
  [ [ binary "*" LMul
    , binary "/" LDiv
    ]
  , [ binary "+" LAdd
    , binary "-" LMinus
    ]
  ]

-- | Statement parser.
statement :: Parser S2
statement = choice [try defStmt, try readStmt, try printStmt, assignStmt]
  where
    defStmt = do
      _ <- symbol "def"
      v <- identifier
      _ <- symbol ":="
      e <- expr
      _ <- symbol "in"
      stmts <- many statement
      _ <- symbol "end"
      pure (Def (Var v) e stmts)

    readStmt = do
      _ <- symbol "read"
      (msg, var) <- parens $ do
        s <- stringLiteral
        _ <- symbol ","
        v <- identifier
        pure (s, Var v)
      _ <- symbol ";"
      pure (LRead msg var)

    printStmt = do
      _ <- symbol "print"
      e <- parens expr
      _ <- symbol ";"
      pure (LPrint e)

    assignStmt = do
      v <- identifier
      _ <- symbol ":="
      e <- expr
      _ <- symbol ";"
      pure (LAssign (Var v) e)

-- | Parse a complete L2 program.
program :: Parser L2
program = between spaceConsumer eof (L2 <$> many statement)

-- | Top level parser function used externally.
parserL2 :: String -> Either String L2
parserL2 src = case parse program "<l2>" src of
  Left err -> Left (errorBundlePretty err)
  Right ast -> Right ast