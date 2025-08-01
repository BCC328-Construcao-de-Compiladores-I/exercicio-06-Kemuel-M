module L.L1.Frontend.RecursiveParser where 

import Control.Applicative (empty)

import L.L1.Frontend.Syntax
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
rwords = ["read", "print"]

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
expr :: Parser E1
expr = makeExprParser term operatorTable

-- | Parse a factor of an expression.
term :: Parser E1
term = choice
  [ LVal . VInt <$> integer
  , LVal . VStr <$> stringLiteral
  , LVar . Var  <$> identifier
  , parens expr
  ]

binary :: String -> (E1 -> E1 -> E1) -> Operator Parser E1
binary name f = InfixL (f <$ symbol name)

operatorTable :: [[Operator Parser E1]]
operatorTable =
  [ [ binary "*" LMul
    , binary "/" LDiv
    ]
  , [ binary "+" LAdd
    , binary "-" LMinus
    ]
  ]

-- | Statement parser terminated by a semicolon.
statement :: Parser S1
statement = choice [try readStmt, try printStmt, assignStmt] <* symbol ";"
  where
    readStmt = do
      _ <- symbol "read"
      (msg, var) <- parens $ do
        s <- stringLiteral
        _ <- symbol ","
        v <- identifier
        pure (s, Var v)
      pure (LRead msg var)

    printStmt = do
      _ <- symbol "print"
      e <- parens expr
      pure (LPrint e)

    assignStmt = do
      v <- identifier
      _ <- symbol ":="
      e <- expr
      pure (LAssign (Var v) e)

-- | Parse a complete L1 program.
program :: Parser L1
program = between spaceConsumer eof (L1 <$> many statement)

-- | Top level parser function used externally.
parserRecursiveL1 :: String -> Either String L1
parserRecursiveL1 src = case parse program "<l1>" src of
  Left err -> Left (errorBundlePretty err)
  Right ast -> Right ast