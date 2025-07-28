{
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module L.L2.Frontend.Lexer (Token (..), Lexeme (..), lexer) where 
}

%wrapper "posn"

$digit = 0-9                                               -- digits
$alpha = [a-zA-Z]                                          -- caracteres alfabeticos

-- second Regular Expression macros
@number     = $digit+
@identifier = $alpha ($alpha | $digit)*
@string     = \"([^\\\"\n]|\\.)*\"                         -- String literal entre aspas duplas

-- tokens declarations
tokens :-
      $white+       ;                                      -- ignora espacos em branco
      "//" .*       ;                                      -- ignora comentarios de uma linha
      -- Símbolos e Operadores
      ":="          {simpleToken TAssign}
      ";"           {simpleToken TSemicolon}
      ","           {simpleToken TComma}
      "("           {simpleToken TLParen}
      ")"           {simpleToken TRParen}
      "+"           {simpleToken TPlus}
      "-"           {simpleToken TMinus}
      "*"           {simpleToken TTimes}
      "/"           {simpleToken TDiv}
      -- Palavras-chave (devem vir antes de @identifier para evitar conflitos)
      "def"         {simpleToken TDef}
      "in"          {simpleToken TIn}
      "end"         {simpleToken TEnd}
      "read"        {simpleToken TRead}
      "print"       {simpleToken TPrint}
      -- Tokens com valor
      @number       {mkNumber}
      @identifier   {mkIdentifier}
      @string       {mkString}

{
-- Definição do tipo Token, que armazena a posição e o lexema.
data Token
  = Token {
      pos :: (Int, Int)
    , lexeme :: Lexeme
    } deriving (Eq, Ord, Show)

-- Definição dos tipos de lexemas reconhecidos.
data Lexeme
  = TNumber Int                        -- Números inteiros
  | TId String                         -- Identificadores
  | TString String                     -- String literal
  | TLParen                            -- (
  | TRParen                            -- )
  | TPlus                              -- +
  | TMinus                             -- -
  | TTimes                             -- *
  | TDiv                               -- /
  | TAssign                            -- :=
  | TSemicolon                         -- ;
  | TComma                             -- ,
  | TDef                               -- def
  | TIn                                -- in
  | TEnd                               -- end
  | TRead                              -- read
  | TPrint                             -- print
  | TEOF                               -- Fim de arquivo
  deriving (Eq, Ord, Show)


-- Converte a posição do Alex para uma tupla (linha, coluna).
position :: AlexPosn -> (Int, Int)
position (AlexPn _ x y) = (x,y)

-- Cria um token do tipo número.
mkNumber :: AlexPosn -> String -> Token
mkNumber p s = Token (position p) (TNumber $ read s)

-- Cria um token do tipo identificador.
mkIdentifier :: AlexPosn -> String -> Token
mkIdentifier p s = Token (position p) (TId s)

-- Cria um token do tipo string, removendo as aspas.
mkString :: AlexPosn -> String -> Token
mkString p s = Token (position p) (TString $ (init . tail) s)

-- Cria um token para símbolos simples sem valor associado.
simpleToken :: Lexeme -> AlexPosn -> String -> Token
simpleToken lx p _ = Token (position p) lx

-- A função principal do analisador léxico.
lexer :: String -> [Token]
lexer = alexScanTokens
}