module L.L2.Frontend.TokenFormat (formatTok) where

import L.L2.Frontend.Lexer (Token(..), Lexeme(..))


-- Converte um 'Token' em uma string legível
formatTok :: Token -> String
formatTok (Token (l, c) lx) =
  case lx of
    TId s        -> "Identificador "                       ++ s                ++ pos
    TNumber n    -> "Número "                              ++ show n           ++ pos
    TString s    -> "String "                              ++ show s           ++ pos
    TAssign      -> "Atribuição :="                        ++ pos
    TPlus        -> "Soma +"                               ++ pos
    TMinus       -> "Subtração -"                          ++ pos
    TTimes       -> "Multiplicação *"                      ++ pos
    TDiv         -> "Divisão /"                            ++ pos
    TLParen      -> "Parêntesis ("                         ++ pos
    TRParen      -> "Parêntesis )"                         ++ pos
    TSemicolon   -> "Ponto e vírgula ;"                    ++ pos
    TComma       -> "Vírgula ,"                            ++ pos
    TDef         -> "Palavra reservada def"                ++ pos
    TIn          -> "Palavra reservada in"                 ++ pos
    TEnd         -> "Palavra reservada end"                ++ pos
    TRead        -> "Palavra reservada read"               ++ pos
    TPrint       -> "Palavra reservada print"              ++ pos
    TEOF         -> "Fim de arquivo"                       ++ pos
  where
    pos = " Linha:" ++ show l ++ " Coluna:" ++ show c
