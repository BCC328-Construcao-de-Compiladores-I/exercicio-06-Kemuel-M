import L.L1.Backend.CCodegen
import L.L1.Backend.V1Codegen
import L.L1.Interpreter.Interp
import L.L1.Frontend.Lexer 
import L.L1.Frontend.RecursiveParser
import L.L1.Frontend.LALRParser (parseLALRL1)
import L.L1.Frontend.Syntax
import L.L1.Frontend.TokenFormat (formatTok)
import Utils.Pretty
import Utils.Repl
import Utils.Value
import V.V0.Instr

import System.Environment
import System.FilePath
import System.Process 

main :: IO ()
main = do
  args <- getArgs
  let opts = parseOptions args
  runWithOptions opts 

-- running the compiler / interpreter 

runWithOptions :: [Option] -> IO ()
runWithOptions opts = case opts of 
  [Lexer file] ->
    alexBasedLexer file
  [Recursive file] -> 
    recursiveParser file
  [LALR file] -> 
    lalrParser file 
  _ -> helpMessage


-- Faz a análise léxica de L1 e imprime os tokens com posição.
alexBasedLexer :: FilePath -> IO ()
alexBasedLexer file = do
  src <- readFile file
  let toks = lexer src
  mapM_ (putStrLn . formatTok) toks


-- Implement the function to do syntax analysis using a recursive parser

recursiveParser :: FilePath -> IO ()
recursiveParser file = do
  src <- readFile file
  case parserRecursiveL1 src of
    Left err -> putStrLn err
    Right ast -> putStrLn (show ast)

-- Implement the LALR parser 

lalrParser :: FilePath -> IO ()
lalrParser file = do
  src <- readFile file
  case parseLALRL1 src of
    Left err -> putStrLn err
    Right ast -> putStrLn (show ast)

-- help message

helpMessage :: IO ()
helpMessage 
  = putStrLn $ unlines [ "L1 language" 
                       , "Usage: l1 [--lexer-only | --recursive | --help]"
                       , "--lexer-only: does the lexical analysis of the input programming using a Alex based lexer."
                       , "--recursive: does the syntax analysis using a recursive descendent Megaparsec parser."
                       , "--lalr: does the syntax analysis using a LALR parser."
                       , "--help: prints this help message."
                       ]

-- parse command line arguments 

data Option 
  = Help 
  | Lexer FilePath
  | Recursive FilePath
  | LALR FilePath 
  deriving (Eq, Show)

parseOptions :: [String] -> [Option]
parseOptions args = 
  case args of 
    ("--lexer-only" : arg : _) -> [Lexer arg]
    ("--recursive" : arg : _) -> [Recursive arg]
    ("--lalr" : arg : _) -> [LALR arg]
    _ -> [Help]
