
import L.L2.Interpreter.Interp
import L.L2.Frontend.Syntax
import L.L2.Frontend.Lexer (Token(..), Lexeme(..), lexer)
import L.L2.Frontend.Parser (parserL2)
import L.L2.Frontend.TokenFormat (formatTok)
import L.L2.Frontend.TypeCheck (typeCheck)
import L.L2.Backend.V1Codegen (v1Codegen)
import L.L2.Backend.CCodegen (cL2Codegen)
import Utils.Pretty

import System.Environment
import System.FilePath
import System.Process
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  let opts = parseOptions args
  runWithOptions opts

-- running the compiler / interpreter

runWithOptions :: [Option] -> IO ()
runWithOptions opts = case opts of
  [Lexer file] ->
    lexerOnly file
  [Parser file] ->
    parserOnly file
  [Interpret file] ->
    interpret file
  [VM file] ->
    v1Compiler file
  [C file] ->
    cCompiler file
  _ -> helpMessage


-- Implement the function to do lexical analysis for L2 programs and outputs the tokens

lexerOnly :: FilePath -> IO ()
lexerOnly file = do
  src <- readFile file
  let toks = lexer src
  mapM_ (putStrLn . formatTok) toks


-- Implement the function to do syntax analysis for L2 programs and outputs the syntax tree

parserOnly :: FilePath -> IO ()
parserOnly file = do
  src <- readFile file
  case parserL2 src of
    Left err -> putStrLn err
    Right ast -> putStrLn (show ast)

-- Implement the whole interpreter pipeline: lexical and syntax analysis and then interpret the program

interpret :: FilePath -> IO ()
interpret file = do
  src <- readFile file
  case parserL2 src of
    Left err -> putStrLn err
    Right ast -> do
      case typeCheck ast of
        Left err -> putStrLn err
        Right checkedAst -> do
          result <- evalL2 checkedAst
          case result of
            Left err -> putStrLn err
            Right _ -> return ()

-- Implement the whole compiler pipeline: lexical, syntax and semantic analysis and then generate v1 instructions from the program.

v1Compiler :: FilePath -> IO ()
v1Compiler file = do
  src <- readFile file
  case parserL2 src of
    Left err -> putStrLn err
    Right ast -> do
      case typeCheck ast of
        Left err -> putStrLn err
        Right checkedAst -> do
          let v1Code = v1Codegen checkedAst
          let outputFile = replaceExtension file ".v1"
          writeFile outputFile (pretty v1Code)
          putStrLn $ "V1 code generated: " ++ outputFile

-- Implement the whole executable compiler, using C source and GCC.

cCompiler :: FilePath -> IO ()
cCompiler file = do
  src <- readFile file
  case parserL2 src of
    Left err -> putStrLn err
    Right ast -> do
      case typeCheck ast of
        Left err -> putStrLn err
        Right checkedAst -> do
          let cCode = cL2Codegen checkedAst
          let cFile = replaceExtension file ".c"
          let exeFile = replaceExtension file ""
          writeFile cFile cCode
          putStrLn $ "C code generated: " ++ cFile
          -- Compile with GCC
          result <- system $ "gcc -o " ++ exeFile ++ " " ++ cFile
          case result of
            ExitSuccess -> putStrLn $ "Executable generated: " ++ exeFile
            ExitFailure code -> putStrLn $ "GCC compilation failed with exit code: " ++ show code

-- help message

helpMessage :: IO ()
helpMessage
  = putStrLn $ unlines [ "L2 language"
                       , "Usage: l2 [--lexer-only | --parse-only | --interpret | --help]"
                       , "--lexer-only: does the lexical analysis of the input program."
                       , "--parse-only: does the syntax analysis of the input program."
                       , "--interpret: does the syntax and semantic analysis and interpret the input program."
                       , "--v1: does the syntax and semantic analysis and then generates V1 code."
                       , "--c: does the syntax and semantic analysis, generates C code and uses GCC to generate an executable."
                       , "--help: prints this help message."
                       ]

-- parse command line arguments

data Option
  = Help
  | Lexer FilePath
  | Parser FilePath
  | Interpret FilePath
  | VM FilePath
  | C FilePath
  deriving (Eq, Show)


parseOptions :: [String] -> [Option]
parseOptions args =
  case args of
    ("--lexer-only" : arg : _) -> [Lexer arg]
    ("--parse-only" : arg : _) -> [Parser arg]
    ("--interpret" : arg : _) -> [Interpret arg]
    ("--v1" : arg : _) -> [VM arg]
    ("--c" : arg : _) -> [C arg]
    _ -> [Help]
