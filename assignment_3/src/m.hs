module Main (main) where

-- STD LIB includes
import System.IO

-- BNFC Generated Modules
import ParM
import ErrM
import LexM
import AbsM

-- Local Imports
import AST
import AstTraversal

{-
================================================================================
WRAPPERS FOR HAPPY AND ALEX APIs
================================================================================
-}

lexSource :: String -> [Token]
lexSource s = myLexer s

parseToks :: [Token] -> Err Prog
parseToks ts = pProg ts

runCompiler :: [Token] -> Err String
runCompiler ts = do
    pt <- parseToks ts
    let ast = traverseProg pt
    return (show ast)

main = do
        s <- getContents
        let ts = lexSource s
        case runCompiler ts of
            Ok ast -> putStrLn ast
            Bad err -> hPutStrLn stderr err


