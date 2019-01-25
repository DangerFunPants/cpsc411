module Main
  ( main
  ) where

-- STD LIB includes
import           System.IO

import           AbsM
import           ErrM
import           LexM

-- BNFC Generated Modules
import           ParM

-- Local Imports
import           AST
import           AstTraversal
import           GenIr
import           IR
import           GenCode

import           Data.Tree
import           Text.Show

{-
================================================================================
WRAPPERS FOR HAPPY AND ALEX APIs
================================================================================
-}
lexSource :: String -> [Token]
lexSource s = myLexer s

parseToks :: [Token] -> Err Prog
parseToks ts = pProg ts

runCompiler :: [Token] -> Err (String, String)
runCompiler ts = do
  pt <- parseToks ts
  let ast = traverseProg pt
  let eIr = getIr ast
  case eIr of
    (Left errMsg) -> fail errMsg
    (Right iProg) -> return $ (((drawTree . toTree) iProg), genCode iProg)

main = do
  s <- getContents
  let ts = lexSource s
  case runCompiler ts of
    Ok (ir, code)   -> do
      putStrLn ir
      -- putStrLn code
    Bad err -> hPutStrLn stderr err
