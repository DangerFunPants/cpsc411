module Main (main) where

import System.IO            -- hPutStrLn, stderr
import System.Environment   -- getArgs

-- Local includes
import Lexer
import Parser
import CodeGen
import AST

runCompiler :: String -> ErrorMonad (String, String)
runCompiler str = do
    toks <- lexer str
    ast <- parseTokens toks
    let stackCode = generateCode ast
    return (stackCode, (show ast))

main :: IO ()
main = do
    args <- getArgs
    let outFileName = case args of
                        (x:xs) -> x
                        otherwise -> "out.a" 
    s <- getContents
    let code = runCompiler s
    case code of
        Err (Left msg) -> hPutStrLn stderr msg
        Err (Right (asm, pp)) -> do 
                            writeFile outFileName asm
                            putStrLn pp