
module AST (Stmt (..), Expr (..), State (..), ErrorMonad (..)) where

import Control.Applicative
import Control.Monad (liftM, ap)

{-
================================================================================
ADT to represent the Abstract Syntax Tree of Minisculus
================================================================================
-}

data Stmt = If Expr Stmt Stmt
            | While Expr Stmt
            | Assign String Expr
            | Block [Stmt]
            | Print Expr
            | Input Expr deriving (Eq)

data Expr = Add Expr Expr
            | Sub Expr Expr
            | Mul Expr Expr
            | Div Expr Expr
            | Neg Expr
            | Id String
            | Num Integer deriving (Eq)

instance Show Stmt where
    show stmt = prettyPrint stmt

{-
================================================================================
STATE MONAD DEFINITION
================================================================================
I haven't quite figured out monad transformers like ErrorT and StateT so I'm using the "plain" versions of the
Error and State monads.
-}

newtype State s a = State { runState :: s -> (a,s) }  

instance Functor (State s) where
    fmap = liftM

instance Applicative (State s) where 
    pure = return
    (<*>) = ap

instance Monad (State s) where  
    return x = State $ \s -> (x,s)  
    (State h) >>= f = State $ \s -> let (a, newState) = h s  
                                        (State g) = f a  
                                    in  g newState


{-
================================================================================
SUM TYPE FOR ERROR PROPOGATION
================================================================================
-}

newtype ErrorMonad a = Err (Either String a) deriving (Show)

instance Functor ErrorMonad where
    fmap = liftM

instance Applicative ErrorMonad where 
    pure = return
    (<*>) = ap

instance Monad ErrorMonad where
    (Err par) >>= fn = case par of
        Left msg -> Err (Left msg)
        Right arg -> fn arg
    
    return arg = Err (Right arg)

    fail errorMsg = Err (Left errorMsg)

type ParseDepth = State String

increaseDepth :: ParseDepth ()
increaseDepth = State $ \s -> ((), '\t':s)

decreaseDepth :: ParseDepth ()
decreaseDepth = State $ \s -> ((), tail s)

getIndent :: ParseDepth String
getIndent = State $ \s -> (s, s)

printExpr :: Expr -> String
printExpr (Neg expr) = 
    "-"++exprStr where
        exprStr = printExpr expr

printExpr (Id varName) = 
    (varName)

printExpr (Num numVal) = 
    (show numVal)

printExpr (Add lhs rhs) = 
    "("++lhsStr++"+"++rhsStr++")" where
        lhsStr = printExpr lhs
        rhsStr = printExpr rhs

printExpr (Sub lhs rhs) = 
    "("++lhsStr++"-"++rhsStr++")" where
        lhsStr = printExpr lhs
        rhsStr = printExpr rhs

printExpr (Mul lhs rhs) = 
    "("++lhsStr++"*"++rhsStr++")" where
        lhsStr = printExpr lhs
        rhsStr = printExpr rhs

printExpr (Div lhs rhs) = 
    "("++lhsStr++"\\"++rhsStr++")" where
        lhsStr = printExpr lhs
        rhsStr = printExpr rhs

printStmt :: Stmt -> ParseDepth String

printStmt (If cond thenPart elsePart) = do
    indent <- getIndent
    let condStr = (printExpr cond)
    increaseDepth
    thenStr <- printStmt thenPart
    elseStr <- printStmt elsePart
    decreaseDepth
    return 
        (indent
        ++"if "++condStr++" then\n"
        ++thenStr++"\n"
        ++indent++"else\n"
        ++elseStr)

printStmt (While cond doPart) = do
    indent <- getIndent
    let condStr = (printExpr cond)
    increaseDepth
    doPartStr <- printStmt doPart
    decreaseDepth
    return 
        (indent++"while "++condStr++" do "++"\n"++doPartStr)

printStmt (Assign varName expr) = do
    indent <- getIndent
    let exprStr = printExpr expr
    return 
        (indent++varName++" := "++exprStr)

printStmt (Block stmtList) = do
    indent <- getIndent
    increaseDepth
    let l = (map printStmt stmtList)
    let res = concat (map (\s -> (fst $ runState s ('\t':indent))++";\n") l) 
    decreaseDepth
    return 
        (indent
        ++"begin\n"
        ++res
        ++indent
        ++"end")
        
printStmt (Print expr) = do
    indent <- getIndent
    let exprStr = (printExpr expr)
    return 
        (indent++"print "++exprStr)

printStmt (Input (Id varName)) = do
    indent <- getIndent
    return (indent++"read "++(varName))

prettyPrint :: Stmt -> String
prettyPrint stmt = let stComp = printStmt stmt in
    fst $ (runState stComp) ""