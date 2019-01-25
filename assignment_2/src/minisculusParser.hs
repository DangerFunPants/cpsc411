{-
Title: AST generator for the Minisculus programming Language
Author: Alexander James
-}

-- export the parseStmt function and AST data types
module Parser (parseTokens) where

-- imports for lexer as well as token type definition
import Lexer
import Debug.Trace
import AST

import Control.Applicative
import Control.Monad (liftM, ap)

{-
================================================================================
Minisculus grammar
================================================================================
The minisculus grammer is given as the following: 

    prog -> stmt. 
    stmt -> IF expr THEN stmt ELSE stmt
            | WHILE expr DO stmt
            | INPUT ID
            | ID ASSIGN expr
            | WRITE expr
            | BEGIN stmtlist END
    stmtlist -> stmtlist stmt SEMICOLON
            |. 
    expr -> expr addop term 
            | term
    addop -> ADD
            | SUB. 
    term -> term mulop factor 
            | factor
    mulop -> MUL
            | DIV
    factor -> LPAR expr RPAR
            | ID
            | NUM
            | SUB NUM

Three of these productions contain immediate left recursion, namely stmtlist, expr and term. These need to be removed
to allow for expressions produced by this grammar to be parsed using recursive descent parsing.

================================================================================
Removing left recursion from term and expr
================================================================================
Using a well known substitution rule, the production:

expr -> expr addop term
       | term

can be transformed into: 

expr -> term expr'
expr' -> addop term expr'
        | \epsilon

where \epsilon denotes the empty production. Applying the same method to term yields the following: 

term -> factor term'
term' -> mulop factor term'
        | \epsilon

================================================================================
Exposing terminal symbols to allow for pattern matching
================================================================================
The two transformed productions listed above no longer include left recursion, however, they begin with non-terminals,
both of which can be expanded using their productions to simplify pattern matching and allow the implementation
of the parser to closely parallel the definition of non-terminal productions. 

Expression: 

expr -> term expr'
expr' -> ADD term expr'
        | SUB term expr'
        | \epsilon

Term: 

term -> factor term'
term' -> MUL factor term'
        | DIV factor term'
        | \epsilon

================================================================================
Removing left recursion from stmtlist
================================================================================
I believe that the production:

stmtlist -> stmt SEMICOLON stmtlist
           | \epsilon

successfully eliminates the left recursion from the original production: 

stmtlist -> stmtlist stmt SEMICOLON

However, a document found online authored by Brett Gilles supposes the following production for a very similar grammar: 

stmtlist -> stmt SEMICOLON stmtlist
           | stmt

this seems somewhat incorrect as the non recursive production for stmtlist (stmtlist -> stmt) does not specify the 
terminating semicolon in the case that a single statement is given in the stmtlist.

================================================================================
Transformed minsculus grammar
================================================================================

    prog -> stmt
    stmt -> IF expr thenpart
            | WHILE expr DO stmt
            | INPUT ID
            | ID ASSIGN expr
            | WRITE expr
            | BEGIN stmtlist END
    thenpart -> THEN stmt elsepart
    elsepart -> ELSE stmt
    stmtlist -> stmt SEMICOLON stmtlist
                | \epsilon
    expr -> term expr'
    expr' -> ADD term expr'
            | SUB term expr'
            | \epsilon
    term -> factor term'
    term' -> MUL factor term'
            | DIV factor term'
            | \epsilon
    factor -> LPAR expr RPAR
            | ID
            | NUM
            | SUB NUM


================================================================================
Type to represent the AST for minsculus
================================================================================

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
                | Num Integer deriving (Show, Eq)

================================================================================
Helper functions to build error strings.
================================================================================
-}

decorateError :: Lexeme -> AlexPosn -> String
decorateError tok posn = "Error on token: "
                         ++(show tok)++" at position: "
                         ++(show posn)
{-
================================================================================
Methods corresponding to non-termineal productions in Minisculus Grammar
================================================================================
-}

parseStmt :: [Token] -> ErrorMonad (Stmt, [Token])

parseStmt ((Wrap TIf _):toks) = do
    (cond, toks1) <- parseExpression toks
    (ifCons, toks2) <- parseThenPart (If cond) toks1
    (ifStmt, toks') <- parseElsePart ifCons toks2
    return (ifStmt, toks')
    
parseStmt ((Wrap TWhile _):toks) = do
    (cond, toks1) <- parseExpression toks
    (doPart, toks') <- parseStmt $ tail toks1
    return ((While cond doPart), toks')

parseStmt ((Wrap (TId varId) _):toks) = do
    (assignExpr, toks') <- parseExpression $ tail toks
    return ((Assign varId assignExpr), toks')

parseStmt ((Wrap TInput _):toks) = do
    (inputExpr, toks') <- parseExpression toks
    return ((Input inputExpr), toks')

parseStmt ((Wrap TBegin _):toks) = do
    (listOfStmt, toks') <- parseStmtList toks
    return ((Block listOfStmt), toks')

parseStmt ((Wrap TWrite _):toks) = do
    (outExpr, toks') <- parseExpression toks
    return ((Print outExpr), toks')
    
parseStmt ((Wrap tok  pos):toks) = fail (decorateError tok pos)

parseThenPart :: (Stmt -> Stmt -> Stmt) -> [Token] -> ErrorMonad ((Stmt -> Stmt), [Token])
parseThenPart ifConstructor ((Wrap TThen _):toks) = do
    -- parse the stmt
    (thenPart, toks') <- (parseStmt toks)
    -- partially apply the ifConstructor and return
    return ((ifConstructor thenPart), toks')

parseThenPart _ ((Wrap tok pos):toks) = fail (decorateError tok pos)

parseElsePart :: (Stmt -> Stmt) -> [Token] -> ErrorMonad (Stmt, [Token])
parseElsePart ifConstructor ((Wrap TElse _):toks) = do
    (elsePart, toks') <- (parseStmt toks)
    return ((ifConstructor elsePart), toks')

parseElsePart _ ((Wrap tok pos):toks) = fail (decorateError tok pos)

parseExpression :: [Token] -> ErrorMonad (Expr, [Token])

parseExpression (toks) = do
    (expr, toks1) <- parseTerm toks
    parsed <- parseExpression' (expr, toks1)
    return parsed


parseExpression' :: (Expr, [Token]) -> ErrorMonad (Expr, [Token])

parseExpression' (expr, (Wrap TAdd _):toks) = do
    (expr1, toks1) <- parseTerm toks
    (newExpr, toks') <- parseExpression' (expr1, toks1)
    return ((Add expr newExpr), toks')

parseExpression' (expr, (Wrap TSub _):toks) = do
    (expr1, toks1) <- parseTerm toks
    (newExpr, toks') <- parseExpression' (expr1, toks1)
    return ((Sub expr newExpr), toks')

parseExpression' (expr, toks) = return (expr, toks)

parseTerm :: [Token] -> ErrorMonad (Expr, [Token])

parseTerm (toks) = do
    (parsed1) <- parseFactor toks
    (parsed') <- parseTerm' parsed1
    return parsed'

parseTerm' :: (Expr, [Token]) -> ErrorMonad (Expr, [Token])

parseTerm' (expr, (Wrap TMul _):toks) = do
    (expr1, toks1) <- parseFactor toks
    (newExpr, toks') <- parseTerm' (expr1, toks1)
    return ((Mul expr newExpr), toks')

parseTerm' (expr, (Wrap TDiv _):toks) = do
    (expr1, toks1) <- parseFactor toks
    (newExpr, toks') <- parseTerm' (expr1, toks1)
    return ((Div expr newExpr), toks')

parseTerm' (expr, toks) = return (expr, toks)

parseFactor :: [Token] -> ErrorMonad (Expr, [Token])

parseFactor ((Wrap TLPar _):toks) = do
    (exprVal, toks1) <- parseExpression toks
    toks' <- (case toks1 of 
                ((Wrap TRPar _):rest) -> (return rest)
                ((Wrap tok pos):rest) -> fail (decorateError tok pos)) -- strip the RPar
    return (exprVal, toks')

parseFactor ((Wrap (TId tokId) _):toks) = return ((Id tokId), toks)

parseFactor ((Wrap (TNum numVal) _):toks) = return ((Num numVal), toks)

parseFactor ((Wrap TSub _):(Wrap (TNum val) _):toks') = do
    return ((Neg (Num val)), toks')

parseFactor ((Wrap tok pos):ts) = fail (decorateError tok pos)

parseStmtList :: [Token] -> ErrorMonad ([Stmt], [Token])

parseStmtList ((Wrap TEnd _):toks) = return ([], toks)

parseStmtList (toks) = do
    (newStmt, toks1) <- parseStmt toks
    case toks1 of 
        ((Wrap TSemicolon _):toks2) -> do
                                (recCall, toks') <- parseStmtList toks2
                                return ((newStmt:recCall), toks')
        otherwise -> case (toks1) of ((Wrap tok pos):toks) -> fail (decorateError tok pos)

parseTokens :: [Token] -> ErrorMonad Stmt
parseTokens toks = do
    (stmt, toks') <- parseStmt toks
    case (toks') of
        [] -> return stmt
        (Wrap tok posn):xs -> fail ("Error on token "++(show tok)++" at position: "++(show posn))