module CodeGen (generateCode) where

import AST

{-

================================================================================
Transformed Minisculus Grammar (for reference)
================================================================================
    prog -> stmt
    stmt -> IF expr THEN stmt ELSE stmt
            | WHILE expr DO stmt
            | INPUT ID
            | ID ASSIGN expr
            | WRITE expr
            | BEGIN stmtlist END
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
Stmt and Expr Types (for reference)
================================================================================

data Stmt = If Expr Stmt Stmt
          | While Expr Stmt
          | Assign String Expr
          | Block [Stmt]
          | Print Expr
          | Input Expr deriving (Show, Eq)

data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Neg Expr
          | Id String
          | Num Integer deriving (Show, Eq)

-}

type LabelState = State Int

getNextLabel :: LabelState Int
getNextLabel = State $ \s -> (s, (succ s))

generateStmtCode :: Stmt -> LabelState String

generateStmtCode (If cond thenPart elsePart) = do
    let condCode = generateExprCode cond
    l1 <- getNextLabel
    l2 <-getNextLabel
    thenPartCode <- generateStmtCode thenPart
    elsePartCode <- generateStmtCode elsePart
    return 
        (condCode++"\n"
        ++"cJUMP L"++(show l1)++"\n"
        ++thenPartCode++"\n"
        ++"JUMP L"++(show l2)++"\n"
        ++"L"++(show l1)++":\n"
        ++elsePartCode++"\n"
        ++"L"++(show l2)++":\n")

generateStmtCode (While cond doPart) = do
    let condCode = generateExprCode cond
    l1 <- getNextLabel
    l2 <- getNextLabel
    doPartCode <- generateStmtCode doPart
    return 
        ("L"++(show l1)++":\n"
        ++condCode++"\n"
        ++"cJUMP L"++(show l2)++"\n"
        ++doPartCode++"\n"
        ++"JUMP L"++(show l1)++"\n"
        ++"L"++(show l2)++":\n")

generateStmtCode (Assign varName expr) = do
    let exprCode = generateExprCode expr
    return 
        (exprCode++"\n"
        ++"LOAD "++(varName)++"\n")

generateStmtCode (Block (first:rest)) = do
    firstCode <- generateStmtCode first
    recCall <- generateStmtCode $ Block rest
    return (firstCode++recCall)

generateStmtCode (Block []) = return ""

generateStmtCode (Print expr) = do
    let exprCode = generateExprCode expr
    return
        (exprCode++"\n"
        ++"PRINT"++"\n")

generateStmtCode (Input (Id varName)) = do
    return ("READ "++varName++"\n")

generateExprCode :: Expr -> String

generateExprCode (Neg expr) = 
          exprCode++"\n"
        ++"cPUSH -1"++"\n"
        ++"OP2 *"++"\n" where
    exprCode = (generateExprCode expr)

generateExprCode (Id varName) = 
          "rPUSH "++varName++"\n"

generateExprCode (Num numVal) = 
          "cPUSH "++(show numVal)++"\n"

generateExprCode (Add lhs rhs) = 
          lhsCode++"\n"
        ++rhsCode++"\n"
        ++"OP2 +"++"\n" where
    lhsCode = (generateExprCode lhs)
    rhsCode = generateExprCode rhs

generateExprCode (Sub lhs rhs) = 
          lhsCode++"\n"
        ++rhsCode++"\n"
        ++"OP2 -"++"\n" where 
    lhsCode = (generateExprCode lhs)
    rhsCode = (generateExprCode rhs)

generateExprCode (Mul lhs rhs) = 
          lhsCode++"\n"
        ++rhsCode++"\n"
        ++"OP2 *"++"\n" where
    lhsCode = (generateExprCode lhs)
    rhsCode = (generateExprCode rhs)

generateExprCode (Div lhs rhs) = 
          lhsCode++"\n"
        ++rhsCode++"\n"
        ++"OP2 /"++"\n" where
    lhsCode = (generateExprCode lhs)
    rhsCode = (generateExprCode rhs)

generateCode :: Stmt -> String
generateCode stmt = 
    let stComp = generateStmtCode stmt in
    fst $ (runState stComp) 0