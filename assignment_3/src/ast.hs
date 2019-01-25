module AST
  ( M_prog(..)
  , M_decl(..)
  , M_stmt(..)
  , M_type(..)
  , M_expr(..)
  , M_operation(..)
  ) where

import           Control.Applicative
import           Control.Monad       (ap, liftM)

{-
================================================================================
STATE MONAD DEFINITION
================================================================================
-}
newtype State s a = State
  { runState :: s -> (a, s)
  }

instance Functor (State s) where
  fmap = liftM

instance Applicative (State s) where
  pure = return
  (<*>) = ap

instance Monad (State s) where
  return x = State $ \s -> (x, s)
  (State h) >>= f =
    State $ \s ->
      let (a, newState) = h s
          (State g) = f a
      in g newState

{-
================================================================================
ABSTRACT SYNTAX TREE FOR M-
================================================================================
-}
data M_prog =
  M_prog ([M_decl], [M_stmt])

data M_decl
  = M_var (String, [M_expr], M_type)
  | M_fun (String, [(String, Int, M_type)], M_type, [M_decl], [M_stmt])
  | M_data (String, [(String, [M_type])])

data M_stmt
  = M_ass (String, [M_expr], M_expr)
  | M_while (M_expr, M_stmt)
  | M_cond (M_expr, M_stmt, M_stmt)
  | M_read (String, [M_expr])
  | M_print M_expr
  | M_return M_expr
  | M_block ([M_decl], [M_stmt])
  | M_case (M_expr, [(String, [String], M_stmt)])

data M_type
  = M_int
  | M_bool
  | M_real
  | M_char
  | M_type String

data M_expr
  = M_ival Integer
  | M_rval Float
  | M_bval Bool
  | M_cval Char
  | M_size (String, Int)
  | M_id (String, [M_expr])
  | M_app (M_operation, [M_expr])

data M_operation
  = M_fn String
  | M_cid String
  | M_add
  | M_mul
  | M_sub
  | M_div
  | M_neg
  | M_lt
  | M_le
  | M_gt
  | M_ge
  | M_eq
  | M_not
  | M_and
  | M_or
  | M_float
  | M_floor
  | M_ceil

instance Show M_prog where
  show prog =
    let statefulComp = printProg prog
    in fst $ (runState statefulComp) ""

type ParseDepth = State String

{-
================================================================================
PARSE DEPTH STATE MANIPULATORS
================================================================================
-}
increaseDepth :: ParseDepth ()
increaseDepth = State $ \s -> ((), s ++ "\t")

decreaseDepth :: ParseDepth ()
decreaseDepth = State $ \s -> ((), tail s)

getIndent :: ParseDepth String
getIndent = State $ \s -> (s, s)

{-
================================================================================
M- AST PRETTY PRINTER
================================================================================
-}
{-
================================================================================
OPERATIONS
================================================================================
-}
printOperation :: M_operation -> String
printOperation (M_fn str)  = str
printOperation (M_add)     = "+"
printOperation (M_mul)     = "*"
printOperation (M_sub)     = "-"
printOperation (M_div)     = "\\"
printOperation (M_neg)     = "-"
printOperation (M_lt)      = "<"
printOperation (M_le)      = "<="
printOperation (M_gt)      = ">"
printOperation (M_ge)      = ">="
printOperation (M_eq)      = "="
printOperation (M_not)     = "not"
printOperation (M_and)     = "&&"
printOperation (M_or)      = "||"
printOperation (M_float)   = "float"
printOperation (M_floor)   = "floor"
printOperation (M_ceil)    = "ceil"
printOperation (M_cid cid) = cid

{-
================================================================================
EXPRESSIONS
================================================================================
-}
printExpr :: M_expr -> String
printExpr (M_ival n) = show n
printExpr (M_rval n) = show n
printExpr (M_bval b) = show b
printExpr (M_size (id, dims)) =
  "size(" ++ id ++ (concat $ replicate dims "[]") ++ ")"
printExpr (M_id (id, exprList)) = id ++ (printArrDims exprList)
printExpr (M_app (op, argList)) = op' ++ "(" ++ (intersperse argList) ++ ")"
  where
    intersperse (p1:p2:ps) = (printExpr p1) ++ ", " ++ (intersperse (p2 : ps))
    intersperse (p1:ps)    = printExpr p1
    intersperse []         = []
    op' = printOperation op

{-
================================================================================
TYPES
================================================================================
-}
printType :: M_type -> String
printType (M_int)           = "int"
printType (M_real)          = "real"
printType (M_bool)          = "bool"
printType (M_char)          = "char"
printType (M_type typeName) = typeName

{-
================================================================================
STATEMENTS
================================================================================
-}
printStmt :: M_stmt -> ParseDepth String
printStmt (M_ass (id, modList, rVal)) = do
  indent <- getIndent
  let arrDims = printArrDims modList
  let retVal = printExpr rVal
  return $ indent ++ id ++ arrDims ++ " := " ++ retVal ++ ";\n"
printStmt (M_while (expr, stmt)) = do
  indent <- getIndent
  let condStr = printExpr expr
  increaseDepth
  doPartStr <- printStmt stmt
  decreaseDepth
  return $
    indent ++
    "while " ++
    condStr ++ " do \n" ++ indent ++ "{\n" ++ doPartStr ++ indent ++ "};\n"
printStmt (M_cond (expr, thenStmt, elseStmt)) = do
  indent <- getIndent
  let condStr = printExpr expr
  increaseDepth
  thenPartStr <- printStmt thenStmt
  elsePartStr <- printStmt elseStmt
  decreaseDepth
  return $
    indent ++
    "if " ++
    condStr ++
    " then\n" ++
    indent ++
    "{\n" ++
    thenPartStr ++
    indent ++
    "}\n" ++
    indent ++ "else\n" ++ indent ++ "{\n" ++ elsePartStr ++ indent ++ "};\n"
printStmt (M_read (id, modList)) = do
  indent <- getIndent
  return $ indent ++ "read " ++ id ++ (printArrDims modList) ++ ";\n"
printStmt (M_print expr) = do
  indent <- getIndent
  return $ indent ++ "print " ++ (printExpr expr) ++ ";\n"
printStmt (M_return expr) = do
  indent <- getIndent
  return $ indent ++ "return " ++ (printExpr expr) ++ ";\n"
printStmt (M_block (declList, stmtList)) = do
  indent <- getIndent
  let declStr = fmap printDecl declList
  increaseDepth
  let stmtStr = fmap printStmt stmtList
  let declRes = concat (map (\s -> (fst $ runState s indent)) declStr)
  let stmtRes = concat (map (\s -> (fst $ runState s ('\t' : indent))) stmtStr)
  decreaseDepth
  return $
    declRes ++ "\n" ++ indent ++ "begin\n" ++ stmtRes ++ indent ++ "end\n"
printStmt (M_case (expr, caseList)) = do
  indent <- getIndent
  let exprStr = printExpr expr
  increaseDepth
  caseStr <- mapM printCase caseList
  let cl' = concat caseStr
  decreaseDepth
  return $
    indent ++
    "case " ++ exprStr ++ " of\n" ++ indent ++ "{\n" ++ cl' ++ indent ++ "};\n"

{-
================================================================================
DECLARATIONS
================================================================================
-}
printDecl :: M_decl -> ParseDepth String
printDecl (M_var (id, modList, t)) = do
  indent <- getIndent
  let varType = printType t
  return $
    indent ++
    "var " ++ id ++ (printArrDims modList) ++ " : " ++ varType ++ ";\n"
printDecl (M_fun (id, paramList, retType, bodyDecl, bodyStmt)) = do
  protoIndent <- getIndent
  increaseDepth
  indent <- getIndent
  let varType = printType retType
  declStr <- mapM printDecl bodyDecl
  increaseDepth
  stmtStr <- mapM printStmt bodyStmt
  decreaseDepth
  decreaseDepth
  let declStr' = concat declStr
  let stmtStr' = concat stmtStr
  return $
    protoIndent ++
    "fun " ++
    id ++
    (printParamList paramList) ++
    " : " ++
    varType ++
    "\n" ++
    protoIndent ++
    "{" ++
    "\n" ++
    declStr' ++
    indent ++
    "begin\n" ++ stmtStr' ++ indent ++ "end\n" ++ protoIndent ++ "};\n"
printDecl (M_data (cid, consList)) = do
  indent <- getIndent
  increaseDepth
  cidIndent <- getIndent
  decreaseDepth
  return $
    indent ++
    "data " ++ cid ++ cidIndent ++ "\n = " ++ (printConsList consList) ++ "\n"

{-
================================================================================
PROGRAM
================================================================================
-}
printProg :: M_prog -> ParseDepth String
printProg (M_prog (declList, stmtList)) = do
  cindent <- getIndent
  declStr <- mapM printDecl declList
  increaseDepth
  stmtStr <- mapM printStmt stmtList
  decreaseDepth
  let declStr' = concat declStr
  let stmtStr' = concat stmtStr
  return $ declStr' ++ "begin\n" ++ stmtStr' ++ "end\n"

{-
================================================================================
PRETTY PRINTER HELPER FUNCTIONS
================================================================================
-}
printArrDims :: [M_expr] -> String
printArrDims modList = concat $ fmap (\e -> "[" ++ (printExpr e) ++ "]") modList

printParamList :: [(String, Int, M_type)] -> String
printParamList [] = "()"
printParamList paramList =
  "(" ++ (init (concat $ fmap (printParam) paramList)) ++ ")"

printParam :: (String, Int, M_type) -> String
printParam (id, dims, t) =
  id ++ (concat $ replicate dims "[]") ++ " : " ++ (printType t) ++ ","

printConsList :: [(String, [M_type])] -> String
printConsList (c1:c2:cs) =
  (printCons c1) ++ " | " ++ (printConsList (c2 : cs)) ++ "\n"
printConsList (c1:cs) = printCons c1
printConsList [] = []

printCons :: (String, [M_type]) -> String
printCons (cid, typeList) = cid ++ " " ++ (printTypeList typeList)

printTypeList :: [M_type] -> String
printTypeList (t1:t2:ts) = (printType t1) ++ " * " ++ (printTypeList (t2 : ts))
printTypeList (t1:ts) = (printType t1)
printTypeList [] = []

printCase :: (String, [String], M_stmt) -> ParseDepth String
printCase (cid, paramList, stmt) = do
  indent <- getIndent
  let argList = "(" ++ (intersperse paramList) ++ ")"
        where
          intersperse (p1:p2:ps) = p1 ++ "," ++ (intersperse (p2 : ps))
          intersperse (p:ps)     = p
          intersperse []         = []
  increaseDepth
  stmtIndent <- getIndent
  decreaseDepth
  let stmtStr = fst $ ((runState stateComp) stmtIndent)
        where
          stateComp = printStmt stmt
  return $ indent ++ cid ++ argList ++ " => \n" ++ stmtStr
