module IR
  ( IProg(..)
  , IFunBody(..)
  , IStmt(..)
  , IExpr(..)
  , IOpn(..)
  , DeclWrapper(..)
  , insertSymbol
  , lookupSymbol
  , toTree
  ) where

import           AST
import           SymbolTable

import           Data.Tree

data IProg
    -- funcs, local var count, array descriptions, prog stmts
      =
  IProg ([IFunBody], Int, [(Int, [IExpr])], [IStmt])
  deriving (Show)

data IFunBody
    -- name, localFuncs, localVarCount, argCount, arrayDescription, body
      =
  IFunBody (String, [IFunBody], Int, Int, [(Int, [IExpr])], [IStmt])
  deriving (Show)

data DeclWrapper
  = FunDecl IFunBody
  | ArrDecl (Int, [IExpr])

data IStmt
    -- Level Offset Indexes RHS
  = IAssign (Int, Int, [IExpr], IExpr)
  | IWhile (IExpr, IStmt)
  | ICond (IExpr, IStmt, IStmt)
    -- (CaseExpr, [(consNum, argCount, stmt)])
  | ICase (IExpr, [(Int, Int, IStmt)])
  | IReadB (Int, Int, [IExpr])
  | IPrintB IExpr
  | IReadI (Int, Int, [IExpr])
  | IPrintI IExpr
  | IReadF (Int, Int, [IExpr])
  | IPrintF IExpr
  | IReadC (Int, Int, [IExpr])
  | IPrintC IExpr
  | IReturn IExpr
  | IBlock ([IFunBody], Int, [(Int, [IExpr])], [IStmt])
  deriving (Show)

data IExpr
  = IIVal Int
  | IRVal Float
  | IBVal Bool
  | ICVal Char
  | IId (Int, Int, [IExpr]) -- level offset ArrayIndices
  | IApp (IOpn, [IExpr])
  | IRef (Int, Int)
  | ISize (Int, Int, Int)
  deriving (Show)

data IOpn
  = ICall (String, Int)
  | ICons (Int, Int)
  | IAddI
  | IMulI
  | ISubI
  | IDivI
  | INegI
  | IAddF
  | IMulF
  | ISubF
  | IDivF
  | INegF
  | ILtI
  | ILeI
  | IGtI
  | IGeI
  | IEqI
  | ILtF
  | ILeF
  | IGtF
  | IGeF
  | IEqF
  | ILtC
  | ILeC
  | IGtC
  | IGeC
  | IEqC
  | INot
  | IAnd
  | IOr
  | IFloat
  | IFloor
  | ICeil
  deriving (Show, Eq)

toTree :: IProg -> Tree String
toTree (IProg (funList, varC, arrD, stmtList)) =
  Node
    "IProg"
    ((toTreeFunList funList) ++
     [(Node ("GlobalVarCount: " ++ (show varC)) [])] ++
     (toTreeArrDims arrD) ++ (toTreeStmtList stmtList))

toTreeFunList :: [IFunBody] -> Forest String
toTreeFunList (fun:rest) =
  (Node ("FunctionBody") (toTreeFunBody fun)) : (toTreeFunList rest)
toTreeFunList [] = []

toTreeFunBody :: IFunBody -> Forest String
toTreeFunBody (IFunBody (name, localFuncs, varC, argC, arrD, bodyStmt)) =
  [(Node (show name) [])] ++
  (toTreeFunList localFuncs) ++
  [(Node ("LocalVarCount: " ++ (show varC)) [])] ++
  [(Node ("ArgCount: " ++ (show argC)) [])] ++
  (toTreeArrDims arrD) ++ (toTreeStmtList bodyStmt)

toTreeStmtList :: [IStmt] -> Forest String
toTreeStmtList (stmtList) =
  [(Node ("StatementList") (fmap toTreeStmt stmtList))]

toTreeStmt :: IStmt -> Tree String
toTreeStmt (ICond (condExp, thenStmt, elseStmt)) =
  (Node
     "IF"
     [(Node (show condExp) []), (toTreeStmt thenStmt), (toTreeStmt elseStmt)])
toTreeStmt (IBlock (funBodyList, varC, arrD, stmtList)) =
  (Node
     "Block"
     [ (Node "Local Functions" (toTreeFunList funBodyList))
     , (Node ("Local Var Count: " ++ (show varC)) [])
     , (Node "Local Arrays" (toTreeArrDims arrD))
     , (Node "Block Statements" (toTreeStmtList stmtList))
     ])
toTreeStmt (IWhile (condExpr, stmt)) =
  (Node "While" [(Node (show condExpr) []), (toTreeStmt stmt)])
toTreeStmt (stmt) = (Node (show stmt) [])

toTreeArrDims :: [(Int, [IExpr])] -> Forest String
toTreeArrDims ((dims, arrList):rest) =
  (Node ("Array: " ++ (show dims) ++ " " ++ (show arrList)) []) :
  (toTreeArrDims rest)
toTreeArrDims [] = []
