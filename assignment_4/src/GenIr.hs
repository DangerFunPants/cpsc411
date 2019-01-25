module GenIr
  ( getIr
  ) where

import           AST
import           IR
import           SymbolTable

import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (ExceptT (..), catchE, runExceptT,
                                             throwE)
import           GHC.Exts                   (groupWith)

data ErrorType
  = Plain String
  | Param (String -> ErrorType)

instance (Show) ErrorType where
  show (Plain str) = str
  show (Param f)   = show (f "")

apply :: ErrorType -> String -> ErrorType
apply (Param fn) str = (fn str)
apply (p) _          = p

{-
================================================================================
Declarations
================================================================================
-}
walkDecl :: M_decl -> ExceptT String SymbolTable [DeclWrapper]
walkDecl (M_var (name, [], varT@(M_type tName))) = do
  sym <- lookupDatatype tName
  let inSym = (Variable name varT 0)
  insertSymbol name inSym
  return []
walkDecl (M_var (name, [], varT)) = do
  let inSym = (Variable name varT 0)
  insertSymbol name inSym
  return []
walkDecl (M_var (name, arrInd, varT)) = do
  let inSym = (Variable name varT (length arrInd))
  insertSymbol name inSym
  arrExpr <- mapM walkExpr arrInd
  if (checkArrInd arrExpr)
    then do
      let retVal = ArrDecl ((length arrInd), (fmap fst arrExpr))
      return [retVal]
    else throwE "Must use integers as array indices."
walkDecl (M_data (name, typeList)) = do
  return []
walkDecl (M_fun (name, argList, retT, declList, stmtList)) = do
  (IFunction _ label _ _) <- lookupSymbol name
  addScopeLevel
  walkFuncArgs argList
  iDeclList <- walkDecls declList
  iStmtList <- walkStmts stmtList
  catchE
    (checkFunRetType retT (last stmtList))
    (\f -> throwE (show (apply f name)))
  varCount <- removeScopeLevel
  let funBody = funBodyList iDeclList
  let arrInd = arrIndList iDeclList
  let retVal =
        FunDecl
          (IFunBody
             (label, funBody, varCount, (length argList), arrInd, iStmtList))
  return [retVal]
  where
    checkFunRetType retT (M_return expr) = do
      (iExpr, exprT) <- catchE (walkExpr expr) (\s -> (throwE (Plain s)))
      if exprT == retT
        then return ()
        else throwE $
             (Param
                (\s ->
                   Plain
                     ("Expected function " ++
                      (s) ++
                      " to return " ++
                      (show retT) ++ " but it is returning " ++ (show exprT))))

walkDecls :: [M_decl] -> ExceptT String SymbolTable [DeclWrapper]
walkDecls declList@(_:_) = do
  let sorted = (concat . (groupWith mapFn)) declList
  preProcFuncs (filter isFunc declList)
  preProcDataTypes (filter isData declList)
  iDeclList <- walkDecls' sorted
  return iDeclList
  where
    mapFn (M_var _)  = 0
    mapFn (M_data _) = 1
    mapFn (M_fun _)  = 2
    walkDecls' (first:rest) = do
      iDecl <- walkDecl first
      iDeclList <- walkDecls' rest
      return (iDecl ++ iDeclList)
    walkDecls' [] = return []
    preProcFuncs ((M_fun (name, argList, retT, _, _)):rest) = do
      label <- nextLabel
      let inSym = (Function label (fmap remName argList) retT)
      insertSymbol name inSym
      preProcFuncs rest
      return ()
    preProcFuncs [] = return ()
    remName (_, dims, argT) = (argT, dims)
    isFunc (M_fun _) = True
    isFunc _         = False
    isData (M_data _) = True
    isData _          = False
    preProcDataTypes ((M_data (name, argList)):rest) = do
      let inSym = (Datatype name)
      insertSymbol name inSym
      insertCons argList name
      preProcDataTypes rest
      return ()
    preProcDataTypes [] = return ()
    insertCons ((consName, paramTs):rest) tName = do
      let inSym = Constructor consName paramTs tName
      insertSymbol consName inSym
      insertCons rest tName
      return ()
    insertCons [] _ = return ()
walkDecls [] = return []

{-
================================================================================
Expressions
================================================================================

Some strange stuff happens here when pattern matching against constructors for
operations that are nested within the constructors of the M_expr type, namely
the M_app constructor. It seems that the ordering of the functions has an effect
on the ability of the GHC to compile the source. More investigation required.
-}
walkExpr :: M_expr -> ExceptT String SymbolTable (IExpr, M_type)
walkExpr (M_ival intVal) = do
  return ((IIVal intVal), M_int)
walkExpr (M_rval realVal) = do
  return ((IRVal realVal), M_real)
walkExpr (M_bval boolVal) = do
  return ((IBVal boolVal), M_bool)
walkExpr (M_cval charVal) = do
  return ((ICVal charVal), M_char)
walkExpr (M_app ((M_fn fnName), exprList)) = do
  eList <- mapM walkExpr exprList
  (IFunction lvl ident argList retType) <- lookupSymbol fnName
  let tc = typeCheckFunArgs argList eList
  arrInds <- checkArrDims exprList (fmap fst eList) argList
  if (tc && arrInds)
    then do
      let opT = ICall (ident, lvl)
      let ret = (IApp (opT, (fmap fst eList)), retType)
      return ret
    else throwE "Invalid function arguments passed"
  where
    checkArrDims ((M_id (name, deref)):restAst) ((IId (_, _, dims)):restExpr) ((argT, argDim):restArgs) = do
      (IVariable _ _ varT card) <- lookupSymbol name
      let derefCardinality = card - (length dims)
      if derefCardinality == argDim
        then (checkArrDims restAst restExpr restArgs)
        else return False
    checkArrDims _ _ _ = return True
    typeCheckFunArgs ((tDec, dim):restDec) ((expr, tPass):restPass)
      | tDec == tPass = typeCheckFunArgs restDec restPass
      | otherwise = False
    typeCheckFunArgs [] [] = True
    typeCheckFunArgs [] _ = False
    typeCheckFunArgs _ [] = False
walkExpr (M_app ((M_cid cName), exprList)) = do
  eList <- mapM walkExpr exprList
  (IConstructor num typeList consType) <- lookupSymbol cName
  let fstExprList = fmap fst eList
  if (typeCheckConsArgs eList typeList)
    then return $
         ( IApp ((ICons (num, (length typeList))), fstExprList)
         , (M_type consType))
    else throwE "Invalid parameters passed to type constructor."
  where
    typeCheckConsArgs ((eList, tPass):restDec) (t:restTypes)
      | tPass == t = typeCheckConsArgs restDec restTypes
      | otherwise = False
    typeCheckConsArgs [] [] = True
    typeCheckConsArgs [] _ = False
    typeCheckConsArgs _ [] = False
walkExpr (M_app (opType, exprList)) = do
  eList <- mapM walkExpr exprList
  expr <- application opType eList
  return expr
walkExpr id@(M_id (ident, exprs)) = do
  (IVariable level offset varT dims) <- lookupSymbol ident
  iExprs <- mapM walkExpr exprs
  if ((checkArrInd iExprs) && (length iExprs) <= dims)
    then do
      let fstExp = fmap fst iExprs
      return $ ((IId (level, offset, fstExp)), varT)
    else throwE ("Invalid array expression: " ++ (show id))
walkExpr (M_size (ident, cardinality)) = do
  (IVariable level offset varT dims) <- lookupSymbol ident
  if cardinality >= (dims)
    then throwE "Invalid array size operation"
    else return $ ((ISize (level, offset, dims)), M_int)

application ::
     M_operation
  -> [(IExpr, M_type)]
  -> ExceptT String SymbolTable (IExpr, M_type)
application opType eList = do
  (opn, t) <- getTypedOp' eList opType
  return (IApp (opn, (fmap fst eList)), t)
  where
    getTypedOp' eList opType =
      case eList of
        ((_, M_int):(_, M_int):rest) -> (validType M_int opType)
        ((_, M_real):(_, M_real):rest) -> (validType M_real opType)
        ((_, M_bool):(_, M_bool):rest) -> (validType M_bool opType)
        ((_, M_bool):rest) -> (validType M_bool opType)
        ((_, M_int):rest) -> (validType M_int opType)
        otherwise -> throwE "Invalid Types in binary operation"

validType :: M_type -> M_operation -> ExceptT String SymbolTable (IOpn, M_type)
validType (M_int) = validIntOp
validType (M_real) = validRealOp
validType (M_bool) = validBoolOp
validType (M_char) = validCharOp
validType (M_type _) =
  \s -> throwE ("Binary and unary operators not valid for user types.")

validIntOp (M_add) = return (IAddI, M_int)
validIntOp (M_sub) = return (ISubI, M_int)
validIntOp (M_mul) = return (IMulI, M_int)
validIntOp (M_div) = return (IDivI, M_int)
validIntOp (M_lt) = return (ILtI, M_bool)
validIntOp (M_gt) = return (IGtI, M_bool)
validIntOp (M_le) = return (ILeI, M_bool)
validIntOp (M_ge) = return (IGeI, M_bool)
validIntOp (M_eq) = return (IEqI, M_bool)
validIntOp (M_float) = return (IFloat, M_real)
validIntOp (op) =
  throwE ("Operation: " ++ (show op) ++ " not defined on reals.")

validRealOp (M_add) = return (IAddF, M_real)
validRealOp (M_sub) = return (ISubF, M_real)
validRealOp (M_mul) = return (IMulF, M_real)
validRealOp (M_div) = return (IDivF, M_real)
validRealOp (M_lt) = return (ILtF, M_bool)
validRealOp (M_gt) = return (IGtF, M_bool)
validRealOp (M_le) = return (ILeF, M_bool)
validRealOp (M_ge) = return (IGeF, M_bool)
validRealOp (M_eq) = return (IEqF, M_bool)
validRealOp (M_floor) = return (IFloor, M_real)
validRealOp (M_ceil) = return (ICeil, M_real)
validRealOp (op) =
  throwE ("operation: " ++ (show op) ++ " not defined on integers.")

validBoolOp (M_not) = return (INot, M_bool)
validBoolOp (M_or) = return (IOr, M_bool)
validBoolOp (M_and) = return (IAnd, M_bool)
validBoolOp (op) =
  throwE ("Operation: " ++ (show op) ++ " not defined on booleans.")

validCharOp (M_lt) = return (ILtC, M_bool)
validCharOp (M_gt) = return (IGtC, M_bool)
validCharOp (M_le) = return (ILeC, M_bool)
validCharOp (M_ge) = return (IGeC, M_bool)
validCharOp (M_eq) = return (IEqC, M_bool)
validCharOp (op) =
  throwE ("Operation: " ++ (show op) ++ " not defined on characters.")

{-
================================================================================
Statements
================================================================================
-}
walkStmt :: M_stmt -> ExceptT String SymbolTable IStmt
walkStmt (M_ass (name, indExpr, expr)) = do
  (IVariable level offset varT dims) <- lookupSymbol name
  (rhsExpr, t) <- walkExpr expr
  if (t == varT) && (dims == (length indExpr))
    then return $ IAssign (level, offset, [], rhsExpr)
    else throwE "Invalid Types in Assign Statement"
walkStmt (M_while (condExpr, bodyStmts)) = do
  (iCond, condT) <- walkExpr condExpr
  if condT == M_bool
    then do
      iStmts <- walkStmt bodyStmts
      let ret = IWhile (iCond, iStmts)
      return ret
    else throwE "Loop predicate must have boolean type."
walkStmt (M_cond (condExpr, thenBody, elseBody)) = do
  (iCond, condT) <- walkExpr condExpr
  if condT == M_bool
    then do
      iThenStmts <- walkStmt thenBody
      iElseStmts <- walkStmt elseBody
      let ret = ICond (iCond, iThenStmts, iElseStmts)
      return ret
    else throwE "Conditional predicate must have boolean type."
walkStmt (M_read (name, indExpr)) = do
  (IVariable level offset varT dims) <- lookupSymbol name
  iExprList <- mapM walkExpr indExpr
  let fstExprList = fmap fst iExprList
  cons <- readCons varT -- (level, offset, fstExprList)
  let ret = cons (level, offset, fstExprList)
  if dims == (length indExpr)
    then return ret
    else throwE "Cannot read in arrays."
  where
    readCons t
      | t == M_int = return IReadI
      | t == M_bool = return IReadB
      | t == M_real = return IReadF
      | t == M_char = return IReadC
      | otherwise = throwE "Cannot read in user defined types."
-- TODO: Pattern match against ID constructor to check array dimensions.
walkStmt (M_print expr) = do
  (iExpr, exprT) <- walkExpr expr
  cons <- printCons exprT
  let ret = cons iExpr
  case expr of
    (M_id (name, arrInd)) -> do
      (IVariable _ _ _ card) <- lookupSymbol name
      if (card /= (length arrInd))
        then throwE "Cannot print arrays."
        else return ret
    _ -> return ret
  where
    printCons M_int  = return IPrintI
    printCons M_real = return IPrintF
    printCons M_bool = return IPrintB
    printCons M_char = return IPrintC
    printCons _      = throwE "Cannot print user defined types."
walkStmt (M_return expr) = do
  (iExpr, exprT) <- walkExpr expr
  return (IReturn iExpr)
walkStmt (M_block (declList, stmtList)) = do
  addScopeLevel
  declList <- walkDecls declList
  stmtList <- walkStmts stmtList
  varCount <- removeScopeLevel
  let funBody = funBodyList declList
  let arrInd = arrIndList declList
  let ret = IBlock (funBody, varCount, arrInd, stmtList)
  return ret
walkStmt (M_case (expr, caseList)) = do
  addScopeLevel
  (caseExpr, exprT) <- walkExpr expr
  case exprT of
    (M_type _) -> do
      casesList <- mapM (walkCase exprT) caseList
      removeScopeLevel
      return $ ICase (caseExpr, casesList)
    _ -> throwE "Cannot case on primitive types."

walkCase ::
     M_type
  -> (String, [String], M_stmt)
  -> ExceptT String SymbolTable (Int, Int, IStmt)
walkCase (M_type tName) (cName, argList, stmt) = do
  (IConstructor num typeList consT) <- lookupSymbol cName
  if (length argList) /= (length typeList) || (tName /= consT)
    then throwE "Invalid pattern match in case construct."
    else do
      addScopeLevel
      mapM insVar (zip argList typeList)
      iStmt <- walkStmt stmt
      removeScopeLevel
      return (num, (length typeList), iStmt)
  where
    insVar (ident, varT) = do
      let inSym = Variable ident varT 0
      insertSymbol ident inSym

walkStmts :: [M_stmt] -> ExceptT String SymbolTable [IStmt]
walkStmts (stmt:rest) = do
  iStmt <- walkStmt stmt
  iStmtList <- walkStmts rest
  return (iStmt : iStmtList)
walkStmts [] = return []

{-
================================================================================
Helper Functions
================================================================================
-}
walkFuncArgs :: [(String, Int, M_type)] -> ExceptT String SymbolTable ()
walkFuncArgs ((name, dims, argT):rest) = do
  let inSym = Argument name argT dims
  insertSymbol name inSym
  walkFuncArgs rest
  return ()
walkFuncArgs [] = return ()

funBodyList :: [DeclWrapper] -> [IFunBody]
funBodyList wrapList = (((map stripFunDecl) . (filter isFunDecl)) wrapList)
  where
    isFunDecl (FunDecl _) = True
    isFunDecl decl        = False
    stripFunDecl (FunDecl decl) = decl

arrIndList :: [DeclWrapper] -> [(Int, [IExpr])]
arrIndList wrapList = (((map stripArrDecl) . (filter isArrDecl)) wrapList)
  where
    isArrDecl (ArrDecl _) = True
    isArrDecl decl        = False
    stripArrDecl (ArrDecl decl) = decl

-- Just checks the for the type of the indices, not that the number
-- of indices used is valid.
checkArrInd arrExprList =
  case (length (filter isNotInt arrExprList)) of
    0         -> True
    otherwise -> False
  where
    isNotInt (_, t) = t /= M_int

{-
================================================================================
Root Function
================================================================================
-}
walkAst' :: M_prog -> ExceptT String SymbolTable IProg
walkAst' (M_prog (decls, stmts)) = do
  addScopeLevel
  iDeclList <- walkDecls decls
  iStmtList <- walkStmts stmts
  varCount <- removeScopeLevel
  let funDecl = funBodyList iDeclList
  let arrDecl = arrIndList iDeclList
  return (IProg (funDecl, varCount, arrDecl, iStmtList))

walkAst :: M_prog -> SymbolTable (Either String IProg)
walkAst prog = do
  let stateFun = runExceptT (walkAst' prog)
  stateFun

getIr :: M_prog -> Either String IProg
getIr ast = eProg
  where
    (eProg, _) = ((runState (walkAst ast)) (0, []))
