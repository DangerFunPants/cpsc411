module AstTraversal
  ( traverseProg
  ) where

-- BNFC generated modules
import           AbsM

-- Data structure definitions
import           AST

{-
================================================================================
PROGRAM INSTANTIATION
================================================================================
-}
traverseProg :: Prog -> M_prog
traverseProg (Prog_P0 block) = M_prog (traverseBlock block)

traverseBlock :: Block -> ([M_decl], [M_stmt])
traverseBlock (Block_P0 decl progBody) =
  ((traverseDeclList decl), (traverseProgBody progBody))

{-
================================================================================
TOP LEVEL DECLARATIONS
================================================================================
-}
traverseDeclList :: Declarations -> [M_decl]
traverseDeclList (Declarations_P0 declList) =
  concat $ fmap (traverseDecl) declList

traverseDecl :: Declaration -> [M_decl]
traverseDecl (Declaration_P0 varDecl)  = traverseVarDecl varDecl
traverseDecl (Declaration_P1 funDecl)  = [traverseFunDecl funDecl]
traverseDecl (Declaration_P2 dataDecl) = [traverseDataDecl dataDecl]

{-
================================================================================
VARIABLE DECLARATIONS
================================================================================
-}
traverseVarDecl :: VarDeclaration -> [M_decl]
traverseVarDecl (VarDeclaration_P0 var varSpecs _ t) =
  fmap (\f -> f t') declList
  where
    declList = traverseVarSpecs varSpecs
    t' = traverseType t

traverseVarSpecs :: VarSpecs -> [(M_type -> M_decl)]
traverseVarSpecs (VarSpecs_P0 vs moreVs) = vs' : moreVs'
  where
    vs' = traverseVarSpec vs
    moreVs' = traverseMoreVarSpecs moreVs

traverseMoreVarSpecs :: MoreVarSpecs -> [(M_type -> M_decl)]
traverseMoreVarSpecs (MoreVarSpecs_P0 vs moreVs) = vs' : moreVs'
  where
    vs' = traverseVarSpec vs
    moreVs' = traverseMoreVarSpecs moreVs
traverseMoreVarSpecs (MoreVarSpecs_P1) = []

traverseVarSpec :: VarSpec -> (M_type -> M_decl)
traverseVarSpec (VarSpec_P0 (ID id) dims) = \t -> (M_var (id, dims', t))
  where
    dims' = traverseArrayDimensions dims

{-
================================================================================
PRIMITIVE TYPES
================================================================================
-}
traverseType :: Type -> M_type
traverseType (TypeINT _)      = M_int
traverseType (TypeREAL _)     = M_real
traverseType (TypeBOOL _)     = M_bool
traverseType (TypeCHAR _)     = M_char
traverseType (TypeID (ID id)) = M_type id

{-
================================================================================
ARRAY DIMENSIONS
================================================================================
-}
traverseArrDims :: ArrayDimensions -> [M_expr]
traverseArrDims (ArrayDimensions_P0 dimList) = fmap traverseDims dimList

traverseDims :: ArrDims -> M_expr
traverseDims (ArrDims_P0 _ expr _) = traverseExpr expr

traverseBasicArrDims :: BasicArrayDimensions -> Int
traverseBasicArrDims (BasicArrayDimensions_P0 arrDims) = length arrDims

traverseArrayDimensions :: ArrayDimensions -> [M_expr]
traverseArrayDimensions (ArrayDimensions_P0 exprList) = fmap mapFn exprList
  where
    mapFn =
      \e ->
        case e of
          (ArrDims_P0 _ exp _) -> traverseExpr exp

{-
================================================================================
FUNCTION DECLARATION
================================================================================
-}
traverseFunDecl :: FunDeclaration -> M_decl
traverseFunDecl (FunDeclaration_P0 _ (ID id) paramList _ t _ fBlock _) =
  M_fun (id, paramList', t', localDecls, funStmts)
  where
    paramList' = traverseParamList paramList
    t' = traverseType t
    (localDecls, funStmts) = traverseFunBlock fBlock

traverseParamList :: ParamList -> [(String, Int, M_type)]
traverseParamList (ParamList_P0 _ params _) = traverseParams params

traverseParams :: Parameters -> [(String, Int, M_type)]
traverseParams (Parameters_P0 basicDecl morePs) = basicDecl' ++ morePs'
  where
    basicDecl' = traverseBasicDeclaration basicDecl
    morePs' = traverseMoreParams morePs
traverseParams (Parameters_P1) = []

traverseMoreParams :: MoreParameters -> [(String, Int, M_type)]
traverseMoreParams (MoreParameters_P0 basicDecl morePs) = basicDecl' ++ morePs'
  where
    basicDecl' = traverseBasicDeclaration basicDecl
    morePs' = traverseMoreParams morePs
traverseMoreParams (MoreParameters_P1) = []

traverseBasicDeclaration :: BasicDeclaration -> [(String, Int, M_type)]
traverseBasicDeclaration (BasicDeclaration_P0 varIdList _ t) =
  fmap (\f -> f t') varIdList'
  where
    varIdList' = traverseVarIdList varIdList
    t' = traverseType t

traverseVarIdList :: VarIdentList -> [(M_type -> (String, Int, M_type))]
traverseVarIdList (VarIdentList_P0 varId moreVarId) = varId' : moreVarId'
  where
    varId' = traverseVarId varId
    moreVarId' = traverseMoreVarId moreVarId

traverseMoreVarId :: MoreVarIdent -> [(M_type -> (String, Int, M_type))]
traverseMoreVarId (MoreVarIdent_P0 varId moreVarId) = varId' : moreVarId'
  where
    varId' = traverseVarId varId
    moreVarId' = traverseMoreVarId moreVarId
traverseMoreVarId (MoreVarIdent_P1) = []

traverseVarId :: VarIdent -> (M_type -> (String, Int, M_type))
traverseVarId (VarIdent_P0 (ID id) arrDims) = \t -> (id, arrDims', t)
  where
    arrDims' = traverseBasicArrDims arrDims

traverseFunBlock :: FunBlock -> ([M_decl], [M_stmt])
traverseFunBlock (FunBlock_P0 declList funBody) = (declList', funBody')
  where
    declList' = traverseDeclList declList
    funBody' = traverseFunBody funBody

traverseFunBody :: FunBody -> [M_stmt]
traverseFunBody (FunBody_P0 _ stmts _ expr _) = (stmts') ++ returnVal
  where
    stmts' = traverseProgStmts stmts
    returnVal = [(M_return (traverseExpr expr))]
traverseFunBody (FunBody_P1 stmts _ expr) = (stmts' ++ returnVal)
  where
    stmts' = traverseProgStmts stmts
    returnVal = [(M_return (traverseExpr expr))]

{-
================================================================================
DATA DECLARATION
================================================================================
-}
traverseDataDecl :: DataDeclaration -> M_decl
traverseDataDecl (DataDeclaration_P0 _ (ID id) _ consDecls) =
  M_data (id, consDecls')
  where
    consDecls' = traverseConsDecls consDecls

traverseConsDecls :: ConsDeclarations -> [(String, [M_type])]
traverseConsDecls (ConsDeclarations_P0 consDecl moreConsDecl) =
  (traverseConsDecl consDecl) : (traverseMoreConsDecl moreConsDecl)

traverseMoreConsDecl :: MoreConsDeclaration -> [(String, [M_type])]
traverseMoreConsDecl (MoreConsDeclaration_P0 _ consDecl moreConsDecl) =
  (traverseConsDecl consDecl) : (traverseMoreConsDecl moreConsDecl)
traverseMoreConsDecl (MoreConsDeclaration_P1) = []

traverseConsDecl :: ConsDeclaration -> (String, [M_type])
traverseConsDecl (ConsDeclaration_P0 (CID cid) _ typeList) = (cid, typeList')
  where
    typeList' = traverseTypeList typeList
traverseConsDecl (ConsDeclaration_P1 (CID cid)) = (cid, [])

traverseTypeList :: TypeList -> [M_type]
traverseTypeList (TypeList_P0 t moreT) =
  (traverseType t) : (traverseMoreType moreT)

traverseMoreType :: MoreType -> [M_type]
traverseMoreType (MoreType_P0 _ t moreT) =
  (traverseType t) : (traverseMoreType moreT)
traverseMoreType (MoreType_P1) = []

{-
================================================================================
BLOCKS
================================================================================
-}
traverseProgBody :: ProgramBody -> [M_stmt]
traverseProgBody (ProgramBody_P0 _ stmts _) = traverseProgStmts stmts
traverseProgBody (ProgramBody_P1 stmts)     = traverseProgStmts stmts

{-
================================================================================
STATEMENTS
================================================================================
-}
traverseProgStmts :: ProgStmts -> [M_stmt]
traverseProgStmts (ProgStmts_P0 stmts) = fmap traverseProgStmt stmts

traverseProgStmt :: ProgStmt -> M_stmt
traverseProgStmt (ProgStmt_P0 _ expr _ thenStmt _ elseStmt) =
  M_cond (expr', thenStmt', elseStmt')
  where
    expr' = traverseExpr expr
    thenStmt' = traverseProgStmt thenStmt
    elseStmt' = traverseProgStmt elseStmt
traverseProgStmt (ProgStmt_P1 _ expr _ stmt) = M_while (expr', stmt')
  where
    expr' = traverseExpr expr
    stmt' = traverseProgStmt stmt
traverseProgStmt (ProgStmt_P2 _ ident) = M_read (traverseIdentifier ident)
traverseProgStmt (ProgStmt_P3 ident _ expr) = M_ass (id', idExpr, expr')
  where
    (id', idExpr) = traverseIdentifier ident
    expr' = traverseExpr expr
traverseProgStmt (ProgStmt_P4 _ expr) = M_print (traverseExpr expr)
traverseProgStmt (ProgStmt_P5 _ block _) = M_block (traverseBlock block)
traverseProgStmt (ProgStmt_P6 _ expr _ _ caseList _) = M_case (expr', caseList')
  where
    expr' = traverseExpr expr
    caseList' = traverseCaseList caseList

traverseIdentifier :: Identifier -> (String, [M_expr])
traverseIdentifier (Identifier_P0 (ID id) arrDims) = (id, arrDims')
  where
    arrDims' = traverseArrayDimensions arrDims

{-
================================================================================
CASE STATEMENTS
================================================================================
-}
traverseCaseList :: CaseList -> [(String, [String], M_stmt)]
traverseCaseList (CaseList_P0 caseT moreCaseT) =
  (traverseCase caseT) : (traverseMoreCase moreCaseT)

traverseMoreCase :: MoreCase -> [(String, [String], M_stmt)]
traverseMoreCase (MoreCase_P0 _ caseT moreCaseT) =
  (traverseCase caseT) : (traverseMoreCase moreCaseT)
traverseMoreCase (MoreCase_P1) = []

traverseCase :: Case -> (String, [String], M_stmt)
traverseCase (Case_P0 (CID cid) varList _ stmt) = (cid, varList', stmt')
  where
    varList' = traverseVarList varList
    stmt' = traverseProgStmt stmt

traverseVarList :: VarList -> [String]
traverseVarList (VarList_P0 _ varListPrime _) = varListPrime'
  where
    varListPrime' = traverseVarListPrime varListPrime
traverseVarList (VarList_P1) = []

traverseVarListPrime :: VarListPrime -> [String]
traverseVarListPrime (VarListPrime_P0 (ID id) moreVlPrime) = id : moreVlPrime'
  where
    moreVlPrime' = traverseMoreVarListPrime moreVlPrime

traverseMoreVarListPrime :: MoreVarListPrime -> [String]
traverseMoreVarListPrime (MoreVarListPrime_P0 (ID id) moreVlPrime) =
  id : (traverseMoreVarListPrime moreVlPrime)
traverseMoreVarListPrime (MoreVarListPrime_P1) = []

{-
================================================================================
EXPRESSIONS
================================================================================
-}
traverseExpr :: Expr -> M_expr
traverseExpr (Expr_P0 exp1 _ bTerm) =
  M_app (M_or, [(traverseExpr exp1), (traverseBIntTerm bTerm)])
traverseExpr (Expr_P1 bTerm) = traverseBIntTerm bTerm

traverseBIntTerm :: BintTerm -> M_expr
traverseBIntTerm (BintTerm_P0 bTerm _ bFactor) =
  (M_app (M_and, [(traverseBIntTerm bTerm), (traverseBIntFactor bFactor)]))
traverseBIntTerm (BintTerm_P1 bFactor) = traverseBIntFactor bFactor

traverseBIntFactor :: BintFactor -> M_expr
traverseBIntFactor (BintFactor_P0 _ bFactor) =
  M_app (M_not, [traverseBIntFactor bFactor])
traverseBIntFactor (BintFactor_P1 intExpr compOp intExpr') =
  M_app (compOp', [intExprP, intExprP'])
  where
    compOp' = traverseCompOp compOp
    intExprP = traverseIntExpr intExpr
    intExprP' = traverseIntExpr intExpr'
traverseBIntFactor (BintFactor_P2 intExpr) = traverseIntExpr intExpr

traverseCompOp :: CompareOp -> M_operation
traverseCompOp (CompareOp_P0 _) = M_eq
traverseCompOp (CompareOp_P1 _) = M_lt
traverseCompOp (CompareOp_P2 _) = M_gt
traverseCompOp (CompareOp_P3 _) = M_le
traverseCompOp (CompareOp_P4 _) = M_ge

traverseIntExpr :: IntExpr -> M_expr
traverseIntExpr (IntExpr_P0 intExpr addOp intTerm) =
  M_app (addOp', [intExpr', intTerm'])
  where
    addOp' = traverseAddOp addOp
    intExpr' = traverseIntExpr intExpr
    intTerm' = traverseIntTerm intTerm
traverseIntExpr (IntExpr_P1 intTerm) = traverseIntTerm intTerm

traverseAddOp :: AddOp -> M_operation
traverseAddOp (AddOp_P0 _) = M_add
traverseAddOp (AddOp_P1 _) = M_sub

traverseIntFactor :: IntFactor -> M_expr
traverseIntFactor (IntFactor_P0 _ expr _) = traverseExpr expr
traverseIntFactor (IntFactor_P1 _ _ (ID id) arrDims _) =
  M_size (id, (traverseBasicArrDims arrDims))
traverseIntFactor (IntFactor_P2 _ _ expr _) =
  (M_app (M_float, [traverseExpr expr]))
traverseIntFactor (IntFactor_P3 _ _ expr _) =
  (M_app (M_floor, [traverseExpr expr]))
traverseIntFactor (IntFactor_P4 _ _ expr _) =
  (M_app (M_ceil, [traverseExpr expr]))
traverseIntFactor (IntFactor_P5 (ID id) modList) = traverseModList modList id
traverseIntFactor (IntFactor_P6 (IVAL val)) = M_ival $ (read val :: Int)
traverseIntFactor (IntFactor_P7 (RVAL val)) = M_rval $ (read val :: Float)
traverseIntFactor (IntFactor_P8 (BVAL val)) = M_bval $ parseBool val
  where
    parseBool v =
      case v of
        "true"  -> True
        "false" -> False
traverseIntFactor (IntFactor_P9 _ intFact) =
  M_app (M_neg, [traverseIntFactor intFact])
traverseIntFactor (IntFactor_P10 (CID id) consArgList) = M_app (op, exprList)
  where
    op = M_cid id
    exprList = traverseConsArgList consArgList
traverseIntFactor (IntFactor_P11 (CVAL val)) = M_cval $ (read val :: Char)

traverseIntTerm :: IntTerm -> M_expr
traverseIntTerm (IntTerm_P0 intTerm mulOp intFact) =
  M_app
    ( (traverseMulOp mulOp)
    , [(traverseIntTerm intTerm), (traverseIntFactor intFact)])
traverseIntTerm (IntTerm_P1 intFact) = traverseIntFactor intFact

traverseMulOp :: MulOp -> M_operation
traverseMulOp (MulOp_P0 _) = M_mul
traverseMulOp (MulOp_P1 _) = M_div

traverseModList :: ModifierList -> String -> M_expr
traverseModList (ModifierList_P0 funArgList) id =
  M_app ((M_fn id), (traverseFunArgList funArgList))
traverseModList (ModifierList_P1 dims) id =
  M_id (id, (traverseArrayDimensions dims))

traverseFunArgList :: FunArgumentList -> [M_expr]
traverseFunArgList (FunArgumentList_P0 _ args _) = traverseArgs args

traverseArgs :: Arguments -> [M_expr]
traverseArgs (Arguments_P0 argList) = fmap traverseExpr argList

traverseConsArgList :: ConsArgumentList -> [M_expr]
traverseConsArgList (ConsArgumentList_P0 funArgs) = traverseFunArgList funArgs
traverseConsArgList (ConsArgumentList_P1)         = []
