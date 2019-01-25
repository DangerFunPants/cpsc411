module SkelM where

-- Haskell module generated by the BNF converter

import AbsM
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transVAR :: VAR -> Result
transVAR x = case x of
  VAR string -> failure x
transCOLON :: COLON -> Result
transCOLON x = case x of
  COLON string -> failure x
transINT :: INT -> Result
transINT x = case x of
  INT string -> failure x
transREAL :: REAL -> Result
transREAL x = case x of
  REAL string -> failure x
transBOOL :: BOOL -> Result
transBOOL x = case x of
  BOOL string -> failure x
transCHAR :: CHAR -> Result
transCHAR x = case x of
  CHAR string -> failure x
transSLPAR :: SLPAR -> Result
transSLPAR x = case x of
  SLPAR string -> failure x
transSRPAR :: SRPAR -> Result
transSRPAR x = case x of
  SRPAR string -> failure x
transFUN :: FUN -> Result
transFUN x = case x of
  FUN string -> failure x
transLPAR :: LPAR -> Result
transLPAR x = case x of
  LPAR string -> failure x
transRPAR :: RPAR -> Result
transRPAR x = case x of
  RPAR string -> failure x
transBEGIN :: BEGIN -> Result
transBEGIN x = case x of
  BEGIN string -> failure x
transEND :: END -> Result
transEND x = case x of
  END string -> failure x
transRETURN :: RETURN -> Result
transRETURN x = case x of
  RETURN string -> failure x
transIF :: IF -> Result
transIF x = case x of
  IF string -> failure x
transTHEN :: THEN -> Result
transTHEN x = case x of
  THEN string -> failure x
transELSE :: ELSE -> Result
transELSE x = case x of
  ELSE string -> failure x
transWHILE :: WHILE -> Result
transWHILE x = case x of
  WHILE string -> failure x
transREAD :: READ -> Result
transREAD x = case x of
  READ string -> failure x
transASSIGN :: ASSIGN -> Result
transASSIGN x = case x of
  ASSIGN string -> failure x
transPRINT :: PRINT -> Result
transPRINT x = case x of
  PRINT string -> failure x
transDO :: DO -> Result
transDO x = case x of
  DO string -> failure x
transOR :: OR -> Result
transOR x = case x of
  OR string -> failure x
transAND :: AND -> Result
transAND x = case x of
  AND string -> failure x
transNOT :: NOT -> Result
transNOT x = case x of
  NOT string -> failure x
transEQUAL :: EQUAL -> Result
transEQUAL x = case x of
  EQUAL string -> failure x
transLESST :: LESST -> Result
transLESST x = case x of
  LESST string -> failure x
transGREATERT :: GREATERT -> Result
transGREATERT x = case x of
  GREATERT string -> failure x
transLE :: LE -> Result
transLE x = case x of
  LE string -> failure x
transGE :: GE -> Result
transGE x = case x of
  GE string -> failure x
transSUB :: SUB -> Result
transSUB x = case x of
  SUB string -> failure x
transADD :: ADD -> Result
transADD x = case x of
  ADD string -> failure x
transMUL :: MUL -> Result
transMUL x = case x of
  MUL string -> failure x
transDIV :: DIV -> Result
transDIV x = case x of
  DIV string -> failure x
transCLPAR :: CLPAR -> Result
transCLPAR x = case x of
  CLPAR string -> failure x
transCRPAR :: CRPAR -> Result
transCRPAR x = case x of
  CRPAR string -> failure x
transSIZE :: SIZE -> Result
transSIZE x = case x of
  SIZE string -> failure x
transFLOOR :: FLOOR -> Result
transFLOOR x = case x of
  FLOOR string -> failure x
transCEIL :: CEIL -> Result
transCEIL x = case x of
  CEIL string -> failure x
transFLOAT :: FLOAT -> Result
transFLOAT x = case x of
  FLOAT string -> failure x
transDATA :: DATA -> Result
transDATA x = case x of
  DATA string -> failure x
transOF :: OF -> Result
transOF x = case x of
  OF string -> failure x
transCASE :: CASE -> Result
transCASE x = case x of
  CASE string -> failure x
transARROW :: ARROW -> Result
transARROW x = case x of
  ARROW string -> failure x
transSLASH :: SLASH -> Result
transSLASH x = case x of
  SLASH string -> failure x
transIVAL :: IVAL -> Result
transIVAL x = case x of
  IVAL string -> failure x
transRVAL :: RVAL -> Result
transRVAL x = case x of
  RVAL string -> failure x
transBVAL :: BVAL -> Result
transBVAL x = case x of
  BVAL string -> failure x
transCVAL :: CVAL -> Result
transCVAL x = case x of
  CVAL string -> failure x
transCID :: CID -> Result
transCID x = case x of
  CID string -> failure x
transID :: ID -> Result
transID x = case x of
  ID string -> failure x
transProg :: Prog -> Result
transProg x = case x of
  Prog_P0 block -> failure x
transBlock :: Block -> Result
transBlock x = case x of
  Block_P0 declarations programbody -> failure x
transDeclarations :: Declarations -> Result
transDeclarations x = case x of
  Declarations_P0 declarations -> failure x
transDeclaration :: Declaration -> Result
transDeclaration x = case x of
  Declaration_P0 vardeclaration -> failure x
  Declaration_P1 fundeclaration -> failure x
  Declaration_P2 datadeclaration -> failure x
transVarDeclaration :: VarDeclaration -> Result
transVarDeclaration x = case x of
  VarDeclaration_P0 var varspecs colon type_ -> failure x
transVarSpecs :: VarSpecs -> Result
transVarSpecs x = case x of
  VarSpecs_P0 varspec morevarspecs -> failure x
transMoreVarSpecs :: MoreVarSpecs -> Result
transMoreVarSpecs x = case x of
  MoreVarSpecs_P0 varspec morevarspecs -> failure x
  MoreVarSpecs_P1 -> failure x
transVarSpec :: VarSpec -> Result
transVarSpec x = case x of
  VarSpec_P0 id arraydimensions -> failure x
transType :: Type -> Result
transType x = case x of
  TypeINT int -> failure x
  TypeREAL real -> failure x
  TypeBOOL bool -> failure x
  TypeCHAR char -> failure x
  TypeID id -> failure x
transArrayDimensions :: ArrayDimensions -> Result
transArrayDimensions x = case x of
  ArrayDimensions_P0 arrdimss -> failure x
transArrDims :: ArrDims -> Result
transArrDims x = case x of
  ArrDims_P0 slpar expr srpar -> failure x
transBasicArrayDimensions :: BasicArrayDimensions -> Result
transBasicArrayDimensions x = case x of
  BasicArrayDimensions_P0 emptyarrdimss -> failure x
transEmptyArrDims :: EmptyArrDims -> Result
transEmptyArrDims x = case x of
  EmptyArrDims_P0 slpar srpar -> failure x
transFunDeclaration :: FunDeclaration -> Result
transFunDeclaration x = case x of
  FunDeclaration_P0 fun id paramlist colon type_ clpar funblock crpar -> failure x
transFunBlock :: FunBlock -> Result
transFunBlock x = case x of
  FunBlock_P0 declarations funbody -> failure x
transFunBody :: FunBody -> Result
transFunBody x = case x of
  FunBody_P0 begin progstmts return expr end -> failure x
  FunBody_P1 progstmts return expr -> failure x
transParamList :: ParamList -> Result
transParamList x = case x of
  ParamList_P0 lpar parameters rpar -> failure x
transParameters :: Parameters -> Result
transParameters x = case x of
  Parameters_P0 basicdeclaration moreparameters -> failure x
  Parameters_P1 -> failure x
transMoreParameters :: MoreParameters -> Result
transMoreParameters x = case x of
  MoreParameters_P0 basicdeclaration moreparameters -> failure x
  MoreParameters_P1 -> failure x
transBasicDeclaration :: BasicDeclaration -> Result
transBasicDeclaration x = case x of
  BasicDeclaration_P0 varidentlist colon type_ -> failure x
transVarIdentList :: VarIdentList -> Result
transVarIdentList x = case x of
  VarIdentList_P0 varident morevarident -> failure x
transVarIdent :: VarIdent -> Result
transVarIdent x = case x of
  VarIdent_P0 id basicarraydimensions -> failure x
transMoreVarIdent :: MoreVarIdent -> Result
transMoreVarIdent x = case x of
  MoreVarIdent_P0 varident morevarident -> failure x
  MoreVarIdent_P1 -> failure x
transDataDeclaration :: DataDeclaration -> Result
transDataDeclaration x = case x of
  DataDeclaration_P0 data_ id equal consdeclarations -> failure x
transConsDeclarations :: ConsDeclarations -> Result
transConsDeclarations x = case x of
  ConsDeclarations_P0 consdeclaration moreconsdeclaration -> failure x
transMoreConsDeclaration :: MoreConsDeclaration -> Result
transMoreConsDeclaration x = case x of
  MoreConsDeclaration_P0 slash consdeclaration moreconsdeclaration -> failure x
  MoreConsDeclaration_P1 -> failure x
transConsDeclaration :: ConsDeclaration -> Result
transConsDeclaration x = case x of
  ConsDeclaration_P0 cid of_ typelist -> failure x
  ConsDeclaration_P1 cid -> failure x
transTypeList :: TypeList -> Result
transTypeList x = case x of
  TypeList_P0 type_ moretype -> failure x
transMoreType :: MoreType -> Result
transMoreType x = case x of
  MoreType_P0 mul type_ moretype -> failure x
  MoreType_P1 -> failure x
transProgramBody :: ProgramBody -> Result
transProgramBody x = case x of
  ProgramBody_P0 begin progstmts end -> failure x
  ProgramBody_P1 progstmts -> failure x
transProgStmts :: ProgStmts -> Result
transProgStmts x = case x of
  ProgStmts_P0 progstmts -> failure x
transProgStmt :: ProgStmt -> Result
transProgStmt x = case x of
  ProgStmt_P0 if_ expr then_ progstmt1 else_ progstmt2 -> failure x
  ProgStmt_P1 while expr do_ progstmt -> failure x
  ProgStmt_P2 read identifier -> failure x
  ProgStmt_P3 identifier assign expr -> failure x
  ProgStmt_P4 print expr -> failure x
  ProgStmt_P5 clpar block crpar -> failure x
  ProgStmt_P6 case_ expr of_ clpar caselist crpar -> failure x
transIdentifier :: Identifier -> Result
transIdentifier x = case x of
  Identifier_P0 id arraydimensions -> failure x
transCaseList :: CaseList -> Result
transCaseList x = case x of
  CaseList_P0 case_ morecase -> failure x
transMoreCase :: MoreCase -> Result
transMoreCase x = case x of
  MoreCase_P0 slash case_ morecase -> failure x
  MoreCase_P1 -> failure x
transCase :: Case -> Result
transCase x = case x of
  Case_P0 cid varlist arrow progstmt -> failure x
transVarList :: VarList -> Result
transVarList x = case x of
  VarList_P0 lpar varlistprime rpar -> failure x
  VarList_P1 -> failure x
transVarListPrime :: VarListPrime -> Result
transVarListPrime x = case x of
  VarListPrime_P0 id morevarlistprime -> failure x
transMoreVarListPrime :: MoreVarListPrime -> Result
transMoreVarListPrime x = case x of
  MoreVarListPrime_P0 id morevarlistprime -> failure x
  MoreVarListPrime_P1 -> failure x
transExpr :: Expr -> Result
transExpr x = case x of
  Expr_P0 expr or bintterm -> failure x
  Expr_P1 bintterm -> failure x
transBintTerm :: BintTerm -> Result
transBintTerm x = case x of
  BintTerm_P0 bintterm and bintfactor -> failure x
  BintTerm_P1 bintfactor -> failure x
transBintFactor :: BintFactor -> Result
transBintFactor x = case x of
  BintFactor_P0 not bintfactor -> failure x
  BintFactor_P1 intexpr1 compareop intexpr2 -> failure x
  BintFactor_P2 intexpr -> failure x
transCompareOp :: CompareOp -> Result
transCompareOp x = case x of
  CompareOp_P0 equal -> failure x
  CompareOp_P1 lesst -> failure x
  CompareOp_P2 greatert -> failure x
  CompareOp_P3 le -> failure x
  CompareOp_P4 ge -> failure x
transIntExpr :: IntExpr -> Result
transIntExpr x = case x of
  IntExpr_P0 intexpr addop intterm -> failure x
  IntExpr_P1 intterm -> failure x
transAddOp :: AddOp -> Result
transAddOp x = case x of
  AddOp_P0 add -> failure x
  AddOp_P1 sub -> failure x
transIntTerm :: IntTerm -> Result
transIntTerm x = case x of
  IntTerm_P0 intterm mulop intfactor -> failure x
  IntTerm_P1 intfactor -> failure x
transMulOp :: MulOp -> Result
transMulOp x = case x of
  MulOp_P0 mul -> failure x
  MulOp_P1 div -> failure x
transIntFactor :: IntFactor -> Result
transIntFactor x = case x of
  IntFactor_P0 lpar expr rpar -> failure x
  IntFactor_P1 size lpar id basicarraydimensions rpar -> failure x
  IntFactor_P2 float lpar expr rpar -> failure x
  IntFactor_P3 floor lpar expr rpar -> failure x
  IntFactor_P4 ceil lpar expr rpar -> failure x
  IntFactor_P5 id modifierlist -> failure x
  IntFactor_P6 ival -> failure x
  IntFactor_P7 rval -> failure x
  IntFactor_P8 bval -> failure x
  IntFactor_P9 sub intfactor -> failure x
  IntFactor_P10 cid consargumentlist -> failure x
  IntFactor_P11 cval -> failure x
transModifierList :: ModifierList -> Result
transModifierList x = case x of
  ModifierList_P0 funargumentlist -> failure x
  ModifierList_P1 arraydimensions -> failure x
transFunArgumentList :: FunArgumentList -> Result
transFunArgumentList x = case x of
  FunArgumentList_P0 lpar arguments rpar -> failure x
transArguments :: Arguments -> Result
transArguments x = case x of
  Arguments_P0 exprs -> failure x
transConsArgumentList :: ConsArgumentList -> Result
transConsArgumentList x = case x of
  ConsArgumentList_P0 funargumentlist -> failure x
  ConsArgumentList_P1 -> failure x

