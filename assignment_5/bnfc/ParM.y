-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParM where
import AbsM
import LexM
import ErrM

}

%name pProg Prog
%name pBlock Block
%name pDeclarations Declarations
%name pListDeclaration ListDeclaration
%name pDeclaration Declaration
%name pVarDeclaration VarDeclaration
%name pVarSpecs VarSpecs
%name pMoreVarSpecs MoreVarSpecs
%name pVarSpec VarSpec
%name pType Type
%name pArrayDimensions ArrayDimensions
%name pArrDims ArrDims
%name pListArrDims ListArrDims
%name pBasicArrayDimensions BasicArrayDimensions
%name pEmptyArrDims EmptyArrDims
%name pListEmptyArrDims ListEmptyArrDims
%name pFunDeclaration FunDeclaration
%name pFunBlock FunBlock
%name pFunBody FunBody
%name pParamList ParamList
%name pParameters Parameters
%name pMoreParameters MoreParameters
%name pBasicDeclaration BasicDeclaration
%name pVarIdentList VarIdentList
%name pVarIdent VarIdent
%name pMoreVarIdent MoreVarIdent
%name pDataDeclaration DataDeclaration
%name pConsDeclarations ConsDeclarations
%name pMoreConsDeclaration MoreConsDeclaration
%name pConsDeclaration ConsDeclaration
%name pTypeList TypeList
%name pMoreType MoreType
%name pProgramBody ProgramBody
%name pProgStmts ProgStmts
%name pListProgStmt ListProgStmt
%name pProgStmt ProgStmt
%name pIdentifier Identifier
%name pCaseList CaseList
%name pMoreCase MoreCase
%name pCase Case
%name pVarList VarList
%name pVarListPrime VarListPrime
%name pMoreVarListPrime MoreVarListPrime
%name pExpr Expr
%name pBintTerm BintTerm
%name pBintFactor BintFactor
%name pCompareOp CompareOp
%name pIntExpr IntExpr
%name pAddOp AddOp
%name pIntTerm IntTerm
%name pMulOp MulOp
%name pIntFactor IntFactor
%name pModifierList ModifierList
%name pFunArgumentList FunArgumentList
%name pArguments Arguments
%name pListExpr ListExpr
%name pConsArgumentList ConsArgumentList
-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype {Token}
%token
  ',' { PT _ (TS _ 1) }
  ';' { PT _ (TS _ 2) }

L_VAR { PT _ (T_VAR $$) }
L_COLON { PT _ (T_COLON $$) }
L_INT { PT _ (T_INT $$) }
L_REAL { PT _ (T_REAL $$) }
L_BOOL { PT _ (T_BOOL $$) }
L_CHAR { PT _ (T_CHAR $$) }
L_SLPAR { PT _ (T_SLPAR $$) }
L_SRPAR { PT _ (T_SRPAR $$) }
L_FUN { PT _ (T_FUN $$) }
L_LPAR { PT _ (T_LPAR $$) }
L_RPAR { PT _ (T_RPAR $$) }
L_BEGIN { PT _ (T_BEGIN $$) }
L_END { PT _ (T_END $$) }
L_RETURN { PT _ (T_RETURN $$) }
L_IF { PT _ (T_IF $$) }
L_THEN { PT _ (T_THEN $$) }
L_ELSE { PT _ (T_ELSE $$) }
L_WHILE { PT _ (T_WHILE $$) }
L_READ { PT _ (T_READ $$) }
L_ASSIGN { PT _ (T_ASSIGN $$) }
L_PRINT { PT _ (T_PRINT $$) }
L_DO { PT _ (T_DO $$) }
L_OR { PT _ (T_OR $$) }
L_AND { PT _ (T_AND $$) }
L_NOT { PT _ (T_NOT $$) }
L_EQUAL { PT _ (T_EQUAL $$) }
L_LESST { PT _ (T_LESST $$) }
L_GREATERT { PT _ (T_GREATERT $$) }
L_LE { PT _ (T_LE $$) }
L_GE { PT _ (T_GE $$) }
L_SUB { PT _ (T_SUB $$) }
L_ADD { PT _ (T_ADD $$) }
L_MUL { PT _ (T_MUL $$) }
L_DIV { PT _ (T_DIV $$) }
L_CLPAR { PT _ (T_CLPAR $$) }
L_CRPAR { PT _ (T_CRPAR $$) }
L_SIZE { PT _ (T_SIZE $$) }
L_FLOOR { PT _ (T_FLOOR $$) }
L_CEIL { PT _ (T_CEIL $$) }
L_FLOAT { PT _ (T_FLOAT $$) }
L_DATA { PT _ (T_DATA $$) }
L_OF { PT _ (T_OF $$) }
L_CASE { PT _ (T_CASE $$) }
L_ARROW { PT _ (T_ARROW $$) }
L_SLASH { PT _ (T_SLASH $$) }
L_IVAL { PT _ (T_IVAL $$) }
L_RVAL { PT _ (T_RVAL $$) }
L_BVAL { PT _ (T_BVAL $$) }
L_CVAL { PT _ (T_CVAL $$) }
L_CID { PT _ (T_CID $$) }
L_ID { PT _ (T_ID $$) }


%%

VAR    :: { VAR} : L_VAR { VAR ($1)}
COLON    :: { COLON} : L_COLON { COLON ($1)}
INT    :: { INT} : L_INT { INT ($1)}
REAL    :: { REAL} : L_REAL { REAL ($1)}
BOOL    :: { BOOL} : L_BOOL { BOOL ($1)}
CHAR    :: { CHAR} : L_CHAR { CHAR ($1)}
SLPAR    :: { SLPAR} : L_SLPAR { SLPAR ($1)}
SRPAR    :: { SRPAR} : L_SRPAR { SRPAR ($1)}
FUN    :: { FUN} : L_FUN { FUN ($1)}
LPAR    :: { LPAR} : L_LPAR { LPAR ($1)}
RPAR    :: { RPAR} : L_RPAR { RPAR ($1)}
BEGIN    :: { BEGIN} : L_BEGIN { BEGIN ($1)}
END    :: { END} : L_END { END ($1)}
RETURN    :: { RETURN} : L_RETURN { RETURN ($1)}
IF    :: { IF} : L_IF { IF ($1)}
THEN    :: { THEN} : L_THEN { THEN ($1)}
ELSE    :: { ELSE} : L_ELSE { ELSE ($1)}
WHILE    :: { WHILE} : L_WHILE { WHILE ($1)}
READ    :: { READ} : L_READ { READ ($1)}
ASSIGN    :: { ASSIGN} : L_ASSIGN { ASSIGN ($1)}
PRINT    :: { PRINT} : L_PRINT { PRINT ($1)}
DO    :: { DO} : L_DO { DO ($1)}
OR    :: { OR} : L_OR { OR ($1)}
AND    :: { AND} : L_AND { AND ($1)}
NOT    :: { NOT} : L_NOT { NOT ($1)}
EQUAL    :: { EQUAL} : L_EQUAL { EQUAL ($1)}
LESST    :: { LESST} : L_LESST { LESST ($1)}
GREATERT    :: { GREATERT} : L_GREATERT { GREATERT ($1)}
LE    :: { LE} : L_LE { LE ($1)}
GE    :: { GE} : L_GE { GE ($1)}
SUB    :: { SUB} : L_SUB { SUB ($1)}
ADD    :: { ADD} : L_ADD { ADD ($1)}
MUL    :: { MUL} : L_MUL { MUL ($1)}
DIV    :: { DIV} : L_DIV { DIV ($1)}
CLPAR    :: { CLPAR} : L_CLPAR { CLPAR ($1)}
CRPAR    :: { CRPAR} : L_CRPAR { CRPAR ($1)}
SIZE    :: { SIZE} : L_SIZE { SIZE ($1)}
FLOOR    :: { FLOOR} : L_FLOOR { FLOOR ($1)}
CEIL    :: { CEIL} : L_CEIL { CEIL ($1)}
FLOAT    :: { FLOAT} : L_FLOAT { FLOAT ($1)}
DATA    :: { DATA} : L_DATA { DATA ($1)}
OF    :: { OF} : L_OF { OF ($1)}
CASE    :: { CASE} : L_CASE { CASE ($1)}
ARROW    :: { ARROW} : L_ARROW { ARROW ($1)}
SLASH    :: { SLASH} : L_SLASH { SLASH ($1)}
IVAL    :: { IVAL} : L_IVAL { IVAL ($1)}
RVAL    :: { RVAL} : L_RVAL { RVAL ($1)}
BVAL    :: { BVAL} : L_BVAL { BVAL ($1)}
CVAL    :: { CVAL} : L_CVAL { CVAL ($1)}
CID    :: { CID} : L_CID { CID ($1)}
ID    :: { ID} : L_ID { ID ($1)}

Prog :: { Prog }
Prog : Block { AbsM.Prog_P0 $1 }
Block :: { Block }
Block : Declarations ProgramBody { AbsM.Block_P0 $1 $2 }
Declarations :: { Declarations }
Declarations : ListDeclaration { AbsM.Declarations_P0 (reverse $1) }
ListDeclaration :: { [Declaration] }
ListDeclaration : {- empty -} { [] }
                | ListDeclaration Declaration ';' { flip (:) $1 $2 }
Declaration :: { Declaration }
Declaration : VarDeclaration { AbsM.Declaration_P0 $1 }
            | FunDeclaration { AbsM.Declaration_P1 $1 }
            | DataDeclaration { AbsM.Declaration_P2 $1 }
VarDeclaration :: { VarDeclaration }
VarDeclaration : VAR VarSpecs COLON Type { AbsM.VarDeclaration_P0 $1 $2 $3 $4 }
VarSpecs :: { VarSpecs }
VarSpecs : VarSpec MoreVarSpecs { AbsM.VarSpecs_P0 $1 $2 }
MoreVarSpecs :: { MoreVarSpecs }
MoreVarSpecs : ',' VarSpec MoreVarSpecs { AbsM.MoreVarSpecs_P0 $2 $3 }
             | {- empty -} { AbsM.MoreVarSpecs_P1 }
VarSpec :: { VarSpec }
VarSpec : ID ArrayDimensions { AbsM.VarSpec_P0 $1 $2 }
Type :: { Type }
Type : INT { AbsM.TypeINT $1 }
     | REAL { AbsM.TypeREAL $1 }
     | BOOL { AbsM.TypeBOOL $1 }
     | CHAR { AbsM.TypeCHAR $1 }
     | ID { AbsM.TypeID $1 }
ArrayDimensions :: { ArrayDimensions }
ArrayDimensions : ListArrDims { AbsM.ArrayDimensions_P0 (reverse $1) }
ArrDims :: { ArrDims }
ArrDims : SLPAR Expr SRPAR { AbsM.ArrDims_P0 $1 $2 $3 }
ListArrDims :: { [ArrDims] }
ListArrDims : {- empty -} { [] }
            | ListArrDims ArrDims { flip (:) $1 $2 }
BasicArrayDimensions :: { BasicArrayDimensions }
BasicArrayDimensions : ListEmptyArrDims { AbsM.BasicArrayDimensions_P0 (reverse $1) }
EmptyArrDims :: { EmptyArrDims }
EmptyArrDims : SLPAR SRPAR { AbsM.EmptyArrDims_P0 $1 $2 }
ListEmptyArrDims :: { [EmptyArrDims] }
ListEmptyArrDims : {- empty -} { [] }
                 | ListEmptyArrDims EmptyArrDims { flip (:) $1 $2 }
FunDeclaration :: { FunDeclaration }
FunDeclaration : FUN ID ParamList COLON Type CLPAR FunBlock CRPAR { AbsM.FunDeclaration_P0 $1 $2 $3 $4 $5 $6 $7 $8 }
FunBlock :: { FunBlock }
FunBlock : Declarations FunBody { AbsM.FunBlock_P0 $1 $2 }
FunBody :: { FunBody }
FunBody : BEGIN ProgStmts RETURN Expr ';' END { AbsM.FunBody_P0 $1 $2 $3 $4 $6 }
        | ProgStmts RETURN Expr ';' { AbsM.FunBody_P1 $1 $2 $3 }
ParamList :: { ParamList }
ParamList : LPAR Parameters RPAR { AbsM.ParamList_P0 $1 $2 $3 }
Parameters :: { Parameters }
Parameters : BasicDeclaration MoreParameters { AbsM.Parameters_P0 $1 $2 }
           | {- empty -} { AbsM.Parameters_P1 }
MoreParameters :: { MoreParameters }
MoreParameters : ',' BasicDeclaration MoreParameters { AbsM.MoreParameters_P0 $2 $3 }
               | {- empty -} { AbsM.MoreParameters_P1 }
BasicDeclaration :: { BasicDeclaration }
BasicDeclaration : VarIdentList COLON Type { AbsM.BasicDeclaration_P0 $1 $2 $3 }
VarIdentList :: { VarIdentList }
VarIdentList : VarIdent MoreVarIdent { AbsM.VarIdentList_P0 $1 $2 }
VarIdent :: { VarIdent }
VarIdent : ID BasicArrayDimensions { AbsM.VarIdent_P0 $1 $2 }
MoreVarIdent :: { MoreVarIdent }
MoreVarIdent : ',' VarIdent MoreVarIdent { AbsM.MoreVarIdent_P0 $2 $3 }
             | {- empty -} { AbsM.MoreVarIdent_P1 }
DataDeclaration :: { DataDeclaration }
DataDeclaration : DATA ID EQUAL ConsDeclarations { AbsM.DataDeclaration_P0 $1 $2 $3 $4 }
ConsDeclarations :: { ConsDeclarations }
ConsDeclarations : ConsDeclaration MoreConsDeclaration { AbsM.ConsDeclarations_P0 $1 $2 }
MoreConsDeclaration :: { MoreConsDeclaration }
MoreConsDeclaration : SLASH ConsDeclaration MoreConsDeclaration { AbsM.MoreConsDeclaration_P0 $1 $2 $3 }
                    | {- empty -} { AbsM.MoreConsDeclaration_P1 }
ConsDeclaration :: { ConsDeclaration }
ConsDeclaration : CID OF TypeList { AbsM.ConsDeclaration_P0 $1 $2 $3 }
                | CID { AbsM.ConsDeclaration_P1 $1 }
TypeList :: { TypeList }
TypeList : Type MoreType { AbsM.TypeList_P0 $1 $2 }
MoreType :: { MoreType }
MoreType : MUL Type MoreType { AbsM.MoreType_P0 $1 $2 $3 }
         | {- empty -} { AbsM.MoreType_P1 }
ProgramBody :: { ProgramBody }
ProgramBody : BEGIN ProgStmts END { AbsM.ProgramBody_P0 $1 $2 $3 }
            | ProgStmts { AbsM.ProgramBody_P1 $1 }
ProgStmts :: { ProgStmts }
ProgStmts : ListProgStmt { AbsM.ProgStmts_P0 (reverse $1) }
ListProgStmt :: { [ProgStmt] }
ListProgStmt : {- empty -} { [] }
             | ListProgStmt ProgStmt ';' { flip (:) $1 $2 }
ProgStmt :: { ProgStmt }
ProgStmt : IF Expr THEN ProgStmt ELSE ProgStmt { AbsM.ProgStmt_P0 $1 $2 $3 $4 $5 $6 }
         | WHILE Expr DO ProgStmt { AbsM.ProgStmt_P1 $1 $2 $3 $4 }
         | READ Identifier { AbsM.ProgStmt_P2 $1 $2 }
         | Identifier ASSIGN Expr { AbsM.ProgStmt_P3 $1 $2 $3 }
         | PRINT Expr { AbsM.ProgStmt_P4 $1 $2 }
         | CLPAR Block CRPAR { AbsM.ProgStmt_P5 $1 $2 $3 }
         | CASE Expr OF CLPAR CaseList CRPAR { AbsM.ProgStmt_P6 $1 $2 $3 $4 $5 $6 }
Identifier :: { Identifier }
Identifier : ID ArrayDimensions { AbsM.Identifier_P0 $1 $2 }
CaseList :: { CaseList }
CaseList : Case MoreCase { AbsM.CaseList_P0 $1 $2 }
MoreCase :: { MoreCase }
MoreCase : SLASH Case MoreCase { AbsM.MoreCase_P0 $1 $2 $3 }
         | {- empty -} { AbsM.MoreCase_P1 }
Case :: { Case }
Case : CID VarList ARROW ProgStmt { AbsM.Case_P0 $1 $2 $3 $4 }
VarList :: { VarList }
VarList : LPAR VarListPrime RPAR { AbsM.VarList_P0 $1 $2 $3 }
        | {- empty -} { AbsM.VarList_P1 }
VarListPrime :: { VarListPrime }
VarListPrime : ID MoreVarListPrime { AbsM.VarListPrime_P0 $1 $2 }
MoreVarListPrime :: { MoreVarListPrime }
MoreVarListPrime : ',' ID MoreVarListPrime { AbsM.MoreVarListPrime_P0 $2 $3 }
                 | {- empty -} { AbsM.MoreVarListPrime_P1 }
Expr :: { Expr }
Expr : Expr OR BintTerm { AbsM.Expr_P0 $1 $2 $3 }
     | BintTerm { AbsM.Expr_P1 $1 }
BintTerm :: { BintTerm }
BintTerm : BintTerm AND BintFactor { AbsM.BintTerm_P0 $1 $2 $3 }
         | BintFactor { AbsM.BintTerm_P1 $1 }
BintFactor :: { BintFactor }
BintFactor : NOT BintFactor { AbsM.BintFactor_P0 $1 $2 }
           | IntExpr CompareOp IntExpr { AbsM.BintFactor_P1 $1 $2 $3 }
           | IntExpr { AbsM.BintFactor_P2 $1 }
CompareOp :: { CompareOp }
CompareOp : EQUAL { AbsM.CompareOp_P0 $1 }
          | LESST { AbsM.CompareOp_P1 $1 }
          | GREATERT { AbsM.CompareOp_P2 $1 }
          | LE { AbsM.CompareOp_P3 $1 }
          | GE { AbsM.CompareOp_P4 $1 }
IntExpr :: { IntExpr }
IntExpr : IntExpr AddOp IntTerm { AbsM.IntExpr_P0 $1 $2 $3 }
        | IntTerm { AbsM.IntExpr_P1 $1 }
AddOp :: { AddOp }
AddOp : ADD { AbsM.AddOp_P0 $1 } | SUB { AbsM.AddOp_P1 $1 }
IntTerm :: { IntTerm }
IntTerm : IntTerm MulOp IntFactor { AbsM.IntTerm_P0 $1 $2 $3 }
        | IntFactor { AbsM.IntTerm_P1 $1 }
MulOp :: { MulOp }
MulOp : MUL { AbsM.MulOp_P0 $1 } | DIV { AbsM.MulOp_P1 $1 }
IntFactor :: { IntFactor }
IntFactor : LPAR Expr RPAR { AbsM.IntFactor_P0 $1 $2 $3 }
          | SIZE LPAR ID BasicArrayDimensions RPAR { AbsM.IntFactor_P1 $1 $2 $3 $4 $5 }
          | FLOAT LPAR Expr RPAR { AbsM.IntFactor_P2 $1 $2 $3 $4 }
          | FLOOR LPAR Expr RPAR { AbsM.IntFactor_P3 $1 $2 $3 $4 }
          | CEIL LPAR Expr RPAR { AbsM.IntFactor_P4 $1 $2 $3 $4 }
          | ID ModifierList { AbsM.IntFactor_P5 $1 $2 }
          | IVAL { AbsM.IntFactor_P6 $1 }
          | RVAL { AbsM.IntFactor_P7 $1 }
          | BVAL { AbsM.IntFactor_P8 $1 }
          | SUB IntFactor { AbsM.IntFactor_P9 $1 $2 }
          | CID ConsArgumentList { AbsM.IntFactor_P10 $1 $2 }
          | CVAL { AbsM.IntFactor_P11 $1 }
ModifierList :: { ModifierList }
ModifierList : FunArgumentList { AbsM.ModifierList_P0 $1 }
             | ArrayDimensions { AbsM.ModifierList_P1 $1 }
FunArgumentList :: { FunArgumentList }
FunArgumentList : LPAR Arguments RPAR { AbsM.FunArgumentList_P0 $1 $2 $3 }
Arguments :: { Arguments }
Arguments : ListExpr { AbsM.Arguments_P0 $1 }
ListExpr :: { [Expr] }
ListExpr : {- empty -} { [] }
         | Expr { (:[]) $1 }
         | Expr ',' ListExpr { (:) $1 $3 }
ConsArgumentList :: { ConsArgumentList }
ConsArgumentList : FunArgumentList { AbsM.ConsArgumentList_P0 $1 }
                 | {- empty -} { AbsM.ConsArgumentList_P1 }
{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
}
