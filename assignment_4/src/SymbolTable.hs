module SymbolTable
  ( lookupSymbol
  , lookupDatatype
  , addScopeLevel
  , removeScopeLevel
  , insertSymbol
  , nextLabel
  , SymbolTable(..)
  , InSymbolDescription(..)
  , OutSymbolDescription(..)
  ) where

import           AST
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Debug.Trace

data InSymbolDescription
  = Argument String
             M_type
             Int -- Name Type Cardinality
  | Variable String
             M_type
             Int -- Name Type Cardinality
  | Function String
             [(M_type, Int)]
             M_type -- Name Arguments RetType
  | Datatype String -- TypeName
  | Constructor String
                [M_type]
                String -- Name Arguments ConstructedType
  deriving (Show)

data OutSymbolDescription
  = IVariable Int
              Int
              M_type
              Int -- Level Offset Type Cardinality
  | IFunction Int
              String
              [(M_type, Int)]
              M_type -- Level Label Arguments ReturnType
  | IConstructor Int
                 [M_type]
                 String -- Level Arguments ConstructedTypce
  | IType [String] -- ConsNameList
  deriving (Show)

data Symbol
  = VarAttr Int
            M_type
            Int -- Offset Type and dimension
  | FunAttr String
            [(M_type, Int)]
            M_type -- Label ArgTypes RetType
  | ConsAttr Int
             [M_type]
             String -- ConsNum ArgTypes ConsType
  | TypeAttr [String] -- ConsNames
  deriving (Show)

-- Current level of scope nesting
type Level = Int

-- Positive offset from FP of any given AR
type PosOffset = Int

-- Negative offset from FP of any given AR
type NegOffset = Int

-- A single activation record
data SymbolTablePrim =
  SymbolTable PosOffset
              NegOffset
              [(String, Symbol)]
  deriving (Show)

-- The list of activation records that compose the totality of the symbol table.
type SymbolTableComp = [SymbolTablePrim]

type SymbolTable = State (Int, SymbolTableComp)

nextLabel' :: SymbolTable String
nextLabel' = State $ \s -> nextLabel'' s
  where
    nextLabel'' (labelNo, stc) =
      (("fun" ++ (show labelNo)), ((succ labelNo), stc))

nextLabel :: ExceptT String SymbolTable String
nextLabel = do
  label <- lift $ nextLabel'
  return label

nextConsNum :: SymbolTable Int
nextConsNum = State $ \s -> nextConsNum' s
  where
    nextConsNum' (consNum, stc) = (consNum, ((succ consNum), stc))

getSymbolTable :: SymbolTable SymbolTableComp
getSymbolTable = State $ \s -> getTable s
  where
    getTable state@(labelNo, stc) = (stc, state)

removeScope :: SymbolTable Int
removeScope = State $ \s -> removeScope' s
  where
    removeScope' (labelNo, ((SymbolTable posOff _ _):xs)) =
      ((pred posOff), (labelNo, xs))

addScope :: SymbolTable ()
addScope = State $ \s -> addScope' s
  where
    addScope' (labelNo, stc) = ((), (labelNo, (newSt : stc)))
    newSt = SymbolTable 1 (-4) []

insertSymbolTable :: String -> InSymbolDescription -> SymbolTable ()
insertSymbolTable key var@(Variable _ _ _) =
  State $ \s -> ((), (fst s, addVarToTable (snd s) key var))
  where
    addVarToTable ((SymbolTable posOff negOff syms):rest) key (Variable name varT dims) =
      (SymbolTable (succ posOff) negOff ((key, VarAttr posOff varT dims) : syms)) :
      rest
insertSymbolTable key arg@(Argument _ _ _) =
  State $ \s -> ((), (fst s, addArgToTable (snd s) key arg))
  where
    addArgToTable ((SymbolTable posOff negOff syms):rest) key (Argument name argT dims) =
      (SymbolTable posOff (pred negOff) ((key, VarAttr negOff argT dims) : syms)) :
      rest
insertSymbolTable key fun@(Function _ _ _) = do
  State $ \s -> ((), (fst s, addFunToTable (snd s) key fun))
  where
    addFunToTable ((SymbolTable posOff negOff syms):rest) key (Function name args retT) =
      (SymbolTable posOff negOff ((key, FunAttr name args retT) : syms)) : rest
insertSymbolTable key d@(Datatype _) =
  State $ \s -> ((), (fst s, addDataToTable (snd s) key d))
  where
    addDataToTable ((SymbolTable posOff negOff syms):rest) key (Datatype name) =
      (SymbolTable posOff negOff ((key, TypeAttr [name]) : syms)) : rest
insertSymbolTable key cons@(Constructor _ _ _) = do
  consNum <- nextConsNum
  State $ \s -> ((), (fst s, (addConsToTable (snd s) key cons consNum)))
  where
    addConsToTable ((SymbolTable posOff negOff syms):rest) key (Constructor name argTypes consType) consNum =
      (SymbolTable
         posOff
         negOff
         ((key, ConsAttr consNum argTypes consType) : syms)) :
      rest

insertSymbol :: String -> InSymbolDescription -> ExceptT String SymbolTable ()
  -- | trace ("Inserting: " ++ (key) ++ " with value: " ++ (show inSym)) False =
  --   undefined
-- insertSymbol key inSym
insertSymbol key inSym = do
  st <- lift $ getSymbolTable
  if null st
    then throwE "Cannot insert into empty symbol table"
    else case searchSymTable (head st) key 0 of
           Nothing -> do
             lift $ (insertSymbolTable key inSym)
             return ()
           Just _ ->
             throwE
               ("Key collisions in symbol table. " ++ (show inSym) ++ (show key))

convOutSym :: Symbol -> Int -> OutSymbolDescription
convOutSym (VarAttr offset varType dims) level =
  IVariable level offset varType dims
convOutSym (FunAttr funId argList retType) level =
  IFunction level funId argList retType
convOutSym (ConsAttr a typeList consId) _ = IConstructor a typeList consId
convOutSym (TypeAttr consName) _ = IType consName

searchSymTable :: SymbolTablePrim -> String -> Int -> Maybe OutSymbolDescription
searchSymTable (SymbolTable posOff negOff ((symName, sym):xs)) searchStr level =
  if searchStr == symName
    then Just $ convOutSym sym level
    else (searchSymTable (SymbolTable posOff negOff xs) searchStr level)
searchSymTable (SymbolTable posOff negOff []) searchStr _ = Nothing

lookupSymbol :: String -> ExceptT String SymbolTable OutSymbolDescription
lookupSymbol symName = do
  st <- lift $ getSymbolTable
  (searchAll st symName 0)

searchAll ::
     SymbolTableComp
  -> String
  -> Int
  -> ExceptT String SymbolTable OutSymbolDescription
searchAll (st:rest) symName level = do
  case searchSymTable st symName level of
    Nothing  -> searchAll rest symName (succ level)
    Just osd -> return osd
searchAll [] name _ = throwE ("Failed to find symbol: " ++ name)

lookupDatatype :: String -> ExceptT String SymbolTable OutSymbolDescription
lookupDatatype symName = do
  st <- lift $ getSymbolTable
  symbol <- searchAll st symName 0
  case symbol of
    (IType _) -> return symbol
    otherwise -> throwE ("Failed to find datatype: " ++ symName)

addScopeLevel :: ExceptT String SymbolTable ()
addScopeLevel = do
  lift $ addScope

removeScopeLevel :: ExceptT String SymbolTable Int
removeScopeLevel = do
  st <- lift $ getSymbolTable
  if null st
    then throwE "Attempt to remove scope level from empty symbol table."
    else do
      varCount <- lift $ removeScope
      return varCount
