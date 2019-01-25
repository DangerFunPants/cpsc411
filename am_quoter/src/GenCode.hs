module GenCode
    ( genCode
    ) where

import IR;
import AST;

import Data.List

data Instruction
    -- Load Instructions
    = LoadFp
    | LoadSp
    | LoadCp
    | LoadF Float
    | LoadI Int
    | LoadB Bool
    | LoadC Char
    | LoadO Int
    | LoadOS
    | LoadH
    -- Store Instructions
    | StoreFp
    | StoreSp
    | StoreO Int
    | StoreOS
    | StoreH Int
    | StoreHO Int
    | StoreF Float
    | StoreI Int
    | StoreB Bool
    -- Jump Instructions
    | JumpA String
    | JumpC String
    | JumpS
    | JumpO
    -- Operation Application
    | App Operation
    -- Allocation Instructions
    | AllocI Int
    | AllocS
    | AllocH Int
    -- Print Instructions
    | PrintF
    | PrintI
    | PrintB
    | PrintC
    -- Read Instructions
    | ReadF
    | ReadI
    | ReadB
    | ReadC
    -- Labels
    | Label String
    -- Halt
    | Halt
    deriving (Show)

data Operation
    -- Floating point operations
    = AddF
    | SubF
    | DivF
    | MulF
    | NegF
    | Floor
    | Ceil
    | LtF
    | LeF 
    | GtF
    | GeF
    | EqF
    -- Integer Operations
    | AddI
    | SubI
    | DivI
    | MulI
    | NegI
    | Float
    | LtI
    | LeI
    | GtI
    | GeI
    | EqI
    -- Character Operations
    | LtC
    | LeC
    | GtC 
    | GeC
    | EqC
    -- Boolean Operations
    | And
    | Or
    | Not
    deriving (Show)

printOperation :: Operation -> String
printOperation op = 
    case op of
        AddF -> "ADD_F"
        SubF -> "SUB_F"
        DivF -> "DIV_F"
        MulF -> "MUL_F"
        NegF -> "NEG_F"
        Floor -> "FLOOR"
        Ceil -> "CEIL"
        AddI -> "ADD"
        SubI -> "SUB"
        DivI -> "DIV"
        MulI -> "MUL"
        NegI -> "NEG"
        Float -> "FLOAT"
        LtF -> "LT_F"
        LeF -> "LE_F"
        GtF -> "GT_F"
        GeF -> "GE_F"
        EqF -> "EQ_F"
        LtI -> "LT"
        LeI -> "LE"
        GtI -> "GT"
        GeI -> "GE"
        EqI -> "EQ"
        LtC -> "LT_C"
        LeC -> "LE_C"
        GtC -> "GT_C"
        GeC -> "GE_C"
        EqC -> "EQ_C"
        And -> "AND"
        Or -> "OR"
        Not -> "NOT"

printInstruction :: Instruction -> String
printInstruction (Label v) = v++":\n"
printInstruction instr = "\t"++instrName++"\n"
    where
        instrName = case instr of
                        LoadFp -> "LOAD_R %fp"
                        LoadSp -> "LOAD_R %sp"
                        (LoadF v) -> "LOAD_F "++(show v)
                        (LoadI v) -> "LOAD_I "++(show v)
                        (LoadB v) -> "LOAD_B "++
                            (case v of
                                True -> "true"
                                False -> "false")
                        (LoadC v) -> ("LOAD_C "++"\""++[(v)]++"\"")
                        (LoadO v) -> "LOAD_O "++(show v)
                        (LoadOS) -> "LOAD_OS"
                        (StoreFp) -> "STORE_R %fp"
                        (StoreSp) -> "STORE_R %sp"
                        (StoreO v) -> "STORE_O "++(show v)
                        (StoreH v) -> "STORE_H "++(show v)
                        (StoreHO v) -> "STORE_HO "++(show v)
                        (StoreF v) -> "STORE_F "++(show v)
                        (StoreI v) -> "STORE_I "++(show v)
                        (StoreB v) -> "STORE_B "++(show v)
                        (StoreOS) -> "STORE_OS"
                        (App op) -> "APP "++(printOperation op)
                        (JumpA v) -> "JUMP "++(v)
                        (JumpS) -> "JUMP_S"
                        (JumpC v) -> "JUMP_C "++(v)
                        (JumpO) -> "JUMP_O"
                        (AllocI v) -> "ALLOC "++(show v)
                        (AllocS) -> "ALLOC_S"
                        (AllocH v) -> "ALLOC_H "++(show v)
                        (ReadF) -> "READ_F"
                        (ReadI) -> "READ_I"
                        (ReadB) -> "READ_B"
                        (ReadC) -> "READ_C"
                        (PrintF) -> "PRINT_F"
                        (PrintI) -> "PRINT_I"
                        (PrintB) -> "PRINT_B"
                        (PrintC) -> "PRINT_C"
                        (Halt) -> "HALT"
                        (LoadCp) -> "LOAD_R %cp"
                        (LoadH) -> "LOAD_H"
{-
================================================================================
State Type
================================================================================
    Stores the current label number.
-}

type LabelState = State Int
nextLabel :: LabelState String
nextLabel = State $ \s -> ("l"++(show s), (succ s))

{-
================================================================================
Declarations
================================================================================
-}

-- Local variable allocation for primitive types. 
genCodeVarDecl :: Int -> LabelState [Instruction]
genCodeVarDecl varC = do
    return [(AllocI varC)]

-- Function declaration lists (Global, Function and Block)
genCodeFunDecls :: [IFunBody] -> LabelState [Instruction]
genCodeFunDecls fbList = do
    funcCode <- mapM genCodeFunDecl fbList
    return (concat funcCode)

-- Function Bodies.
genCodeFunDecl :: IFunBody -> LabelState [Instruction]
genCodeFunDecl (IFunBody (name, localFuncs, varCount, argCount, arrs, stmts)) = do
    varDeclCode <- genCodeVarDecl varCount
    localFuncCode <- genCodeFunDecls localFuncs
    funBody <- genCodeStmts stmts
    arrDeclCode <- genCodeArrDecls arrs (succ varCount)
    return $
        [ (Label name) ]
        ++(initCode)
        ++(arrDeclCode)
        ++(funBody)
        ++(cleanupCode)
        ++(localFuncCode)
    where 
        initCode = 
            [ (LoadSp)
            , (StoreFp)
            , (AllocI varCount)
            , (LoadI (negate (varCount + 2)))
            ]
        cleanupCode = 
            [ LoadFp 
            , (StoreO (negate (argCount + 3)))
            , LoadFp
            , (LoadO 0)
            , LoadFp
            , (StoreO (negate (argCount + 2)))
            , LoadFp
            , (LoadO (succ varCount))
            , (AllocS)
            , StoreFp
            , (AllocI (negate argCount))
            , JumpS
            ]

genCodeArrDecls :: [(Int, [IExpr])] -> Int -> LabelState [Instruction]
genCodeArrDecls (x:xs) dealloc = do
    thisOne <- genCodeArrDecl x dealloc
    recCall <- genCodeArrDecls xs dealloc
    return (thisOne++recCall)
genCodeArrDecls [] _  = return []

genCodeArrDecl :: (Int, [IExpr]) -> Int -> LabelState [Instruction]
genCodeArrDecl (offset, arrExpr) dealloc= do
    -- this should push all array dims onto the top of the stack.
    firstInd <- genCodeExpr (head arrExpr)
    restInd <- genCodeExprs (tail arrExpr)
    sizeComps <- mapM genCodeExpr arrExpr
    totalSizeCode <- computeSize sizeComps
    let sizeCode = if (odd (length arrExpr)) && ((length arrExpr) > 1)
                    then totalSizeCode++[(App MulI)]
                    else totalSizeCode
    -- update the deallocation counter
    return $
        firstInd++
        [ (LoadSp)
        , (LoadFp)
        , (StoreO (offset))
        ]++
        restInd++
        sizeCode++
        [ (App NegI)
        , (LoadFp)
        , (LoadO dealloc)
        , (App AddI)
        , (LoadI (negate card))
        , (App AddI)
        , (LoadFp)
        , (StoreO dealloc)
        ]++
        sizeCode++
        [ (AllocS) ]
    where
        -- TODO: Make this less horrible, caller currently needs to know alot 
        -- about what is going on in this function.
        computeSize :: [[Instruction]] -> LabelState [Instruction]
        computeSize (c1:[]) = do
            return c1
        computeSize (c1:c2:rest) = do
            recCall <- computeSize rest
            return (c1++c2++[(App MulI)]++recCall)
        computeSize [] = return []
        card = (length arrExpr)

{-
================================================================================
Statements
================================================================================
-}

-- Statement Lists (Function Bodies, Program Scope and Blocks)
genCodeStmts :: [IStmt] -> LabelState [Instruction]
genCodeStmts stmtList = do
    stmtsCode <- (mapM genCodeStmt stmtList)
    return (concat stmtsCode)

genCodeStmt :: IStmt -> LabelState [Instruction]
-- Reading literals
genCodeStmt (IReadI pars) = genReadCode pars ReadI
genCodeStmt (IReadF pars) = genReadCode pars ReadF
genCodeStmt (IReadB pars) = genReadCode pars ReadB
genCodeStmt (IReadC pars) = genReadCode pars ReadC

-- Printing literals
genCodeStmt (IPrintI expr) = genPrintCode expr PrintI
genCodeStmt (IPrintF expr) = genPrintCode expr PrintF
genCodeStmt (IPrintB expr) = genPrintCode expr PrintB
genCodeStmt (IPrintC expr) = genPrintCode expr PrintC

-- Primitive assignment
genCodeStmt (IAssign (lvl, offset, arrExpr, rhs)) = do
    evalCode <- genCodeExpr rhs
    storeCode <- storeValue lvl offset arrExpr
    return $ evalCode++storeCode

-- Loop constructs
genCodeStmt (IWhile (condExpr, stmtBlock)) = do
    condCode <- genCodeExpr condExpr
    bodyCode <- genCodeStmt stmtBlock
    l1 <- nextLabel
    l2 <- nextLabel
    return $
        [ (Label l1) ]
        ++ condCode ++
        [ (JumpC l2) ]
        ++ bodyCode ++
        [ (JumpA l1)
        , (Label l2)
        ]

-- Conditionals
genCodeStmt (ICond (condExpr, thenStmt, elseStmt)) = do
    condCode <- genCodeExpr condExpr
    thenBodyCode <- genCodeStmt thenStmt
    elseBodyCode <- genCodeStmt elseStmt
    l1 <- nextLabel
    l2 <- nextLabel
    return $ 
        condCode++
        [ (JumpC l1) ]
        ++ thenBodyCode ++
        [ (JumpA l2)
        , (Label l1) 
        ]
        ++ elseBodyCode ++
        [ (Label l2) ]

-- Statment Blocks
genCodeStmt (IBlock (fbList, varCount, arrExpr, stmtList)) = do
    bodyCode <- genCodeStmts stmtList
    arrDeclCode <- genCodeArrDecls arrExpr (succ varCount)
    return $ 
        newRecord++
        arrDeclCode++
        bodyCode++
        leaveRecord
    where
        newRecord = 
            [ LoadFp
            , (AllocI 2)
            , LoadSp
            , StoreFp
            , (AllocI varCount)
            , (LoadI (negate (varCount+3)))
            ]
        leaveRecord = 
            [ LoadFp
            , (LoadO (succ varCount)) -- Load the deallocation counter
            , (AllocS)
            , StoreFp
            ]

-- Function return
genCodeStmt (IReturn expr) = do
    exprCode <- genCodeExpr expr
    return exprCode 

genCodeStmt (ICase (caseExpr, caseList)) = do
    blockInit <- genCodeEnterAr
    loadExprCode <- genCodeExpr caseExpr
    labelList <- mapM (\_ -> nextLabel) caseList
    labels <- mapM genCodeCaseLabel labelList
    returnLabel <- nextLabel
    caseStmtCode <- genCodeCaseStmts (sortBy sortCase caseList) labelList returnLabel
    return $ 
        blockInit++
        loadExprCode++
        [ (LoadH)
        , (JumpO)
        ]++
        labels++
        caseStmtCode++
        [ (Label ("case_"++returnLabel))
        , (StoreFp)
        ]
    where
        sortCase :: (Int, Int, IStmt) -> (Int, Int, IStmt) -> Ordering
        sortCase (c1, _, _) (c2, _, _) = c1 `compare` c2
        genCodeEnterAr :: LabelState [Instruction]
        genCodeEnterAr = do
            return $
                [ (LoadFp)      -- Static link
                , (AllocI 2)    -- Void spaces (SL at off 2 from %fp)
                , (LoadSp)     
                , (StoreFp)     -- Update %fp
                ]
        genCodeCaseStmts :: [(Int, Int, IStmt)] -> [String] -> String -> LabelState [Instruction]
        genCodeCaseStmts ((cNum, argC, stmt):rest) (label:labels) retLabel = do
            stmtCode <- genCodeStmt stmt
            recCall <- genCodeCaseStmts rest labels retLabel
            return $
                [ (Label ("case_"++label)) ]++
                stmtCode++
                [ (AllocI (negate (argC + 2))) ]++
                [ (JumpA ("case_"++retLabel)) ]++
                recCall
        genCodeCaseStmts [] _ _ = return []
        genCodeCaseLabel :: String -> LabelState Instruction
        genCodeCaseLabel label = do
            return $ (JumpA ("case_"++(label)))

        

{-
--------------------------------------------------------------------------------
Statement Helpers
--------------------------------------------------------------------------------
-}
genReadCode :: (Int, Int, [IExpr]) -> Instruction -> LabelState [Instruction]
genReadCode (lvl, offset, arrExpr) oper = do
    storeCode <- storeValue lvl offset arrExpr    
    return $ [oper]++storeCode

genPrintCode :: IExpr -> Instruction -> LabelState [Instruction]
genPrintCode expr oper = do
    exprCode <- genCodeExpr expr
    return $ exprCode++[oper]


{-
================================================================================
Expressions
================================================================================

    This set of functions should generate AM code that places the result of 
    evaluating the expression on the top of the stack. 
-}

genCodeExprs :: [IExpr] -> LabelState [Instruction]
genCodeExprs exprList = do
    exprCode <- mapM genCodeExpr exprList
    return (concat exprCode)

genCodeExpr :: IExpr -> LabelState [Instruction]

-- Primitive literals
genCodeExpr (IIVal val) = do
    return [(LoadI val)]
genCodeExpr (IRVal val) = do
    return [(LoadF val)]
genCodeExpr (IBVal val) = do
    return [(LoadB val)]
genCodeExpr (ICVal val) = do
    return [(LoadC val)]

-- Variable IDs
genCodeExpr (IId (lvl, offset, arrExpr)) = do
    loadCode <- loadValue lvl offset arrExpr
    return loadCode

-- Array Size expression
genCodeExpr (ISize (lvl, offset, dimension)) = do
    loadDimCode <- loadArrDim lvl offset dimension
    return loadDimCode

{-
--------------------------------------------------------------------------------
Application Statements
--------------------------------------------------------------------------------
    The following functions implement the built in unary and binary operations
    as well as function application and datatype construction. 
-}

-- Function application
genCodeExpr (IApp ((ICall (label, lvl)), args)) = do
    argCode <- genCodeExprs args
    chaseCode <- chaseLink lvl
    return $
        (argCode) ++
        [ (AllocI 1) ]
        ++ chaseCode ++
        [ LoadFp 
        , LoadCp
        , (JumpA label) 
        ]

-- Data Constructors
genCodeExpr (IApp ((ICons (consNum, argC)), argList)) = do
    consIndexCode <- genCodeConsIndex consNum
    exprCode <- genCodeExprs argList
    storeArgsCode <- genCodeStoreConsArgs argC
    return $ 
        exprCode++
        consIndexCode++
        storeArgsCode
    where
        genCodeConsIndex :: Int -> LabelState [Instruction]
        genCodeConsIndex consNum = 
            return $ 
                [ (LoadI consNum)
                ]
        genCodeStoreConsArgs :: Int -> LabelState [Instruction]
        genCodeStoreConsArgs argCount = do
            return $ 
                [ (StoreH (argCount + 1)) ]

-- Binary and unary operation applications
genCodeExpr (IApp (opType, exprList)) = do
    exprCode <- genCodeExprs exprList
    return $ (exprCode)++[(App oper)]
    where
        oper = getArithmeticOper opType

getArithmeticOper :: IOpn -> Operation
getArithmeticOper op
        -- Integer operations
        | op == IAddI = AddI
        | op == ISubI = SubI
        | op == IMulI = MulI
        | op == IDivI = DivI
        | op == ILtI = LtI
        | op == ILeI = LeI
        | op == IGtI = GtI
        | op == IGeI = GeI
        | op == INegI = NegI
        | op == IEqI = EqI
        | op == IFloat = Float
        -- Floating point operations
        | op == IAddF = AddF
        | op == ISubF = SubF
        | op == IMulF = MulF
        | op == IDivF = DivF
        | op == ILtF = LtF
        | op == ILeF = LeF
        | op == IGtF = GtF
        | op == IGeF = GeF
        | op == INegF = NegF
        | op == IEqF = EqF
        | op == IFloor = Floor
        | op == ICeil = Ceil
        -- Boolean operations
        | op == INot = Not
        | op == IOr = Or
        | op == IAnd = And
        -- Character operations
        | op == ILtC = LtC
        | op == ILeC = LeC
        | op == IGtC = GtC
        | op == IGeC = GeC
        | op == IEqC = EqC
        
{-
================================================================================
Global Helpers
================================================================================
-}

chaseLink :: Int -> LabelState [Instruction]
chaseLink lvl = do
    return ((LoadFp):(replicate lvl chaseInstr))
    where
        chaseInstr = (LoadO (-2))

genInitCode :: LabelState [Instruction]
genInitCode = do
    return
        [ LoadSp
        , LoadSp    
        , StoreFp
        ]

genCode' :: IProg -> LabelState String
genCode' (IProg (fbList, varC, arrExpr, progStmts)) = do
    initCode <- genInitCode
    varDeclCode <- genCodeVarDecl varC
    arrDeclCode <- genCodeArrDecls arrExpr (succ varC)
    funDeclCode <- genCodeFunDecls fbList
    progStmtCode <- genCodeStmts progStmts
    let codeList = fmap printInstruction
            ( initCode
            ++varDeclCode
            ++[(LoadI (-2 - varC))]
            ++arrDeclCode
            ++progStmtCode
            ++(cleanupCode varC)
            ++[Halt]
            ++funDeclCode
            ++arrErrCode
            )
    return (foldl1 (++) codeList)
    where
        cleanupCode varC = 
            [ LoadFp
            , (LoadO (varC + 1))
            , (AllocS)
            ]
        arrErrCode = 
            [ (Label "arr_err")
            , (LoadC 'A')
            , (PrintC)
            , (LoadC 'R')
            , (PrintC)
            , (LoadC 'R')
            , (PrintC)
            , (LoadC 'A')
            , (PrintC)
            , (LoadC 'Y')
            , (PrintC)
            , (LoadC ' ')
            , (PrintC)
            , (LoadC 'E')
            , (PrintC)
            , (LoadC 'R')
            , (PrintC)
            , (LoadC 'R')
            , (PrintC)
            , (LoadC 'O')
            , (PrintC)
            , (LoadC 'R')
            , (PrintC)
            , (Halt)
            ]
{-
================================================================================
Array Offset Calculation
================================================================================
-}

-- Load the Nth dimension of an array from its header.
-- to the top of the stack.
loadArrDim :: Int -> Int -> Int -> LabelState [Instruction]
loadArrDim lvl offset n = do
    chaseCode <- chaseLink lvl
    return $
        chaseCode++
        [ (LoadO (offset))
        , (LoadO n)
        ]
loadDimOffset :: Int -> Int -> Int -> Int -> LabelState [Instruction]
loadDimOffset lvl offset card count = do
    -- load the next dimension onto the stack.
    loadCode <- loadArrDim lvl offset count
    recCall <- loadDimOffset' lvl offset card (succ count)
    return (loadCode++recCall)

loadDimOffset' :: Int -> Int -> Int -> Int -> LabelState [Instruction]
loadDimOffset' lvl offset card count
    | count == card = return []
    | otherwise = do
        loadCode <- loadArrDim lvl offset (count)
        recCall <- loadDimOffset' lvl offset card (succ count)
        return (loadCode++[(App MulI)]++recCall)
calcOffset :: [IExpr] -> Int -> Int -> Int -> Int -> LabelState [Instruction]
calcOffset (e1:e2:rest) lvl offset card count = do
    -- load the expression
    exprCode <- genCodeExpr e1
    -- load the offset factor for this dimension
    offsetCode <- loadDimOffset lvl offset card (succ count)
    -- keep traversing the expression list.
    recCall <- calcOffset' (e2:rest) lvl offset card (succ count)
    return $
        exprCode++
        offsetCode++
        [ (App MulI) ]++
        recCall
calcOffset (e1:rest) lvl offset card count = do
    exprCode <- genCodeExpr e1
    return exprCode
calcOffset' :: [IExpr] -> Int -> Int -> Int -> Int -> LabelState [Instruction]
calcOffset' (e1:e2:rest) lvl offset card count = do
    exprCode <- genCodeExpr e1
    offsetCode <- loadDimOffset lvl offset card (succ count)
    recCall <- calcOffset' (e2:rest) lvl offset card (succ count)
    return $
        exprCode++
        offsetCode++
        [ (App MulI) 
        , (App AddI)
        ]++
        recCall
calcOffset' (e:rest) _ _ _ _ = do
    exprCode <- genCodeExpr e
    return (exprCode++[(App AddI)])
calcOffset' [] _ _ _ _ = return []

{-
--------------------------------------------------------------------------------
Load/Store helpers
--------------------------------------------------------------------------------
    The following functions will store/load the value ontop of the stack 
    at the specified lvl/offset/array_offset.
-}
storeValue :: Int -> Int -> [IExpr] -> LabelState [Instruction]
storeValue lvl offset exprList = do
    chaseCode <- chaseLink lvl
    if ((length exprList) == 0)
        then return $ chaseCode++[(StoreO offset)]
        else do
            arrIndCode <- calcOffset exprList lvl offset (length exprList) 0
            checkIndCode <- boundsCheck lvl offset exprList 0
            return $ 
                checkIndCode++
                chaseCode++
                [ (LoadO offset) ]++
                arrIndCode++
                [ (LoadI (length exprList)) 
                , (App AddI)
                , (StoreOS)
                ]

loadValue :: Int -> Int -> [IExpr] -> LabelState [Instruction]
loadValue lvl offset arrExpr = do
    staticRes <- chaseLink lvl
    if ((length arrExpr) == 0)
        then return $ staticRes++[ (LoadO offset) ]
        else do
            arrDimCode <- calcOffset arrExpr lvl offset (length arrExpr) 0
            checkIndCode <- boundsCheck lvl offset arrExpr 0
            return $
                checkIndCode++
                staticRes++
                [ (LoadO offset)
                ]++
                arrDimCode++
                [ (LoadI (length arrExpr))
                , (App AddI)
                , (LoadOS)
                ]

boundsCheck :: Int -> Int -> [IExpr] -> Int -> LabelState [Instruction]
boundsCheck lvl offset (e1:rest) count = do
    thisCode <- boundsCheck' lvl offset e1 count
    recCall <- boundsCheck lvl offset rest (succ count)
    return $
        thisCode++
        [ (App LtI)
        , (JumpC "arr_err")
        ]++
        recCall
    where
        boundsCheck' lvl offset expr count = do
            exprCode <- genCodeExpr expr
            arrDimCode <- loadArrDim lvl offset count
            return $ exprCode++arrDimCode
boundsCheck _ _ [] _ = return []

{-
================================================================================
Exported Functions
================================================================================
    The following functions comprise the public API of this module. 
-}

genCode :: IProg -> String
genCode prog = progSource
    where
        stComp = runState (genCode' prog) 0
        progSource = fst stComp
