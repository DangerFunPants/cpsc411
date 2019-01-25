{
module Main (Token (..), Lexeme (..), lexer, AlexPosn, main) where
import System.Environment
}

%wrapper "monadUserState"

$digit = [0-9]      -- digs
$alpha = [a-zA-Z]   -- alphabetic char's


-- for the monad and monadUserState wrapper types, all actions should have type:
--            tokAction :: AlexInput -> Int -> Alex Token

tokens :-

    <0> "%" [^ \n]* \n                  ;
    <state_nested> "%" [^ \n]* \n       ; -- Give single line comments precedence over block comments.

    <0> "/*"                            { increaseCommentDepth }
    <state_nested> "/*"                 { increaseCommentDepth }
    <state_nested> "*/"                 { decreaseCommentDepth }
    <state_nested> [. \n]               ;

    <0> "if"                            { \inp len -> (parseToken inp TIf) }    
    <0> "then"                          { \inp len -> parseToken inp TThen }
    <0> "while"                         { \inp len -> parseToken inp TWhile }
    <0> "do"                            { \inp len -> parseToken inp TDo }
    <0> "read"                          { \inp len -> parseToken inp TInput }
    <0> "else"                          { \inp len -> parseToken inp TElse }
    <0> "begin"                         { \inp len -> parseToken inp TBegin }                                                       
    <0> "end"                           { \inp len -> parseToken inp TEnd }
    <0> "print"                         { \inp len -> parseToken inp TWrite }   

    <0> $alpha [$alpha $digit]*         { \inp len -> parseToken inp (TId (readStr inp len)) }
    <0> $digit+                         { \inp len -> parseToken inp (TNum (read $ readStr inp len)) }                        

    <0> "+"                             { \inp len -> parseToken inp TAdd }      
    <0> ":="                            { \inp len -> parseToken inp TAssign }
    <0> "-"                             { \inp len -> parseToken inp TSub }
    <0> "*"                             { \inp len -> parseToken inp TMul }
    <0> "/"                             { \inp len -> parseToken inp TDiv }                                                          
    <0> "("                             { \inp len -> parseToken inp TLPar }
    <0> ")"                             { \inp len -> parseToken inp TRPar }
    <0> ";"                             { \inp len -> parseToken inp TSemicolon }

    <0> $white                          ;
    <0> .                               { reportError }

{

data Lexeme = TId String
            | TNum Integer
            | TIf
            | TThen
            | TElse
            | TWhile
            | TDo
            | TInput
            | TBegin
            | TEnd
            | TWrite
            | TAdd
            | TAssign
            | TSub
            | TMul
            | TDiv
            | TLPar
            | TRPar
            | TSemicolon
            | TEof deriving (Show, Eq)

-- Created this wrapper type around the actual tokens so that 
-- methods that implement the parser can extract the position value from 
-- the token without needing to explicitly pattern match against the constructors
-- in the Lexeme type.
data Token = Wrap Lexeme AlexPosn deriving (Eq)

instance Show Token where
    show (Wrap tok pos) = show tok

-- Persistent user state contains a list of Positions representing comment tokens that are currently open. 
data AlexUserState = AlexUserState { commentPos :: [AlexPosn] }

alexInitUserState = AlexUserState []
alexEOF = return (Wrap TEof (AlexPn 0 0 0)) 

-- pushes a new comment onto the comment stack
increaseCommentDepth :: AlexInput -> Int -> Alex Token
increaseCommentDepth input@(pos,_,_,_) len = do
    alexSetStartCode state_nested
    pushCommentPosn pos
    skip input len

-- returns the current comment stack and removes the top entry from the comment stack.
decreaseCommentDepth :: AlexInput -> Int -> Alex Token
decreaseCommentDepth input len = do
    stackVal <- popCommentPosn
    if ((length stackVal) == 1) 
        then (alexSetStartCode 0) 
        else (alexSetStartCode state_nested)
    skip input len

-- helper function to decorate Lexeme type with position information.
parseToken :: AlexInput -> Lexeme -> Alex Token
parseToken (pos,_,_,_) tok = return $ (Wrap tok pos)

-- consumes <len> characters from the input stream.
readStr :: AlexInput -> Int -> String
readStr (_,_,_,str) len = take len str

-- helper functions to manipulate the comment stack.
pushCommentPosn :: AlexPosn -> Alex ()
pushCommentPosn posn = 
    Alex $ \s@AlexState{alex_ust=ust} -> Right (s{alex_ust=(alex_ust s){commentPos=[posn]++(commentPos ust)}}, ())

setCommentStack :: [AlexPosn] -> Alex ()
setCommentStack posn = Alex $ \s -> Right(s{alex_ust=(alex_ust s){commentPos=posn}}, ())

getCommentStack :: AlexState -> Either String (AlexState, [AlexPosn])
getCommentStack (s@AlexState{alex_ust=ust}) = Right (s, (commentPos ust))

popCommentPosn :: Alex [AlexPosn]
popCommentPosn = do
    s <- (Alex getCommentStack)
    let newStack = if (null s)
                    then s
                    else tail s
    (setCommentStack newStack)
    return s
    
reportError :: AlexInput -> Int ->  Alex Token
reportError (p,_,_,str) len = alexError ("Error on token "++(take len str)++" at position: "++(show p)) 

-- Function to perform the lexing. Recurses on the token stream until EOF is encountered.
lexer :: String -> Either String [Token]
lexer str =
    let run = do
        (Wrap tok posn) <- alexMonadScan
        if tok == TEof
            then
                do 
                    cd <- popCommentPosn
                    (if (null cd)
                        then do return []
                        else alexError ("Error on comment token: "++(show (head cd))))
            else do toks <- run; return ((Wrap tok posn):toks)
    in runAlex str run    

main = do
    args <- getArgs
    s <- readFile (head args)
    case (lexer s) of
        Right toks -> print toks
        Left err -> print err
}


