
CPSC 411:  Compiling M+ to AM
============================================================================

Author: Robin Cockett
Date: 6 Feb 2002

============================================================================


Steps:  

(1) The LEX/YACC front end for M+ with translation into the syntax tree.  
    Required: basic error handling, construction of the syntax tree (using 
    structures), printing of the syntax tree.  Demonstrate your program on 
    example M+ programs.

(2) Semantic checking for M+.  Using a symbol table check that the 
    syntax tree is legal: scope of variables and that all expressions 
    type check.  Print the IR tree.  Demonstrate your program on 
    example M+ programs.  

(b) generate AM stack machine code .... demonstrate your compiler on 
    example M+ programs (and invent two programs of your own!).


=============================================================================

M+ is an extension of Minisculus which has local variables and 
functions.  The next three assignments require you to implement M+ 
on the AM stack machine.  You will need to write the compiler to 
stack machine code.  You are REQUIRED to use LEX and YACC.

The first thing you must do is to sort out the syntax and implement it 
with a minimum of actions.  The next stage is to check that identifiers 
and functions are correctly used and declared before their use and that 
expressions are correctly typed.  This will require that you build a 
simple symbol table and implement simple type checking.  The last stage 
is to generate the AM code.

=============================================================================



PART I:   LANGUAGE DESCRIPTION
------------------------------

The syntax of minisculus+ is defined as follows:

Minisculus+:
============================================================
prog -> block

block -> declarations program_body

declarations -> declaration SEMICOLON declarations
              | \epsilon

declaration -> var_declaration
             | fun_declaration

var_declaration -> VAR basic_var_declaration

basic_var_declaration -> identifier COLON type 

fun_declaration -> FUN identifier param_list COLON type  
                                            CLPAR fun_block CRPAR

fun_block -> declarations fun_body 

param_list -> LPAR parameters RPAR

parameters -> parameters1 
            | \epsilon

parameters1 -> parameters1 COMMA basic_var_declaration
             | basic_var_declaration

identifier -> ID

type -> INT
      | BOOL 

program_body -> BEGIN prog_stmts END

fun_body -> BEGIN prog_stmts RETURN expr SEMICOLON END

prog_stmts -> prog_stmt SEMICOLON prog_stmts
            | \epsilon                         
                                        
prog_stmt -> IF expr THEN prog_stmt ELSE prog_stmt
           | WHILE expr DO prog_stmt
           | READ ID
           | ID ASSIGN expr
           | PRINT expr
           | CLPAR block CRPAR

expr ->  expr OR bint_term
       | bint_term

bint_term -> bint_term AND bint_factor
           | bint_factor

bint_factor -> NOT bint_factor
             | int_expr compare_op int_expr
             | int_expr

compare_op -> EQUAL | LT | GT | LE |GE

int_expr -> int_expr addop int_term
          | int_term

addop -> ADD | SUB

int_term -> int_term mulop int_factor
          | int_factor

mulop -> MUL | DIV

int_factor -> LPAR expr RPAR
            | ID argument_list
            | NUM
            | BVAL
            | SUB int_factor

argument_list -> LPAR arguments RPAR
               | \epsilon

arguments -> arguments1
           | \epsilon

arguments1 -> arguments1 COMMA expr 
            | expr


============================================================


Terminals of minisculus+
------------------------

"+"  => ADD
"-"  => SUB
"*"  => MUL
"/"  => DIV

"&&" => AND
"||" => OR
"not" => NOT

"="  => EQUAL 
"<"  => LT 
">"  => GT 
"=<"  => LE 
">="  => GE

":=" => ASSIGN

"("   => LPAR
")"   => RPAR
"{"   => CLPAR
"}"   => CRPAR

":"  => COLON
";"  => SEMICLON
","  => COMMA 

"if" => IF
"then" => THEN
"while" => WHILE
"do" => DO
"read" => READ
"else" => ELSE
"begin" => BEGIN
"end" => END
"print" => PRINT
"int"   => INT
"bool"  => BOOL
"var"   => VAR
"fun"   => FUN
"return" => RETURN

{alpha}[_{digit}{alpha}]* => ID          (identifier)
{digit}+ => NUM                          (integer)
"false" => BVAL                          (booleans)
"true" => BVAL

where 

alpha = [a-zA-Z]
digit = [0-9]



Program comments:
----------------

Minusculus+ has two types of comments: multi-line comments 
    /*  comment  */
and one line comments
    %   comment

The multi-line comments allow nesting of comments ...



Commentary on the Minisculus+ grammar
-------------------------------------

A Minisculus+ program is a block that is a list of 
declarations followed by a progam body

============================================================
prog -> block

block -> declarations program_body
============================================================

The decarations can either be function declarations or variable declarations
each declaration is terminated by a semi-colon.

============================================================ 
decarations -> declaration SEMICOLON declarations
             | \epsilon

declaration -> var_declaration
             | fun_declaration
============================================================

A variable declaration is preceded by the reserved word "var" and 
declares an identifier whose type is attached by a colon followed by the 
type.  M+ only has only two types: integers and booleans.  A function 
declaration is preceded by the reserved word "fun" and consists of an 
identifier followed by an argument list with a type attachment followed 
by the function block.  This consist of a declaration list followed 
by the function body enclosed in curly parentheses.  The argument list 
consist of a (possibly empty) list of variable declarations separated by 
commas.

A function can call any function which has already been declared 
or is declared in the same block.  Thus, (mutually) recursive functions 
are permissible. Functions are also allowed to use variables defined in 
the same block.

A variable in a minisculus program can only be legally used if it has 
been declared in an enclosing block or function.  The same is also holds 
true for functions.  Thus M+ supports local function and variable 
definitions ....

============================================================
var_declaration -> VAR basic_var_declaration

basic_var_declaration -> identifier COLON type 

fun_declaration -> FUN identifier param_list COLON type 
                                            CLPAR fun_block CRPAR

fun_block -> declarations fun_body 

param_list -> LPAR parameters RPAR

parameters -> parameters1 
            | \epsilon

parameters1 -> parameters1 COMMA basic_var_declaration
             | basic_var_declaration

identifier -> ID

type -> INT | BOOL
============================================================

The difference between a program body and a function body is that the 
function body MUST end with a return statement.  Otherwise both consist of
a series of program statements separated by semi-colons.  Program statements 
include conditional ("if ... then ... else ...") statements, while loops,
read statements, assignments, print statements, and blocks. Notice that a 
block permits the declaration of local variables and functions and is 
delimited by curly braces.

============================================================
program_body -> BEGIN prog_stmts END

fun_body -> BEGIN prog_stmts RETURN expr SEMICOLON END

prog_stmts -> prog_stmt SEMICOLON prog_stmts
            | \epsilon                         
                                        
prog_stmt -> IF expr THEN prog_stmt ELSE prog_stmt
           | WHILE expr DO prog_stmt
           | READ ID
           | ID ASSIGN expr
           | PRINT expr
           | CLPAR block CRPAR
============================================================

There are two kinds of expression in minisculus+: integer expressions 
and boolean expressions.  The syntax cannot distinguish these 
expressions and thus some type checking is neccessay. 

Boolean expressions are used in conditional and while statements. 
Boolean expressions include the ability to compare integer 
expressions.  


============================================================
expr ->  expr OR bint_term
       | bint_term

bint_term -> bint_term AND bint_factor
           | bint_factor

bint_factor -> NOT bint_factor
             | int_expr compare_op int_expr
             | int_expr

compare_op -> EQUAL | LT | GT | LE |GE

int_expr -> int_expr addop int_term
          | int_term

addop -> ADD | SUB

int_term -> int_term mulop int_factor
          | int_factor

mulop -> MUL | DIV

int_factor -> LPAR expr RPAR
            | ID argument_list
            | NUM
            | BVAL
            | SUB int_factor


============================================================

An argument list is a list of expressions.  Clearly these must 
be correctly typed.

============================================================

argument_list -> LPAR arguments RPAR
               | \epsilon

arguments -> arguments1
           | \epsilon

aguments1 -> arguments1 COMMA expr 
           | expr
============================================================





Questions:
---------

(1) Henry Kwong (Nov. 24, 1999)
Can two functions which are at the same scope level have the
same name but different parameter lists? Or would this be 
considered an error?

ANSWER: This has not been specified!
(a) The most restrictive solution is to disallow ALL 
definition of symbols with the same symbol name ... 
(b) The most general solution is to allow functions which are 
polytypic (i.e. if their parameter lists are different 
then it is OK to have the same name).  This is possible to do 
in m+ ... so why not do it!

(2) Many people!
How long did it take you and how much code did you write?

ANSWER: I wrote almost all the code in three long afternoons/evening ...
I am guessing it took me between 18hrs and 30hrs (i.e. I essentially had 
it going after 18hrs but I continued to test the code for a while).  

The M+ compiler is written in SML.  Typical of programming in a functional 
language the code (one I had all the type and typing errors out) worked 
first time.  Two minor errors were subsequently found --- no one is perfect.

I subsequently modified the code to get the M++ compiler and that took me 
a couple of days (20hrs) although I also had to slightly modify the AM 
assembler as well.
 
The code, (spartan) comments and all, is 1065 lines long:

lex:  80
yacc: 145
symbol table: 89
semantic checking: 229
code generation: 131
pretty printing and utilities: 285
top level: 106

(Louden's TINY language is actually a fair bit simpler than M+ - particularly 
 in the backend as he does not have an IR and has much simpler scoping rules. 
 He does have arrays though. His code is about 2000 lines in C ... )