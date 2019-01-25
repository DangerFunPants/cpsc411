CPSC411 -- Datatypes for M+-


Author: Robin Cockett
Date: 6 March, 2003 (updated 12th March 2003)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Louden: 5.5 
Dragon book: 5.2 (287-293)

The practice of compiler writing has changed greatly from the early days 
of computing when limited storage and speed were ever present 
constraints.  Under these constraints it was important to attempt to compile 
programs in one pass.  This constraint had a knock-on effect both on the
design of programming languages and what it was felt could reasonably 
be attempted by a compiler.  

It also led to a number of interesting but, perhaps now, less relevant 
bits of theory which were explicitly concerned with achieving compilation 
in one pass.  The notion of an L-attributed grammar is an example of this 
as is (more controversially) the notion that one should compute attributes 
in place: the modern (but not universally accepted) approach is to generate 
a new structure (as an attribute) from the old structure. 

The more modern view tends to favor the decoupling of the parsing from the 
code generation stage by inserting a series of steps between the 
output of the parser and the actual generation of code.  Each of these steps 
may actually generate a datatype (or data structure).  Not only is this
"good software engineering practice", as it facilitates modularity and 
debugging, but also it actually allow a greater flexibility for the 
program constructs in languages.   An example is the added 
flexibility in Java which allows the use of some constructs before 
their declaration.  A smaller example is the declaration structure in 
m+ which allows the use of functions and variables defined at the same 
level.!

The series of steps moving from one datatype to the next can usually 
be optimized into one big step.  As this optimization is a mechanical 
process it makes sense to start by developing the program with the 
small steps (to get it correct) then later -- if speed really is an 
issue -- return to optimize the code.

Syntax tree
----------- 

 The first generic step is to generate a syntax tree out of the parsing 
stage.  This is a representation of the parsed input which is suitable 
for the semantic analysis steps which follow.  The syntax tree is usually 
not the parse tree itself: the parse tree often carries too much detailed 
parsing information. However, it is must be closely related to the parse 
tree as semantic errors will be detected using this structure and these 
errors must be displayed to the programmer in a manner which is related 
to the program text.  

Example (Additive expressions again!): 

   Consider again the expression grammar.

Pexp:        expr -> term rest              
Prest1:      rest -> ADD term rest
Prest2:           | SUB term rest
Prest3:           | \epsilon
Pterm:      term -> NUM

The parse tree of "3+4-6" is 

   Pexp(Pterm(3),Prest1(+,Pterm(4),Prest2(-,Pterm(6),Prest3())))

while an abstract syntax tree might be 

   sub(add(num(3),num(4)),num(6))

These two representations are not equivalent!  In the parse trees we 
associate to the right while in the abstract syntax tree we associate 
to the left.  Furthermore, the abstract syntax allows BOTH associations
while the parse tree is specifically designed to exclude that ambiguity. 
Thus, syntax trees may be a more flexible representation which could 
even allow for possibilities which cannot occur as the result of a parsed 
input.

Of course, some of this flexibility can be achieved using precedence 
relations in YACC to disambiguate a deliberately ambiguous grammar.  
In a sense this facility in YACC is a recognition of the value of 
structures which do not have all the detailed parsing information.
present.


Semantic checking to Intermediate representation
------------------------------------------------
      
   The semantic analysis stage checks the parse tree for semantic errors 
and produces an intermediate representation.  It is at this stage that 
one sorts out the scope issues associated to variables.  One also detects 
and type errors which occur:  e.g. trying to assign an integer 
to a boolean, a function with the wrong number of arguments or wrong type 
of arguments ...  The type information can also be used to disambiguate 
certain (ad hoc) polymorphic functions and to insert coercions 
(e.g. assignments of an integer to a reals can be replaced by first floating 
the integer then making the assignment).

     Out of the semantic analysis stage (often) comes an intermediate 
representation which is closer to the form of the machine code we wish 
to generate but may still be abstract enough to manipulate.  For a simple 
stack based compilation (e.g. m+) this representation will, for 
example, tell us the levels (distance from static definition) and offsets 
of our variables and the levels and label to the code of our functions. 
Thus, in this case, we may have as an intermediate representation a tree 
which has replaced all variables names by offset and level information 
and all functions names by the level and label (and for built in functions 
special internal codes).

    The intermediate code in a more complex compiler may still be some 
distance from the actual code.  For example if a three address code is used 
and we are compiling to a RISC (Reduced Instruction Set Computer) architecture, 
such as the SPARC we may yet have to work out a way of allocating registers.  
Efficient use of registers on these architectures is probably the single 
biggest factor in an optimizing the compilation.  On the other hand, if we 
are compiling to a CISC (Complex Instruction Set Computer) then we may have 
to face problems of instruction selection - as there may be many different 
(with different efficiencies) ways of carrying out the overall desired 
computation.

    By the time the semantic checking is complete all the errors which the 
compiler can detect -- that is all the static checking of the language 
constructs - should have been detected.  Thus, the intermediate code should 
have no errors and should not generate any further error messages (on which 
the user is expected to act) on the remaining passage into code (however 
long that process may be!).


  The fact that the intermediate code need no longer bear a close 
relationship to the original program means that at this stage we can 
attempt major program transformations and optimizations. Thus this 
intermediate level is the appropriate time at which to perform 
significant optimizations.  Once the translations into assembler -
replete with its machine idiosyncrasies - has been undertaken much of 
the structure of the program will have been lost.  Thus, major optimizing 
transformations may be difficult to achieve after this stage.

While there are certainly some very effective "peephole" optimizations 
which can be performed at the assembler level to remove unnecessary 
operations and to do some strength reductions (replacing expensive 
operations with cheaper ones) these optimizations are (as their name 
suggests) very local in nature.  


Syntax tree for m+
------------------

Consider the language m+- defined by 

============================================================
prog -> block

block -> declarations program_body.

declarations -> declaration SEMICOLON declarations
             |.

declaration -> var_declaration
             | fun_declaration.

var_declaration -> VAR ID array_dimensions COLON type.

type -> INT
      | REAL
      | BOOL. 

array_dimensions -> SLPAR expr SRPAR array_dimensions
             |.

fun_declaration -> FUN ID param_list COLON type  
                                            CLPAR fun_block CRPAR.

fun_block -> declarations fun_body. 

param_list -> LPAR parameters RPAR.

parameters -> basic_declaration more_parameters
            |.

more_parameters -> COMMA  basic_declaration more_parameters
            |.

basic_declaration -> ID basic_array_dimensions COLON type.

basic_array_dimensions -> SLPAR SRPAR basic_array_dimensions
             |.

program_body -> BEGIN prog_stmts END.

fun_body -> BEGIN prog_stmts RETURN expr SEMICOLON END.

prog_stmts -> prog_stmt SEMICOLON prog_stmts
            |.                         
                                        
prog_stmt -> IF expr THEN prog_stmt ELSE prog_stmt
           | WHILE expr DO prog_stmt
           | READ identifier
           | identifier ASSIGN expr
           | PRINT expr
           | CLPAR block CRPAR.

identifier -> ID array_dimensions.

expr ->  expr OR bint_term
       | bint_term.

bint_term -> bint_term AND bint_factor
           | bint_factor.

bint_factor -> NOT bint_factor
             | int_expr compare_op int_expr
             | int_expr.

compare_op -> EQUAL | LT | GT | LE |GE.

int_expr -> int_expr addop int_term
          | int_term.

addop -> ADD | SUB.

int_term -> int_term mulop int_factor
          | int_factor.

mulop -> MUL | DIV.

int_factor -> LPAR expr RPAR
            | SIZE LPAR ID basic_array_dimensions RPAR
            | FLOAT LPAR expr RPAR
            | FLOOR LPAR expr RPAR
            | CEIL LPAR expr RPAR
            | ID modifier_list
            | IVAL
            | RVAL
            | BVAL
            | SUB int_factor.

modifier_list -> LPAR arguments RPAR
           | array_dimensions.

arguments -> expr more_arguments
           |.

more_arguments -> COMMA expr more_arguments
           |.
============================================================

The parse tree is horribly complex but we may actually represent the 
program in a considerably simpler structure.  There is a suggested 
Haskell datatype for the syntax tree:


data M_prog = M_prog ([M_decl],[M_stmt])
data M_decl = M_var (String,[M_expr],M_type)
            | M_fun (String ,[M_decl],M_type,M_prog)
            | M_arg (String, Int, M_type)
data M_stmt = M_ass (String,[M_expr],M_expr)
            | M_while (M_expr,M_stmt)
            | M_cond (M_expr,M_stmt,M_stmt) 
            | M_read (String,[M_expr])
            | M_print M_expr
            | M_return M_expr
            | M_block ([M_decl],[M_stmt])
data M_type = M_Int | M_Real | M_Bool 
data M_expr = M_ival Int
            | M_rval Real
            | M_bval Bool
            | M_id (String,[M_expr])
            | M_app (M_operation,[M_expr])
data M_operation =  M_fn String | M_add | M_mul | M_sub | M_div | M_neg
                | M_lt | M_le | M_gt | M_ge | M_eq | M_not | M_and | M_or
                | M_float | M_floor | M_ceil |M_size


Here is an m+- program:

============================================================
          var x[2]:int;      
          fun exp(b:int):int
          { var z:int;
            begin if b=0 then z:= 1
                  else z:= x[1] * exp(b-1);
           return z;
           end};
          begin
            read x[1]; 
            read x[2];
            print exp(x[2]);
          end
============================================================

Here is its syntax tree:

  M_prog
    ([M_var ("x",[2],M_int),
      M_fun
        ("exp",[M_arg("b",0,M_int)],M_int,
         M_prog
           ([M_var ("z",[],M_int)],
            [M_cond
               (M_app (M_eq,[M_id("b",[]),M_ival 0]),M_ass ("z",M_ival 1),
                M_ass
                  ("z",[],
                   M_app
                     (M_mul,
                      [M_id("x",[M_ival 1]),
                       M_app
                         (M_fn "exp",[M_app (M_sub,[M_id "b",M_ival 1])])]))),
             M_return (M_id("z",[])]))],
     [M_read("x",[M_ival 1]),M_read("y",[M_ival 2])
     ,M_print (M_app (M_fn "exp",[M_id("y",[M_ival 2])]))])


Finally let us briefly consider what the intermediate representation will be 
for the stack machine.  As mentioned before it will be essentially the 
same except each variable name will be replaced by a pair of integers 
(the levels from static declaration and the offset in the activation record) 
and each function call by the level from its static declaration and the 
label used for the call.

The new Haskell datatype may look like:

data I_prog  = IPROG    ([I_fbody],Int,[(Int,Int)],[I_stmt])
data I_fbody = IFUN     (string,[I_fbody],Int,[I_array],[I_stmt])
data I_array = IALLOC   (Int,[I_expr])
data I_stmt = IASS_V    (Int,Int,I_expr)
            | IASS_A    (Int,Int,I_expr,Iexpr)
            | IWHILE    (I_expr,I_stmt)
            | ICOND     (I_expr,I_stmt,I_stmt)
            | iREAD_F   (Int,Int)
            | iREAD_I   (Int,Int)
            | iREAD_B   (Int,Int)
            | iREAD_AF  (Int,Int,I_expr)
            | iREAD_AI  (Int,Int,I_expr)
            | iREAD_AB  (Int,Int,I_expr)
            | iPRINT_F  I_expr
            | iPRINT_I  I_expr
            | iPRINT_B  I_expr
            | iRETURN I_expr
            | iBLOCK    ([I_fbody],Int,[I_array],[I_stmt])
data I_expr = IINT      Int
            | IREAL     Real
            | IBOOL     Bool
            | IID_V     (Int,Int)          %  identifier for variable
            | IID_A     (Int,Int,I_expr)   %  identifier for array
            | IAPP      (I_opn,[I_expr])
data I_opn = ICALL      (String,Int)
           | IADD_F | IMUL_F | ISUB_F | IDIV_F | INEG_F
           | ILT_F  | ILE_F  | IGT_F  | IGE_F  | IEQ_F   % operations for floats
           | IADD | IMUL | ISUB | IDIV | INEG
           | ILT  | ILE  | IGT  | IGE  | IEQ 
           | INOT | IAND | IOR | IFLOAT | ICEIL |IFLOOR;

Notice that the declarations have gone (they are actually "used" in the 
construction of the symbol tables as they are built for use in the various 
parts of the tree) they are replaced by the bodies of the functions.

The functions are supplied with a label (to which the caller jumps),
the body is then the same as a program block.  Notice that the fact that there 
are arguments is now hidden in the code.  A program block indicates how many 
cells it is necessary to allocate for local storage and also how to calculate 
the storage required for all the arrays.  These are numbers needed in 
the generation of the entry and return code sequence of the function/block. 
It also, of course, contains the representation of the statements in the block.

The call of a function requires the label and the number of levels. 
While all variables are replaced by the offset and level calculated from 
the symbol table.

Notice also that all operations are distinuished by the data that they act 
on.  Thus, multiplication of reals/floats is distinguished from multiplication 
of integers, reading or printing integers is distinuished from reading or 
printing booleans or reals/floats.   We can do this because tyoe checking has 
resolved the type of each argument.


The intermediate representation is:

  IPROG
    ([IFUN
        ("fn1"
        ,[]
        ,1                                    % one local variable
        ,[]                                   % no arrays
        ,[ICOND(IAPP (IEQ,[IID_V (0,-4),IINT 0])
         ,IASS(0,1,IINT 1)
         ,IASS(0,1,IAPP(IMUL,[IID_A (1,1,IINT 2)
                             ,IAPP(ICALL ("fn1",1),[IAPP(ISUB,[IID_V (0,-4)
                                                             ,IINT 1])])])))
         ,IRETURN_V (IID_V (0,1))
         ])]
    ,1           % one local variable which is an array
    ,[(1,[IINT 2])]
    ,[IREAD_AI (0,1,IINT 2)
     ,IREAD_AI (0,2,IINT 3)
     ,IPRINT_I (IAPP (ICALL ("fn1",0),[IID_A (0,2,IINT 2)]))
     ])

>From here it is relatively easy to generate the required stack machine code.

