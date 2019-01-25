CPSC411 -- Datatypes for M+


Author: Robin Cockett
Date: Feb , 2002

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Louden: 5.5 
Dragon book: 5.2 (287-293)

The practice of compiler writing has changed greatly from the early days 
of computing when limited storage and speed were ever present 
constraints.  Under these constaints it was important to attempt to compile 
programs in one pass.  This constraint had a knock-on effect both on the
design of programming languages and what it was felt could reasonably 
be attempted by a compiler.  

It also led to a number of interesting but, perhaps now, less revelent 
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
to the left.  Furthermore, the abstract syntax allows BOTH assocations
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
one sorts out the scope issues associted to variables.  One also detects 
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
biggest factor in an optimizing the compilation.  On the otherhand, if we 
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

Consider the language m+ defined by 

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

The parse tree is horribly complex but we may actually represent the 
program in a considerably simpler structure.  There is a suggested 
Haskell datatype for the syntax tree:


data M_prog = M_prog ([M_decl],[M_stmt])
data M_decl = M_var (string,M_type)
            | M_fun (string ,[(string,M_type)],M_type,M_prog)
data M_stmt = M_ass (string,M_expr)
            | M_while (M_expr,M_stmt)
            | M_cond (M_expr,M_stmt,M_stmt) 
            | M_read string
            | M_print M_expr
            | M_return M_expr
            | M_block ([M_decl],[M_stmt])
data M_type = M_int | M_bool
data M_expr = M_num Int
            | M_bl Bool
            | M_id String
            | M_app (M_operation,[M_expr])
data M_operation =  M_fn String | M_add | M_mul | M_sub | M_div | M_neg
                | M_lt | M_le | M_gt | M_ge | M_eq | M_not | M_and | M_or


Here is an M+ program:

============================================================
          var x:int;
          var y:int;
          fun exp(b:int):int
          { var z:int;
            begin if b=0 then z:= 1
                  else z:= x * exp(b-1);
           return z;
           end};
          begin
            read x; 
            read y;
            print exp(y);
          end
============================================================

Here is its syntax tree:

  M_prog
    ([M_var ("x",M_int),M_var ("y",M_int),
      M_fun
        ("exp",[("b",M_int)],M_int,
         M_prog
           ([M_var ("z",M_int)],
            [M_cond
               (M_app (M_eq,[M_id "b",M_num 0]),M_ass ("z",M_num 1),
                M_ass
                  ("z",
                   M_app
                     (M_mul,
                      [M_id "x",
                       M_app
                         (M_fn "exp",[M_app (M_sub,[M_id "b",M_num 1])])]))),
             M_return (M_id "z")]))],
     [M_read "x",M_read "y",M_print (M_app (M_fn "exp",[M_id "y"]))])


Finally let us briefly consider what the intermediate representation will be 
for the stack machine.  As mentioned before it will be essentially the 
same except each variable name will be replaced by a pair of integers 
(the levels from static declaration and the offset in the activation record) 
and each function call by the level from its static declaration and the 
label used for the call.

The new Haskell datatype may look like:

data I_prog  = IPROG    ([I_fbody],Int,[I_stmt])
data I_fbody = IFUN     (string,[I_fbody],Int,[I_stmt])
data I_stmt = IASS      (Int,Int,I_expr)
            | IWHILE    (I_expr,I_stmt)
            | ICOND     (I_expr,I_stmt,I_stmt)
            | iREAD_I   (Int,Int)
            | iPRINT_I  I_expr
            | iREAD_B   (Int,Int)
            | iPRINT_B  I_expr
            | iRETURN   I_expr
            | iBLOCK    ([I_fbody],Int,[I_stmt])
data I_expr = INUM      Int
            | IBOOL     Bool
            | IID       (Int,Int)
            | IAPP      (I_opn,[I_expr])
data I_opn = ICALL      (String,Int)
           | IADD | IMUL | ISUB | IDIV | INEG
           | ILT  | ILE  | IGT  | IGE  | IEQ 
           | INOT | IAND | IOR;

Notice that the declarations have gone (they are actually "used" in the 
construction of the symbol tables as they are built for use in the various 
parts of the tree) they are replaced by the bodies of the functions.

The functions are supplied with a label (to which the caller jumps),
the number of arguments it expects and the code of the body which is 
a program block.  A program block indicates how many cells it is 
necessary to allocate for local storage.  These are numbers needed in 
the generation of the entry and return code sequence of the function.

The call of a function requires the label and the number of levels. 
While all variable are replaced by the offset and level calculated from 
the symbol table.


The intermediate representation is:

  IPROG
    ([IFUN
        ("fn1"
        ,[]
        ,1
        ,[ICOND(IAPP (IEQ,[IID (0,-4),INUM 0])
         ,IASS(0,1,INUM 1)
         ,IASS(0,1,IAPP(IMUL,[IID (1,1)
                             ,IAPP(ICALL ("fn1",1),[IAPP(ISUB,[IID (0,-4)
                                                             ,INUM 1])])])))
         ,IRETURN (IID (0,1))
         ])]
    ,2
    ,[IREAD_I (0,1)
     ,IREAD_I (0,2)
     ,IPRINT_I (IAPP (ICALL ("fn1",0),[IID (0,2)]))
     ])

From here it is relatively easy to generate the required stack machine code.
