CPSC411 -- The symbol table

Author: Robin Cockett
Date: March 10, 2002

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Dragon book: Symbol Tables 7.6 (429-440)

A symbol table is for storing the information associated with the various 
symbols used in the language.  It is the main (inherited) attribute after 
the parse stage which is used in creating the intermediate representation.
The symbol table can also be useful in the lexical analysis stage to 
disambiguate tokens; this in turn can make the parsers job considerably 
easier.

There are three main functions a symbol table must implement:

         insert  -  inserting a newly introduced variable or function 
                    name into the table with its required attributes.
         lookup  -  looking up the attributes associated with a name.
         delete  -  removing items from the symbol table whose declarations
                    are no longer visible.


Basic symbol table
------------------

A basic implementation of a symbol table is as a linear list.  Each item of 
the list is a pair: the name (a string) and an associated attribute (the 
type of the symbol and any other attributes one may need to know for code 
generation).

This is a simple approach with a constant time insertion but a lookup 
(and possibly deletion) time in the worst case proportional to the number 
of elements in the list.   

More sophisticated implementations may use binary trees, AVL-trees, or 
(most usually) hash tables to improve the look up time.  If you use a hash 
table ensure that the hashing function is suitable for the sorts of variable 
names which are used in programming languages (or else you will get an 
abnormal number of collisions).

In addition it may be useful to have a header for the symbol table where 
attributes of the whole table can be stored (for example the current free 
offset, what it is the symbol table for (e.g. main, function, or block).


Handling Scope
--------------

When a variable is declared that delaration is only visible in the 
body associated with the declaration.  Furthermore, a declaration 
in a subblock of a symbol with the same name as a symbol decared in an 
enclosing block will supercede the original meaning of the symbol in 
the enclosing block.  Thus, the inner declaration punches a hole in the 
scope of the inner declaration. A symbol table should be able to handle 
these scope issues.  In particular, as one leaves the scope of a 
definition one should be able to "delete" the symbols introduced in 
that inner scope easily.

There are several approaches to implementing this, here we outline two:

(a) Stacking values in the same symbol table:  each name in the symbol table 
has a list of attribute values associated with it.  When a new block is 
entered the attributes of the symbol defined are pushed on top of any 
existing attribute values, thus, hiding them.

When the scope of a block is left it is necessary to remove all the 
declarations introduced in that block.  This means that the introduced 
declarations of a block must be marked in some manner so that they can 
be removed.  A simple scheme is to associate with each set of attributes 
(associated with a name) a level number.  The deletion is then of a level.
The disadvantage of this scheme is that one must then traverse the entire 
symbol table to delete a level.  This is rather expensive!

A more efficient alternative is to chain the introduced symbols together 
(by pointers) and then remove them by traversing this chain.  Here the 
symbol table also needs a stack of starts for these chains as an extra 
structure.  

(b)  Stacking symbol tables:  the other approach is to simply open up
a new symbol table when one enters a block.  To find a symbol then involves 
seaching through the symbol tables to find the "highest" level in which 
the symbol appears.  This is not very efficient ... on the otherhand 
deletion, the removal of a level of scope is efficient.

The great advantage of (b) is that it is simple.  Its (significant) 
disadvantage is that look up is slow.  For a full blown compiler 
it may well be worth the extra effort to develop a symbol table of 
type (a) with an efficient deletion.  Note, however, you will need to 
solve all the other issues of offset calculation etc.


Example:  

Consider the following program fragment:

     var x:int;
     fun f(y:int, z:int):int
         { var x:int;
           fun g(a:int,b:int):int
               { var y:int;
                 begin
                    ...  [a:= f(x,v)] ...
                 end
               }
            begin  ... end
         };
     var v:int;
     begin 
           ....
     end;


We illustrate what a symbol table of type (b) should look like when 
we are trying to generate code for the assignment [a:= f(x,v)].  This 
symbol table is built by first building the symbol table for main, 
next the symbol table for f and finally for g:

    [ symbol_table(1,2,[ var_attr(a,int,-5)
                       , var_attr(b,int,-4)
                       , var_attr(y,int,1)])
    , symbol_table(1,2,[ var_attr(y,int,-5)
                       , var_attr(z,int,-4)
                       , var_attr(x,int,1)
                       , fun_attr(g,([int,int],int),code_label_g)])
    , symbol_table(2,0,[ var_attr(x,int,1)
                       , fun_attr(g,([int,int],int),code_label_f)
                       , var_attr(v,int,2)])
    ]

Now when we look up "a" in this symbol table we find it immediately.  
Because it is an argument of the function it has a negative offset 
(it occurs before the frame pointer).  In this way with each symbol 
we may associate a level, with each variable an offset, and with each
function a code label:

              name     level    type              attribute 
              ---------------------------------------------
              "a"       0        int               -5 offset
              "f"       2    ([int,int],int)       code_label_f
              "x"       1        int                1 offset
              "v"       2        int                2 offset

These attributes can then be used in the generation of code.


Programming a symbol table in SML
=================================

While you will not be programming the M+ symbol table in sml it is quite 
instructive to see how it is done.  One particularly nice feature of 
sml is its module system which allows one to specify interfaces before one 
writes the program.  This allows one to provide a simple, although perhaps 
inefficient, implementation and later return to implement a more efficent 
symbol table which can be substituted without affecting the rest of the 
program.

Below is an almost complete implementation of a symbol table for m+ (the 
label generating functions are missing)!!

The two main operations we want to be able to perform on a symbol table 
are the insertion and lookup.  The deletion will actually take care of itself 
in this simple design through the way that sml handles data.  

Insertion:
----------

We first need to think about the form of the things we will wish to insert 
in the symbol table.  The following datatype describes what we will be 
allowed to insert:

datatype SYM_DESC = ARGUMENT of string * M_type 
                  | VARIABLE of string * M_type
                  | FUNCTION of string * M_type list * M_type;

M_type is the datatype of m+ types (essentially int or bool).

This distinguishes three types of thing we may want to insert
      (1) an argument with its type
      (2) a (local) variable with its type
      (3) a (local) function with its type
in each case we insert a string with its type, however, the symbol table 
will treat the cases rather differently.  For example in the case of a 
function insertion the symbol table must create a label (for the jump).  
In the case of an argument insertion the offset calculation is completely 
different from a variable insertion.  We will return to this when we discuss 
the implementation.

The insertion function will have the following type:

    insert: ST * SYM_DESC -> ST

where ST is the type of the symbol table.  It we try to insert an already 
locally declared symbol into the symbol-table then an error will be raised.  
This is handled through sml's exceptions.

Lookup:
-------

When we do a look up we expect back certain information: again I will create 
a special datatype for the returned information so that I know exactly what 
information is to be passed back:

datatype SYM_I_DESC = I_VARIABLE of int * int * M_type
                    | I_FUNCTION of int * string * M_type list * M_type;

Here there are only two variants 
         (1) the symbol is a variable 
                 I_VARIABLE(level, offset, type)
             it was found at level <level> in the symbol table and is at 
             offset <offset> from the frame pointer of that activation record.
             The variable is of type <type>.
         (2) the symbol is a function
                 I_FUNCTION(level,label,arg_type,type)
             it was found at level <level>, the label to which you must jump 
             is <label>.  The types of its arguments are <arg_type> and the 
             type of its returned value is <type>.

Again if anything goes wrong I will raise an exception!  This means that a 
retrieval is a function of the following type:

   lookup: ST * string -> SYM_I_DESC

Scope change and the empty symbol table stack:
---------------------------------------------

We have to supply two further functions in the sml implementation:

   empty: unit -> ST
   new_scope: ST -> ST

The first  of these functions simply supplies an empty symbol table 
while the second starts a new symbol table "scope level".

In a C implemenentation you may need a further function to remove 
a scope level:

    remove_scope: ST -> ST


The signature of the symbol table
---------------------------------

Collecting this together we have an an abstract datatype which is a symbol 
table.  This datatype has the above mentioned functions which allow one 
to manipulate the contents.  In SML we may represent this as a signature:

signature SYMBOL_TABLE =
sig 
   exception SYM_ERROR of string
   type ST
   val empty: unit -> ST
   val new_scope: ST -> ST
   val remove_scope: ST -> (ST * int)
   val insert: ST * SYM_DESC -> ST
   val lookup: ST * string -> SYM_I_DESC
end

Here ST is the abstract datatype of the stack.  As things can go wrong we 
allow some exceptions to be raised.  Finally we need the functions 
we have described above to manipulate the symbol table.


The implementation:
-------------------

The last step is to implement this signature.  In SML we use a structure
to do this:

structure simple_st:SYMBOL_TABLE =
struct
    ....
end;

where inside the structure we place the code for the various required 
functions.  The structure only "exports" those functions (and types) 
mentioned in the signature. 

(a) Internal and exported datatypes:
    -------------------------------

The implementation of the symbol table uses datatypes which are not 
exported to the outside world.  They might look like:

   exception SYM_ERROR of string

   datatype SYM_VALUE = var_attr of int * M_type
                      | fun_attr of string * M_type list * M_type;

   datatype SYM_TABLE = symbol_table of int * int * ((string * SYM_VALUE) list);

   type ST = SYM_TABLE list;

Here there is an "exception" (which is exported).  A type of symbol table value 
(which is private): this can be either the attributes of a function or of 
a variable.  For a variable we hold its offset (which can be negative) and 
its type.  For a function we hold its label and its type.  

A symbol table level is essentially a list of these values paired with 
strings together with two integers to keep track of the positive and 
negative offsets.  The first integer is the number of local variables 
inserted into the table. The next local variable inserted must 
increment this value and take this as its offset.  The second integer is the 
number of arguments: the next argument inserted must increment this value 
and take as it negative offset three more than this incremented value.    
Note that the arguments must be inserted in reverse order into this symbol 
table.  Also remember that arguments occur BEFORE the frame pointer so 
will be accessed by negative offsets.

A symbol table is a list of symbol table levels and this (as an abstract 
type) is exported to the outside world.


(b) The  empty symbol table and adding a level:
    -------------------------------------------

These are the two simplest functions they are:

   val new: unit -> ST = fn () => [];

   val new_scope: ST -> ST = fn s => (symbol_table(0,0,[]))::s;

(b) Lookup:
    -------

The next simplest function is the lookup.  Here we look down through the levels 
until we find the symbol: 

   val lookup: ST * string -> SYM_I_DESC = fn (s, x) => 

    let fun found (Level, var_attr(Offset,Type)) 
                    =  I_VARIABLE(Level,Offset,Type)
          | found (Level, fun_attr(Label,Arg_Type,Type)) 
                    = I_FUNCTION(Level,Label,Arg_Type,Type);

        fun find_level ((str,v)::rest) = if x=str then v
                                         else find_level rest
          | find_level [] = raise SYM_ERROR x

        fun find n [] =  raise SYM_ERROR 
                    ("Symbol table error: symbol "^x^" not found.\n")
          | find n (symbol_table(_,_,vs)::rest) =(found(n,(find_level vs)))
                 handle SYM_ERROR _ =>  find (n+1) rest

    in find 0 s end;

The subsiduary function "found" simply converts the data retrieved into the form 
required by the lookup from the internal form.  "find_level" searches a level for 
the symbol while "find" searches the levels in turn until the symbol is found 
raising an exception when it is not found.


(c)  Insertion:

When we insert information we must generate the correct offsets and for 
function symbols a new label:


   val insert: ST * SYM_DESC -> ST = 
    fn (s,d) => case s of 
      [] => raise SYM_ERROR 
            "Symbol table error: insertion before defining scope."
    | ((symbol_table(nL,nA,sL))::rest) =>
          (case d of 
             ARGUMENT(str,T) 
              => if (in_index_list str sL) then raise SYM_ERROR 
                ("Symbol table error: "^str^"is already defined.")
                 else symbol_table(nL,nA+1
                             ,(str,var_attr(~(nA+4),T))::sL)
           | VARIABLE (str,T) 
              => if (in_index_list str sL) then raise SYM_ERROR 
                ("Symbol table error: "^str^"is already defined.")
                 else symbol_table(nL+1,nA
                             ,(str,var_attr(nL+1,T))::sL)
           | FUNCTION (str,Ts,T)
              => if (in_index_list str sL) then raise SYM_ERROR 
                ("Symbol table error: "^str^"is already defined.")
                 else symbol_table(nL,nA
                             ,(str,fun_attr(getlabel "fn",Ts,T))::sL)
          )::rest;  
 
                              
where 

fun in_index_list str [] = false
  | in_index_list str ((x,_)::xs) = if str=x then true
                                   else in_index_list str xs;


Removing scope:
--------------

This simply removes a scope level and returns the number of local variables 
in that level together with the modified symbol table.


Problems with blocks!!!
-----------------------

This code is quite useable in SML it does not handle blocks very well 
as for these we do not want to add a new activation record but we do need 
to be able to add the block decarations to the current activation record.

To accomplish this I suggest one adds a little more functionality to 
your stack of symbol tables so that you distinguish between block levels 
and function levels.  This can be achieved by adding an extra tag to the 
to the symbol tables to distinguish these two possibilities.  One then 
modifies level counting so that only non-block levels are counted.  
Also for blocks the offset is calculated slightly differently taking into 
account the fact that the offset continues from the level underneath.

To achieve this we must alter the signature slightly: 

signature SYMBOL_TABLE =
sig 
   exception SYM_ERROR of string
   type ST
   val empty: unit -> ST
   val new_fun_scope: ST -> ST
   val new_block_scope: ST -> ST
   val remove_scope: ST -> (ST * int)
   val insert: ST * SYM_DESC -> ST
   val lookup: ST * string -> SYM_I_DESC
end

So that block scopes and function scopes can be created.


 






 

