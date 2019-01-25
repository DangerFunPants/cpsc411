###     Note
For best viewing, use a Markdown renderer like ReText.

CPSC 411 - Assignment Four
==========================

##Application Specifications
The application provided implements the semantic checking and IR generation portions of a compiler for the M++ Language defined in the [MSpec Docment](http://pages.cpsc.ucalgary.ca/~robin/class/411/M+/Mspec.pdf). 

##Current Drawbacks
* The provided version of this applcation does not provide support for nested multiline comments as this involves quite a bit of "Hand-Hacking" ontop of the Alex files when using BNFC to generate specifications for the Lexer and Parser generators. 
* The application does not currently print references to locations in the source code where semantic errors were detected. This could be easily accomplished using the BNFC position token feature. 

##Building the application
To build the application, type the following command in the root directory of the project: 

    make

Invoking the above command will execute a number of phases involved in the compilation of the application: 

1. Invoke BNFC in order to generate the Alex and Happy files.
2. Execute the makefile in order to generate the Haskell source files resultant from the generated Alex and Happy files.
3. Invoke GHC in order to compile the resultant application and place the binary into the root directory of the project. 

##Running the Application
The application (mcc) reads input files from stdin and prints the resulting Intermediate Representation to stdout. In the case that there is an error message the application will print the resulting error message to stderr. The application can be invoked on the command line using the following syntax: 

`./mcc < /path/to/input/file.m+`

##Plumbing Diagrams
A subset of the plumbing diagrams are included in the `./plumbing_diag/` directory. The included diagrams exemplify the processing of Literals, Identifiers, Binary Operator Application, Function Declaration and Assign Statements. 