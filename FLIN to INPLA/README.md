Notes on FLIN implementation

2 versions:
1. Base language - no attributes or syntactic sugar.
2. Extended - attributes (integers only).

These are rough prototypes!
In particular, error checking is minimal: syntactically incorrect FLIN code may be translated (e.g. non-linear variable usage) and INPLA will then refuse it.

These are purely transpilers.
INPLA is required to evaluate terms (nets); go to https://github.com/inpla/inpla/tree/main for code and instructions.

How to use:
These are intended to be run via ghci, but can be compiled and run separately.

Download a set of files and, in download directory enter "ghci Trans <filename.txt>" where <filename.txt> is the name of the file with FLIN function definitions.
Examples are included in the downloads.
Note that there cannot be empty lines in the file, including at the end.
Comments are per line and start with "--".
Use variable names at the end of the alphabet - x,xs,y etc as internally FLIN uses r,s,t and so on.
Trans first outputs the FLIN function definitions and the INPLA translated versions which should be cut & paste into INPLA.
Then can enter FLIN terms which are translated to INPLA.
Cut and paste this into INPLA for evaluation.

Note that functions must be defined before they can be used in other functions.
e.g. add(Z,y)=y ; add(S(x),y)=S(add(x,y)) is OK but not the other way round.
If need mutually defined functions, create a "dummy" one which has correct number of outputs.
