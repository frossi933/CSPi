# CSPi: Communicating Sequential Processes Interpreter

## Overview

University project for Programming Language Analsis and Formal Construction of Programs.
Design and interpretation of DSL for specification in CSP Language. Interpreter written in Haskell and correctness 
demonstrations using the Coq system.


## Formal proof
https://github.com/frossi933/CSPi/blob/master/article/article.pdf

## Running

Go to haskell directory
```
cd haskell/
```
Run the following script
```
./runCSP.sh
```
It will prompt a command line. There you can type "help" to obtain more information.
Then you have to load both specification and implementation files (No matter the order).
To do that the commands will be:
```
:> loadSpec path/to/spec/file
:> loadImp path/to/imp/file
```
The implementation file is a Haskell module called "Imp".
After that, you have to compile the program:
```
:> compile
```  
And finally, you can run it with
```
:> run
```  
This will open a new terminal where the program is executed.
