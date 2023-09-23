#PF23-lang#

Implementation of a programming language inspired by functional programming using OCaml. The syntax is based on Forth and PostScript, which means it is stack-based.

##TYPES##
The following types are supported: INT, STRING, BOOL.

##BASIC OPERATION##
Basic primitive functions include: +, -, /, *, =, <, >, <>.

Furthermore, some stack operations include: SWAP (swaps the top 2 elements of the stack), DROP (removes the element at the top of the stack), DUP (duplicates the top of the stack), ROT (rotates three elements at the top of the stack).

Likewise, we provide support for I/O functions such as: PRINT (displays and removes the top element of the stack) and SCAN. A simple example of integer addition would be:

	SCAN SCAN + PRINT 

Moreover, we support the following control structures: <condition> IF __ ELSE __ ENDIF as well as <condition> IF __ ENDIF.

Finally, to declare a new variable or function, one must follow the following format:

	: NOM [instructions]; 

For example: 

	: FIB DUP 1 < IF DROP 0 ELSE DUP 1 = IF ELSE DUP 1 - FIB SWAP 2 - FIB + THEN THEN; SCAN FIB PRINT


##COMPILATION##

    - You can execute "make" or "ocamlopt -o pf23 main.ml"

##EXECUTION## 

    - ./pf23 filename.pf 
        for evaluating the file filename.pf 
    - ./pf23
        for line-by-line interpretation from the terminal
    - ./pf23 test
        to run the test suite
    - Entire statements can be written in the line-by-line evaluation, for example:
        ex
            : FIB DUP 1 < IF dRop 0 else DUP 1 = if Else DUP 1 - FIB SWAP 2 - FIB + THEN THEN ; SCAN FIB PRINT
