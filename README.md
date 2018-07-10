# Trivial Utilities Manual

###### \[in package TRIVIAL-UTILITIES\]
A collection of useful types, functions and macros.

## Commonly used types

- [type] NEGATIVE-FIXNUM

    Defines all negative integer types upto and including -1.

- [type] NON-POSITIVE-FIXNUM

    Defines all non-positive integer types upto and including 0.

- [type] NON-NEGATIVE-FIXNUM

    Defines all non-negative integer types starting from and including 0.

- [type] POSITIVE-FIXNUM

    Defines all positive integer types starting from and including 1.

## From Graham's On Lisp

- [macro] AIF TEST THEN &OPTIONAL ELSE

    Similar to the normal IF, but binds the result of TEST via LET to IT.

- [macro] AWHEN TEST &BODY THEN

    Similar to the normal WHEN, but binds the result of TEST via LET to IT.

- [macro] AUNLESS TEST &BODY ELSE

    Similar to the normal UNLESS, but binds the result of TEST via LET to IT.

- [macro] APROG1 FIRST &BODY BODY

    Similar to the normal PROG1, but binds the result of FIRST via LET to IT and returns IT.

- [macro] BLAMBDA PARMS &BODY BODY

    Similar to LAMBDA, but defines an unnamed BLOCK from which one can RETURN.

- [macro] ALAMBDA PARMS &BODY BODY

    Similar to LAMBDA, but defines SELF via LABELS which contains the BODY. SELF can be referenced in BODY.

- [generic-function] FLATTEN OBJ

    Flattens the structure of an object.
    
    An implementation for LISTs already exists. Add specific implementations for special objects.

- [method] FLATTEN OBJ

    Returns a list with the flatten contents of the LIST OBJ.

- [function] MKSTR &REST ARGS

    Creates a string out of all elements in ARGS. Each element must be printable per PRINC.

- [function] SYMB &REST ARGS

    Creates a new and internalized SYMBOL, named by applying MKSTR to ARGS.

- [function] SINGLE LST

    Predicate verifying that LST is a list containing exactly one element.

- [function] MKLIST OBJ

    Returns a new list containing OBJ, or OBJ itself if it is already a list.

## From Seibel's Practical Common Lisp

- [macro] ONCE-ONLY (&REST NAMES) &BODY BODY

    Protects against multiple evaluation of NAMES during macro-expansion. Usualy used inside of other macros.

## Other useful stuff

- [function] PARTITION PRED SEQ

    Separates the contents of SEQ into two lists, depending on the result of PRED when applied to each element of SEQ.

    Example usage of PARTITION  

    ```lisp
    (partition #'evenp '(1 2 3 4 5 6))  
    => ((2 4 6) (1 3 5))
    ```

- [function] DEMULTIPLEX INPUT &OPTIONAL (BINDINGS NIL)

    Given a LIST of LISTs returns all combinations of the elements in each of the LISTs in INPUT.

    Example output for DEMULTIPLEX  

    ```lisp
    (demultiplex '((a b c) (x) (1 2)))  
    => ((A X 1) (A X 2) (B X 1) (B X 2) (C X 1) (C X 2))
    ```

## License Information

This library is released under the MIT License. Please refer to the [LICENSE](https://gitlab.com/ediethelm/trivial-utilities/blob/master/LICENSE "License") to get the full licensing text.

## Contributing to this project

Please refer to the [CONTRIBUTING](https://gitlab.com/ediethelm/trivial-utilities/blob/master/CONTRIBUTING "Contributing") document for more information.
