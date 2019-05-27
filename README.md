# Trivial Utilities Manual

###### \[in package TRIVIAL-UTILITIES\]
[![pipeline status](https://gitlab.com/ediethelm/trivial-utilities/badges/master/pipeline.svg)](https://gitlab.com/ediethelm/trivial-utilities/commits/master)
[![Quicklisp](http://quickdocs.org/badge/trivial-utilities.svg)](http://quickdocs.org/trivial-utilities/)

## Description

A collection of useful types, functions and macros.

## Installing trivial-utilities

This project is available in the latest [QuickLisp](https://www.quicklisp.org/beta/ "QuickLisp") distribution, so installing it is reduced to calling:

```lisp
(ql:quickload :trivial-utilities)
```

But if you want access to the latest updates, install it by cloning the Git repository with

```bash
cd $HOME/quicklisp/local-projects
git clone https://gitlab.com/ediethelm/trivial-utilities.git
```

and then loading it as usual via [QuickLisp](https://www.quicklisp.org/beta/ "QuickLisp") as above.

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

    Similar to the normal *IF*, but binds the result of *TEST* via *LET* to *IT*.

- [macro] AWHEN TEST &BODY THEN

    Similar to the normal *WHEN*, but binds the result of *TEST* via *LET* to *IT*.

- [macro] AUNLESS TEST &BODY ELSE

    Similar to the normal *UNLESS*, but binds the result of *TEST* via *LET* to *IT*.

- [macro] APROG1 FIRST &BODY BODY

    Similar to the normal *PROG1*, but binds the result of *FIRST* via *LET* to *IT* and returns *IT*.

- [macro] BLAMBDA PARMS &BODY BODY

    Similar to *LAMBDA*, but defines an unnamed *BLOCK* from which one can *RETURN*.

- [macro] ALAMBDA PARMS &BODY BODY

    Similar to *LAMBDA*, but defines *SELF* via *LABELS* which contains the *BODY*. *SELF* can be referenced in *BODY*.

- [generic-function] FLATTEN OBJ

    Flattens the structure of an object.
    
    An implementation for *LIST*s already exists. Add specific implementations for special objects.

- [method] FLATTEN OBJ

    Returns a list with the flatten contents of the *LIST* *OBJ*.

- [function] MKSTR &REST ARGS

    Creates a string out of all elements in *ARGS*. Each element must be printable per *PRINC*.

- [function] SYMB &REST ARGS

    Creates a new and internalized *SYMBOL*, named by applying *MKSTR* to *ARGS*.

- [function] SINGLE LST

    Predicate verifying that *LST* is a list containing exactly one element.

- [function] MKLIST OBJ

    Returns a new list containing *OBJ*, or *OBJ* itself if it is already a list.

## From Seibel's Practical Common Lisp

- [macro] ONCE-ONLY (&REST NAMES) &BODY BODY

    Protects against multiple evaluation of *NAMES* during macro-expansion. Usualy used inside of other macros.

## Other useful stuff

- [function] PARTITION PRED SEQ

    Separates the contents of *SEQ* into two lists, depending on the result of *PRED* when applied to each element of *SEQ*.

Example usage of *PARTITION*  

```lisp
(partition #'evenp '(1 2 3 4 5 6))
((2 4 6) (1 3 5))
```


- [function] DEMULTIPLEX INPUT &OPTIONAL (BINDINGS NIL)

    Given a *LIST* of *LIST*s returns all combinations of the elements in each of the *LIST*s in *INPUT*.

Example output for *DEMULTIPLEX*  

```lisp
(demultiplex '((a b c) (x) (1 2)))
=> ((A X 1) (A X 2) (B X 1) (B X 2) (C X 1) (C X 2))
```


- [function] MERGE-LAMBDA-LISTS &REST LISTS

    Given two lambda lists, return one representing the unification of both.

- [generic-function] EQUALS OBJ1 OBJ2 &KEY &ALLOW-OTHER-KEYS

    A generic equality comparison function.

- [method] EQUALS OBJ1 OBJ2

    Default method if all other cases fail. Uses cl:equal for comparison.

- [method] EQUALS (OBJ1 NULL) (OBJ2 NULL)

    Equality comparison of two *NULL* symbols always results in *NIL*.

- [method] EQUALS (OBJ1 LIST) (OBJ2 LIST)

    Equality comparison of two *LIST*s requires both to have the same length and each element to be equaly comparable (*EQUALS*).

- [method] EQUALS (OBJ1 STRING) (OBJ2 STRING)

    Equality comparison of two *STRING*s falls back to *string=*.

- [generic-function] CLONE OBJ &KEY &ALLOW-OTHER-KEYS

    A generic function to clone objects.

- [function] ALL-SLOTS CLASS

    Create a list containing all slots defined for *class*, including slots defined (recursively) in all superclasses.

- [function] COLLECT-PERSISTENT-SLOTS INSTANCE &KEY SLOTS-TO-IGNORE

    Any slot which defines :initarg, is bound to a value and is not listed in *slots-to-ignore* will be appended in the form (initarg value).

- [macro] EXTRACT-ADDITIONAL-KEYS (KEYS ARGS) &BODY BODY

    Convenience macro to extract keywords including default values from a plist, such as (:a 1 :b 2 :c 3). Usually used in conjunction with (&rest keys &key &allow-other-keys).  
    *KEYS* - The keys to be extracted. This follows the syntax of **let**. Example: ((a nil) b (c t c-was-in-args-p))  
    *ARGS* - A property list of keyword-value pairs  
    *BODY* - Code to be executed and which uses the extracted keys.

- [macro] MAKE-DOC-UPDATER COMPONENT DOC-ROOT-SECTION

    A macro to generate a documentation updating method specialized for *COMPONENT*. *DOC-ROOT-SECTION* indicates the MGL-PAX root section.

- [generic-function] UPDATE-DOC COMPONENT &OPTIONAL FILE-NAME

    The generic function used to update the documentation of a project identified by *COMPONENT*. *FILE-NAME* defaults to "README.md".

## License Information

This library is released under the MIT License. Please refer to the [LICENSE](https://gitlab.com/ediethelm/trivial-utilities/blob/master/LICENSE "License") to get the full licensing text.

## Contributing to this project

Please refer to the [CONTRIBUTING](https://gitlab.com/ediethelm/trivial-utilities/blob/master/CONTRIBUTING "Contributing") document for more information.
