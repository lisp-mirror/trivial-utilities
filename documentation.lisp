;;;; Copyright (c) Eric Diethelm 2018 <ediethelm@yahoo.com>
;;;; This code is licensed under the MIT license.

(in-package :trivial-utilities)

(defsection @trivial-utilities-manual (:title "Trivial Utilities Manual")
  "[![pipeline status](https://gitlab.com/ediethelm/trivial-utilities/badges/master/pipeline.svg)](https://gitlab.com/ediethelm/trivial-utilities/commits/master)
[![Quicklisp](http://quickdocs.org/badge/trivial-utilities.svg)](http://quickdocs.org/trivial-utilities/)"
  
  (@trivial-utilities-description section)
  (@trivial-utilities-installing section)
  (@commonly-used-types section)
  (@from-onlisp section)
  (@from-pcl section)
  (@other-useful-stuff section)
  (@trivial-utilities-license section)
  (@trivial-utilities-contributing section))

(defsection @trivial-utilities-description (:title "Description")
  "A collection of useful types, functions and macros.")

(defsection @trivial-utilities-installing (:title "Installing trivial-utilities")
  "This project is available in the latest [QuickLisp](https://www.quicklisp.org/beta/ \"QuickLisp\") distribution, so installing it is reduced to calling:

```lisp
(ql:quickload :trivial-utilities)
```

But if you want access to the latest updates, install it by cloning the Git repository with

```bash
cd $HOME/quicklisp/local-projects
git clone https://gitlab.com/ediethelm/trivial-utilities.git
```

and then loading it as usual via [QuickLisp](https://www.quicklisp.org/beta/ \"QuickLisp\") as above.
")

(defsection @commonly-used-types (:title "Commonly used types")
  (negative-fixnum type)
  (non-positive-fixnum type)
  (non-negative-fixnum type)
  (positive-fixnum type))

(defsection @from-onlisp (:title "From Graham's On Lisp")
  (aif macro)
  (awhen macro)
  (aunless macro)
  (aprog1 macro)
  (blambda macro)
  (alambda macro)
  (flatten generic-function)
  (flatten (method () (t)))
  (mkstr function)
  (symb function)
  (single function)
  (mklist function))

(defsection @from-pcl (:title "From Seibel's Practical Common Lisp")
  (once-only macro))

(defsection @other-useful-stuff (:title "Other useful stuff")
  (partition function)
  "  
Example usage of *PARTITION*  

```lisp
(partition #'evenp '(1 2 3 4 5 6))
((2 4 6) (1 3 5))
```"
  (demultiplex function)
  "  
Example output for *DEMULTIPLEX*  

```lisp
(demultiplex '((a b c) (x) (1 2)))
=> ((A X 1) (A X 2) (B X 1) (B X 2) (C X 1) (C X 2))
```
"

  (merge-lambda-lists function)
  (equals generic-function)
  (equals (method () (t t)))
  (equals (method () (null null)))
  (equals (method () (list list)))
  (equals (method () (string string)))
  (clone generic-function)
  (all-slots function)
  (collect-persistent-slots function)
  (extract-additional-keys macro)
  (make-doc-updater macro)
  (update-doc generic-function))


(defsection @trivial-utilities-license (:title "License Information")
  "This library is released under the MIT License. Please refer to the [LICENSE](https://gitlab.com/ediethelm/trivial-utilities/blob/master/LICENSE 'License') to get the full licensing text.")

(defsection @trivial-utilities-contributing (:title "Contributing to this project")
  "Please refer to the [CONTRIBUTING](https://gitlab.com/ediethelm/trivial-utilities/blob/master/CONTRIBUTING 'Contributing') document for more information.")

(make-doc-updater :trivial-utilities @trivial-utilities-manual)
