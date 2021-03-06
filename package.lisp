(uiop:define-package #:trivial-utilities
  (:documentation "trivial-utilities is a collection of useful functions and macros.")
  (:use #:common-lisp)
  (:export #:negative-fixnum
	   #:non-positive-fixnum
	   #:non-negative-fixnum
	   #:positive-fixnum
	   #:aif
	   #:awhen
	   #:aunless
	   #:aprog1
	   #:blambda
	   #:alambda
	   #:flatten
	   #:mkstr
	   #:symb
	   #:single
	   #:mklist
	   #:once-only
	   #:partition
	   #:demultiplex
	   #:merge-lambda-lists
	   #:equals
	   #:clone
	   #:all-slots
	   #:collect-persistent-slots
	   #:extract-additional-keys
	   #:make-doc-updater
	   #:update-doc))

