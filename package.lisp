(mgl-pax:define-package #:trivial-utilities
  (:documentation "trivial-utilities is a collection of useful functions and macros.")
  (:use #:common-lisp #:mgl-pax)
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
	   #:equals))

