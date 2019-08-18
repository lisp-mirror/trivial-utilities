;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defsystem :trivial-utilities
  :name "trivial-utilities"
  :description "A collection of useful functions and macros."
  :version "0.3.11"
  :author "Eric Diethelm <ediethelm@yahoo.com>"
  :licence "MIT"
  :depends-on (:alexandria
	       :iterate
	       :closer-mop)
  :components ((:file "package")
	       (:file "trivial-utilities")))

