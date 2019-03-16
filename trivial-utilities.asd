;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defsystem :trivial-utilities
  :name "trivial-utilities"
  :description "A collection of useful functions and macros."
  :version "0.3.7"
  :author "Eric Diethelm <ediethelm@yahoo.com>"
  :licence "MIT"
  :depends-on (:iterate
               :closer-mop
	       :mgl-pax)
  :components ((:file "package")
	       (:file "trivial-utilities")
	       (:file "documentation")))

