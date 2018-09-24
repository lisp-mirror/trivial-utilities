;;;; Copyright (c) Eric Diethelm 2018 <ediethelm@yahoo.com>
;;;; This code is licensed under the MIT license (unless declared otherwise).

(in-package :trivial-utilities)

;;------------- Commonly used types -------------
(deftype negative-fixnum ()
  "Defines all negative integer types upto and including -1."
  `(integer ,most-negative-fixnum -1))

(deftype non-positive-fixnum ()
  "Defines all non-positive integer types upto and including 0."
  `(integer ,most-negative-fixnum 0))

(deftype non-negative-fixnum ()
  "Defines all non-negative integer types starting from and including 0."
  `(integer 0 , most-positive-fixnum))

(deftype positive-fixnum ()
  "Defines all positive integer types starting from and including 1."
  `(integer 1 ,most-positive-fixnum))


;;------------- From Graham's On Lisp -------------
;; Inspired by and extended from http://ep.yimg.com/ty/cdn/paulgraham/onlisp.lisp
(defmacro aif (test then &optional else)
  "Similar to the normal *IF*, but binds the result of *TEST* via *LET* to *IT*."
  (let ((it (intern (string 'it))))
    `(let ((,it ,test))
       (if ,it ,then ,else))))

(defmacro awhen (test &body then)
  "Similar to the normal *WHEN*, but binds the result of *TEST* via *LET* to *IT*."
  (let ((it (intern (string 'it))))
    `(let ((,it ,test))
       (if ,it (progn ,@then)))))

(defmacro aunless (test &body else)
  "Similar to the normal *UNLESS*, but binds the result of *TEST* via *LET* to *IT*."
  (let ((it (intern (string 'it))))
    `(let ((,it ,test))
       (if (not ,it) (progn ,@else)))))

(defmacro aprog1 (first &body body)
  "Similar to the normal *PROG1*, but binds the result of *FIRST* via *LET* to *IT* and returns *IT*."
  (let ((it (intern (string 'it))))
    `(let ((,it ,first))
       ,@body
       ,it)))

(defmacro blambda (parms &body body)
  "Similar to *LAMBDA*, but defines an unnamed *BLOCK* from which one can *RETURN*."
  `(lambda ,parms
     (block nil
       ,@body)))

(defmacro alambda (parms &body body)
  "Similar to *LAMBDA*, but defines *SELF* via *LABELS* which contains the *BODY*. *SELF* can be referenced in *BODY*."
  `(labels ((,(intern (string 'self)) ,parms ,@body))
     #',(intern (string 'self))))

(defgeneric flatten (obj)
  (:documentation "Flattens the structure of an object.

An implementation for *LIST*s already exists. Add specific implementations for special objects."))

(defmethod flatten ((obj t))
  "Returns a list with the flatten contents of the *LIST* *OBJ*."
  (labels ((rec (obj acc)
             (cond ((null obj) acc)
                   ((atom obj) (cons obj acc))
                   (t (rec (car obj) (rec (cdr obj) acc))))))
    (rec obj nil)))

(defun mkstr (&rest args)
  "Creates a string out of all elements in *ARGS*. Each element must be printable per *PRINC*."
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  "Creates a new and internalized *SYMBOL*, named by applying *MKSTR* to *ARGS*."
  (values (intern (apply #'mkstr args))))

(defun single (lst)
  "Predicate verifying that *LST* is a list containing exactly one element."
  (and (consp lst) (not (cdr lst))))

(defun mklist (obj)
  "Returns a new list containing *OBJ*, or *OBJ* itself if it is already a list."
  (the list (if (listp obj) obj (list obj))))


;;------------- From Seibel's Practical Common Lisp -------------
;; Copyright (c) 2005, Peter Seibel All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;    * Redistributions of source code must retain the above copyright
;;      notice, this list of conditions and the following disclaimer.
;;
;;    * Redistributions in binary form must reproduce the above
;;      copyright notice, this list of conditions and the following
;;      disclaimer in the documentation and/or other materials provided
;;      with the distribution.
;;
;;    * Neither the name of the Peter Seibel nor the names of its
;;      contributors may be used to endorse or promote products derived
;;      from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(defmacro once-only ((&rest names) &body body)
  "Protects against multiple evaluation of *NAMES* during macro-expansion. Usualy used inside of other macros."
  ;; Local modification to use iterate instead of loop
  (let ((gensyms (iterate:iterate (iterate:repeat (length names)) (iterate:collect (gensym)))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
	  ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
	     ,@body)))))


;;------------- Other useful stuff -------------
(defun partition (pred seq)
  "Separates the contents of *SEQ* into two lists, depending on the result of *PRED* when applied to each element of *SEQ*."
  (declare (type function pred))
  (mapcar #'reverse ; reverse both lists
          (reduce (lambda (r x)
                    (if (funcall pred x)
                        (list (cons x (car r))
                              (cadr r))
			(list (car r)
			      (cons x (cadr r)))))
                  seq
                  :initial-value '(() ()))))

(defun demultiplex (input &optional (bindings nil))
  "Given a *LIST* of *LIST*s returns all combinations of the elements in each of the *LIST*s in *INPUT*."
  (when (or (null input) (not (listp input)))
    (return-from demultiplex (list bindings)))

  (let ((bind (mklist bindings)))
    (iterate:iterate
      (iterate:for i in (mklist (car input)))
      (iterate:appending (demultiplex (cdr input) (append bind (list i)))))))


(defgeneric equals (obj1 obj2 &key &allow-other-keys)
   (:method (obj1 obj2 &key &allow-other-keys)
    "Default method if all other cases fail."
    (cl:equal obj1 obj2))) ; calls the original version

(defmethod equals ((obj1 list) (obj2 list) &key &allow-other-keys)
    (and (eq (length obj1) (length obj2))
	 (notany #'(lambda (x y) (not (equals x y))) obj1 obj2)))


(defgeneric clone (obj &key &allow-other-keys)
  (:documentation "A generic function to clone objects."))

