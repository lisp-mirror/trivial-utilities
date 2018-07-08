;;;; Copyright (c) Eric Diethelm 2018 <ediethelm@yahoo.com>
;;;; This code is licensed under the MIT license (unless declared otherwise).

(in-package :trivial-utilities)
(annot:enable-annot-syntax)

;;------------- Commonly used types -------------
@export
(deftype negative-fixnum ()
  `(integer ,most-negative-fixnum -1))

@export
(deftype non-positive-fixnum ()
  `(integer ,most-negative-fixnum 0))

@export
(deftype non-negative-fixnum ()
  `(integer 0 , most-positive-fixnum))

@export
(deftype positive-fixnum ()
  `(integer 1 ,most-positive-fixnum))


;;------------- From Graham's On Lisp -------------
;; Inspired by and extended from http://ep.yimg.com/ty/cdn/paulgraham/onlisp.lisp
@export
(defmacro aif (test then &optional else)
  (let ((it (intern (string 'it))))
    `(let ((,it ,test))
       (if ,it ,then ,else))))

@export
(defmacro awhen (test &body then)
  (let ((it (intern (string 'it))))
    `(let ((,it ,test))
       (if ,it (progn ,@then)))))

@export
(defmacro aunless (test &body else)
  (let ((it (intern (string 'it))))
    `(let ((,it ,test))
       (if (not ,it) (progn ,@else)))))

@export
(defmacro aprog1 (first &body body)
  (let ((it (intern (string 'it))))
    `(let ((,it ,first))
       ,@body
       ,it)))

@export
(defmacro blambda (parms &body body)
  `(lambda ,parms
     (block nil
       ,@body)))

@export
(defmacro alambda (parms &body body)
  `(labels ((,(intern (string 'self)) ,parms ,@body))
     #',(intern (string 'self))))

@export
(defgeneric flatten (obj))

@export
(defmethod flatten ((x t))
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

@export
(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

@export
(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

@export
(defun single (lst)
  (and (consp lst) (not (cdr lst))))

@export
(defun mklist (obj)
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

@export
(defmacro once-only ((&rest names) &body body)
  ;; Local modification to use iterate instead of loop
  (let ((gensyms (iterate:iterate (iterate:repeat (length names)) (iterate:collect (gensym)))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))


;;------------- Other useful stuff -------------
@export
(defun partition (pred seq)
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

@export
(defun demultiplex (input &optional (bindings nil))
  (when (or (null input) (not (listp input)))
    (return-from demultiplex (list bindings)))

  (let ((bind (if (listp bindings) bindings (list bindings))))
    (loop for i in (if (listp (car input)) (car input) (list (car input)))
       append (demultiplex (cdr input) (append bind (list i))))))
