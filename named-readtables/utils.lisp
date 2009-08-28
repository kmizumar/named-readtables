;;;;
;;;; Copyright (c) 2008 - 2009 Tobias C. Rittweiler <tcr@freebits.de>
;;;;
;;;; All rights reserved.
;;;;
;;;; See LICENSE for details.
;;;;

(in-package :editor-hints.named-readtables)

(define-condition simple-style-warning (simple-warning style-warning)
  ())

(defun simple-style-warn (format-control &rest format-args)
  (warn 'simple-style-warning
	 :format-control format-control
	 :format-arguments format-args))

;;; Taken from SWANK (which is Public Domain.)

(defmacro destructure-case (value &rest patterns)
  "Dispatch VALUE to one of PATTERNS.
A cross between `case' and `destructuring-bind'.
The pattern syntax is:
  ((HEAD . ARGS) . BODY)
The list of patterns is searched for a HEAD `eq' to the car of
VALUE. If one is found, the BODY is executed with ARGS bound to the
corresponding values in the CDR of VALUE."
  (let ((operator (gensym "op-"))
        (operands (gensym "rand-"))
        (tmp (gensym "tmp-")))
    `(let* ((,tmp ,value)
            (,operator (car ,tmp))
            (,operands (cdr ,tmp)))
       (case ,operator
         ,@(loop for (pattern . body) in patterns collect
                   (if (eq pattern t)
                       `(t ,@body)
                       (destructuring-bind (op &rest rands) pattern
                         `(,op (destructuring-bind ,rands ,operands
                                 ,@body)))))
         ,@(if (eq (caar (last patterns)) t)
               '()
               `((t (error "destructure-case failed: ~S" ,tmp))))))))

;;; Taken from Alexandria (which is Public Domain, or BSD.)

(defun required-argument (&optional name)
  "Signals an error for a missing argument of NAME. Intended for
use as an initialization form for structure and class-slots, and
a default value for required keyword arguments."
  (error "Required argument ~@[~S ~]missing." name))