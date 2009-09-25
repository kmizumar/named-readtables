;;; -*- Mode:Lisp -*-

(in-package :cl-user)

(defpackage :named-readtables-test
  (:use :cl :named-readtables)
  (:import-from :named-readtables
     #:*empty-readtable*
     #:do-readtable
     #:ensure-function
     #:function=))