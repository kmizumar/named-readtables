
(in-package :common-lisp-user)

(defpackage :editor-hints.named-readtables
  (:use :common-lisp)
  (:nicknames :named-readtables)
  (:export
   #:defreadtable
   #:in-readtable
   #:make-readtable
   #:find-readtable
   #:ensure-readtable
   #:rename-readtable
   #:readtable-name
   #:register-readtable
   #:unregister-readtable
   #:list-all-named-readtables
   ;; Types
   #:named-readtable-designator
   ;; Conditions
   #:readtable-does-not-exist
   #:readtable-does-already-exist
   ))