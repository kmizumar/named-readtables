
(in-package "COMMON-LISP-USER")

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
   #:named-readtable-designator
   ))