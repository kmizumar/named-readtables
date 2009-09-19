
(defpackage :named-readtables-system
  (:use :cl :asdf))

(in-package :named-readtables-system)

(defclass named-readtables-source-file (cl-source-file) ())

(defsystem :named-readtables
  :description "Library that creates a namespace for named readtable akin to the namespace of packages."
  :author "Tobias C. Rittweiler <trittweiler@common-lisp.net>"
  :version "1.0 (unpublished so far)"
  :licence "BSD"
  :serial t
  :default-component-class named-readtables-source-file
  :components
  ((:file "package")
   (:file "utils"                 :depends-on ("package"))
   (:file "cruft"                 :depends-on ("package" "utils"))
   (:file "named-readtables"      :depends-on ("package" "utils" "cruft"))))


#+sbcl
(defmethod perform :around ((operation compile-op)
                            (c named-readtables-source-file))
  (let ((sb-ext:*derive-function-types* t))
    (call-next-method)))