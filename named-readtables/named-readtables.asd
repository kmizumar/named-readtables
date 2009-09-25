;;; -*- Mode:Lisp -*-

(in-package :cl-user)

(defclass asdf::named-readtables-source-file (asdf:cl-source-file) ())

(asdf:defsystem :named-readtables
  :description "Library that creates a namespace for named readtable akin to the namespace of packages."
  :author "Tobias C. Rittweiler <trittweiler@common-lisp.net>"
  :version "1.0 (unpublished so far)"
  :licence "BSD"
  :default-component-class asdf::named-readtables-source-file
  :components
  ((:file "package")
   (:file "utils"                 :depends-on ("package"))
   (:file "cruft"                 :depends-on ("package" "utils"))
   (:file "named-readtables"      :depends-on ("package" "utils" "cruft"))))


#+sbcl
(defmethod asdf:perform :around
    ((operation asdf:compile-op) (component asdf::named-readtables-source-file))
  (let ((sb-ext:*derive-function-types* t))
    (call-next-method)))