
(defsystem :named-readtables
  :description "Library that creates a namespace for named readtable akin to the namespace of packages."
  :author "Tobias C. Rittweiler <trittweiler@common-lisp.net>"
  :version "1.0"
  :licence "BSD"
  :components
  ((:file "package")
   (:file "destructure-case")
   (:file "cruft")
   (:file "named-readtables")))