
(asdf:defsystem :named-readtables
  :description "Library that creates a namespace for named readtable akin to the namespace of packages."
  :author "Tobias C. Rittweiler <trittweiler@common-lisp.net>"
  :version "1.0 (unpublished so far)"
  :licence "BSD"
  :serial t
  :components
  ((:file "package")
   (:file "without-package-locks" :depends-on ("package"))
   (:file "utils"                 :depends-on ("package" "without-package-locks"))
   (:file "cruft"                 :depends-on ("package" "utils"))
   (:file "named-readtables"      :depends-on ("package" "utils" "cruft"))))