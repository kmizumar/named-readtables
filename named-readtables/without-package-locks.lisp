;;;;
;;;; Copyright (c) 2008 - 2009 Tobias C. Rittweiler <tcr@freebits.de>
;;;;
;;;; All rights reserved.
;;;;
;;;; See LICENSE for details.
;;;;

(in-package :editor-hints.named-readtables)

(defmacro without-package-lock ((&rest package-names) &body body)
  (declare (ignorable package-names))
  #+clisp (return-from without-package-lock
            `(ext:without-package-lock (,@package-names) ,@body))
  `(progn ,@body))