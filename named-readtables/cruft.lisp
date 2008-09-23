
(in-package :editor-hints.named-readtables)

(defvar *readtable-names* (make-hash-table :test 'eq))

(declaim (inline %register-readtable-name))
(defun %register-readtable-name (string-designator read-table)
  (setf (gethash read-table *readtable-names*)
	(string string-designator)))

(declaim (inline %unregister-readtable-name))
(defun %unregister-readtable-name (read-table)
  (remhash read-table *readtable-names*))

(declaim (inline %readtable-name))
(defun %readtable-name (read-table)
  (values (gethash read-table *readtable-names*)))

(declaim (inline %list-all-readtable-names))
(defun %list-all-readtable-names ()
  (list* "STANDARD" "CURRENT"
	 (loop for name being each hash-value of *readtable-names*
	       collect name)))

(declaim (inline %standard-readtable))
(defun %standard-readtable ()
  #-sbcl (copy-readtable nil)
  #+sbcl sb-reader:*standard-readtable*)

#+sbcl
(defmacro %with-readtable-iterator ((name readtable) &body body)
  `(sb-reader:with-readtable-iterator (,name ,readtable) ,@body))

#-allegro
(progn
  (defvar *named-readtables* (make-package (gensym "READTABLES+")))

  (declaim (inline %register-readtable))
  (defun %register-readtable (string-designator read-table)
    (let ((name (intern (string string-designator) *named-readtables*)))
      (setf (symbol-value name) read-table)))

  (declaim (inline %unregister-readtable))
  (defun %unregister-readtable (string-designator)
    (let ((name (find-symbol (string string-designator) *named-readtables*)))
      (when name
	(setf (symbol-value name) nil)
	(unintern name *named-readtables*))))

  (declaim (inline %find-readtable-from-name))
  (defun %find-readtable-from-name (string-designator)
    (let ((name (find-symbol (string string-designator) *named-readtables*)))
      (and name (symbol-value name)))))


#+allegro
(progn
  (defun %register-readtable (string-designator read-table)
    (let ((name (intern (string string-designator) :keyword)))
      (setf (named-readtable name) read-table)))

  (defun %unregister-readtable (string-designator)
    (let ((name (intern (string string-designator) :keyword)))
      (setf (named-readtable name) nil)))

  (defun %find-readtable-from-name (string-designator)
    (let ((name (find-symbol (string string-designator) :keyword)))
      (and name (named-readtable name)))))
