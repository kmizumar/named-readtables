
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

;;;; Implementation-dependent cruft

(eval-when (:compile-toplevel :execute)
  (when (find-package "SB-READER")
    (push :sbcl+sb-reader *features*)))

(declaim (inline %standard-readtable))
(defun %standard-readtable ()
  #+sbcl+sb-reader sb-reader:*standard-readtable*
  #+clozure ccl::%initial-readtable%
  #-(or sbcl+sb-reader clozure)
  (copy-readtable nil))

#+sbcl+sb-reader
(defmacro %with-readtable-iterator ((name readtable) &body body)
  `(sb-reader:with-readtable-iterator (,name ,readtable) ,@body))

#+(or (and sbcl (not sbcl+sb-reader)) clozure)
(progn
  #+sbcl
  (defun make-readtable-iterator (readtable)
    (let ((char-macro-array (sb-impl::character-macro-array readtable))
	  (char-macro-ht    (sb-impl::character-macro-hash-table readtable))
	  (dispatch-tables  (sb-impl::dispatch-tables readtable))
	  (char-code 0))
      (with-hash-table-iterator (ht-iterator char-macro-ht)
	(labels ((grovel1 ()		; grovel base macro characters
		   (declare (optimize sb-c::merge-tail-calls))
		   (if (>= char-code sb-int:base-char-code-limit)
		       (grovel2)
		       (let ((entry (svref char-macro-array char-code)))
			 (setq char-code (1+ char-code))
			 (if entry
			     (values t (code-char (1- char-code)) entry nil nil)
			     (grovel1)))))
		 (grovel2 ()	     ; grovel unicode macro characters
		   (multiple-value-bind (more? char reader-fn) (ht-iterator)
		     (if (not more?)
			 (grovel3)
			 (values t char reader-fn nil nil))))
		 (grovel3 ()	    ; grovel dispatch macro characters
		   (if (null dispatch-tables)
		       (values nil nil nil nil nil)
		       (let* ((disp-ch (caar dispatch-tables))
			      (disp-ht (cdar dispatch-tables))
			      (disp-fn (get-macro-character disp-ch readtable))
			      (sub-char-alist))
			 (setq dispatch-tables (cdr dispatch-tables))
			 (maphash (lambda (k v) (push (cons k v) sub-char-alist)) disp-ht)
			 (values t disp-ch disp-fn t sub-char-alist)))))
	  #'grovel1))))

  #+clozure
  (defun make-readtable-iterator (readtable)
    (let ((char-macro-alist (ccl::rdtab.alist readtable)))
      (lambda ()
	(if char-macro-alist
	    (destructuring-bind (char . defn) (pop char-macro-alist)
	      (if (consp defn)
		  (values t char (car defn) t (cdr defn))
		  (values t char defn nil nil)))
	    (values nil nil nil nil nil)))))

  (defmacro %with-readtable-iterator ((name readtable) &body body)
    (let ((it (gensym)))
      `(let ((,it (make-readtable-iterator ,readtable)))
	 (macrolet ((,name () `(funcall ,',it)))
	   ,@body)))))

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
