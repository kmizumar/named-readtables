
(in-package :editor-hints.named-readtables)

#-(or sbcl clozure)
(eval-when (:compile-toplevel)
  (warn "~A hasn't been ported to ~A; you're likely to get a compiler error" 
	(package-name *package*) (lisp-implementation-type)))

(defvar *readtable-names* (make-hash-table :test 'eq))

(declaim (inline %register-readtable-name))
(defun %register-readtable-name (name read-table)
  (setf (gethash read-table *readtable-names*) name))

(declaim (inline %unregister-readtable-name))
(defun %unregister-readtable-name (read-table)
  (remhash read-table *readtable-names*))

(declaim (inline %readtable-name))
(defun %readtable-name (read-table)
  (values (gethash read-table *readtable-names*)))

(declaim (inline %list-all-readtable-names))
(defun %list-all-readtable-names ()
  (list* :standard :current
	 (loop for name being each hash-value of *readtable-names*
	       collect name)))

;;;; Implementation-dependent cruft

(eval-when (:compile-toplevel :execute)
  (when (find-package "SB-READER")
    (push :sbcl+sb-reader *features*)))

(declaim (inline %standard-readtable))
(defun %standard-readtable ()
  #+sbcl+sb-reader sb-reader:*standard-readtable*
  #-sbcl+sb-reader (copy-readtable nil))

#+sbcl+sb-reader
(defmacro %with-readtable-iterator ((name readtable) &body body)
  `(sb-reader:with-readtable-iterator (,name ,readtable) ,@body))

#+(and sbcl (not sbcl+sb-reader))
(progn
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

  (defmacro %with-readtable-iterator ((name readtable) &body body)
    (let ((it (gensym)))
      `(let ((,it (make-readtable-iterator ,readtable)))
	 (macrolet ((,name () `(funcall ,',it)))
	   ,@body)))))
#+clozure
(progn
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
  (defvar *named-readtables* (make-hash-table :test 'eq))

  (declaim (inline %register-readtable))
  (defun %register-readtable (name read-table)
    (setf (gethash name *named-readtables*) read-table))

  (declaim (inline %unregister-readtable))
  (defun %unregister-readtable (name)
    (remhash name *named-readtables*))

  (declaim (inline %find-readtable-from-name))
  (defun %find-readtable-from-name (name)
    (values (gethash name *named-readtables* nil))))


;; FIXME: Adapt from STRING-DESIGNATOR to SYMBOL.
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
