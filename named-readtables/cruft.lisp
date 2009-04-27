;;;;
;;;; Copyright (c) 2008 - 2009 Tobias C. Rittweiler <tcr@freebits.de>
;;;;
;;;; All rights reserved.
;;;;
;;;; See LICENSE for details.
;;;;

(in-package :editor-hints.named-readtables)

(defmacro define-cruft (name lambda-list &body (docstring . alternatives))
  (assert (typep docstring 'string) (docstring) "Docstring missing!")
  (assert (not (null alternatives)))
  `(progn
     (declaim (inline ,name))
     (defun ,name ,lambda-list ,docstring ,(first alternatives))))

(eval-when (:compile-toplevel :execute)
  #+sbcl (when (find-symbol "ASSERT-NOT-STANDARD-READTABLE"
                            (find-package "SB-IMPL"))
           (pushnew :sbcl+safe-standard-readtable *features*)))


;;;;; Implementation-dependent cruft


;;; This should return an implementation's actual standard readtable
;;; object only if the implementation makes the effort to guard against
;;; modification of that object. Otherwise it should better return a
;;; copy.
(define-cruft %standard-readtable ()
  "Return the standard readtable."
  #+ :sbcl+safe-standard-readtable sb-impl::*standard-readtable*
  #+ :common-lisp                  (copy-readtable nil))


;;;; Mapping between a readtable object and its readtable-name.

(defvar *readtable-names* (make-hash-table :test 'eq))

(define-cruft %associate-readtable-with-name (name readtable)
  "Associate READTABLE with NAME for READTABLE-NAME to work."
  #+ :common-lisp (setf (gethash readtable *readtable-names*) name))

(define-cruft %unassociate-readtable-from-name (name readtable)
  "Remove the association between READTABLE and NAME."
  #+ :common-lisp (progn (assert (eq name (gethash readtable *readtable-names*)))
                         (remhash readtable *readtable-names*)))

(define-cruft %readtable-name (readtable)
  "Return the name associated with READTABLE."
  #+ :common-lisp (values (gethash readtable *readtable-names*)))

(define-cruft %list-all-readtable-names ()
  "Return a list of all available readtable names."
  #+ :common-lisp (list* :standard :current
                         (loop for name being each hash-value of *readtable-names*
                               collect name)))


;;;; Mapping between a readtable-name and the actual readtable object.

;;; On Allegro we reuse their named-readtable support so we work
;;; nicely on their infrastructure.

#-allegro
(defvar *named-readtables* (make-hash-table :test 'eq))

#+allegro
(defun readtable-name-for-allegro (symbol)
  (multiple-value-bind (kwd status)
        (if (keywordp symbol)
            (values symbol nil)
            ;; Kludge: ACL uses keywords to name readtables, we allow
            ;; arbitrary symbols.
            (intern (format nil "~A.~A"
                            (package-name (symbol-package symbol))
                            (symbol-name symbol))
                    :keyword))
    (prog1 kwd
      (assert (or (not status) (get kwd 'named-readtable-designator)))
      (setf (get kwd 'named-readtable-designator) t))))

(define-cruft %associate-name-with-readtable (name readtable)
  "Associate NAME with READTABLE for FIND-READTABLE to work."
  #+ :allegro     (setf (excl:named-readtable (readtable-name-for-allegro name)) readtable)
  #+ :common-lisp (setf (gethash name *named-readtables*) readtable))

(define-cruft %unassociate-name-from-readtable (name readtable)
  "Remove the association between NAME and READTABLE"
  #+ :allegro     (let ((n (readtable-name-for-allegro name)))
                    (assert (eq readtable (excl:named-readtable n)))
                    (setf (excl:named-readtable n) nil))
  #+ :common-lisp (progn (assert (eq readtable (gethash name *named-readtables*)))
                         (remhash name *named-readtables*)))

(define-cruft %find-readtable (name)
  "Return the readtable named NAME."
  #+ :allegro     (excl:named-readtable (readtable-name-for-allegro name))
  #+ :common-lisp (values (gethash name *named-readtables* nil)))


;;;; Readtables Iterators

(defmacro with-readtable-iterator ((name readtable) &body body)
  (let ((it (gensym)))
    `(let ((,it (%make-readtable-iterator ,readtable)))
       (macrolet ((,name () `(funcall ,',it)))
         ,@body))))

#+sbcl
(defun %make-readtable-iterator (readtable)
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
(defun %make-readtable-iterator (readtable)
  (let ((char-macro-alist (ccl::rdtab.alist readtable)))
    (lambda ()
      (if char-macro-alist
          (destructuring-bind (char . defn) (pop char-macro-alist)
            (if (consp defn)
                (values t char (car defn) t (cdr defn))
                (values t char defn nil nil)))
          (values nil nil nil nil nil)))))

;;; Written on ACL 8.0.
#+allegro
(defun %make-readtable-iterator (readtable)
  (declare (optimize speed))            ; for TCO
  (check-type readtable readtable)
  (let* ((+macro-attr+ 1)               ; discovered through "reverse-engineering"
         (attribute-table (first (excl::readtable-attribute-table readtable)))
         (macro-table     (first (excl::readtable-macro-table readtable)))
         (dispatch-tables (excl::readtable-dispatch-tables readtable))
         (table-length    (length attribute-table))
         (idx 0))
    (assert (= table-length (length macro-table)))
    (labels ((grovel-macro-chars ()
               (if (>= idx table-length)
                   (grovel-dispatch-chars)
                   (let ((attr (svref attribute-table idx)))
                     (incf idx)
                     (if (= attr +macro-attr+)
                         (values t (code-char (1- idx)) (svref macro-table idx) nil nil)
                         (grovel-macro-chars)))))
             (grovel-dispatch-chars ()
               (if (null dispatch-tables)
                   (values nil nil nil nil nil)
                   (destructuring-bind (disp-char sub-char-table)
                       (first dispatch-tables)
                     (setf dispatch-tables (rest dispatch-tables))
                     (values t
                             disp-char
                             (svref macro-table (char-code disp-char))
                             t
                             (loop for subch-fn   across sub-char-table
                                   for subch-code from 0
                                   when subch-fn
                                     collect (cons (code-char subch-code)
                                                   subch-fn)))))))
      #'grovel-macro-chars)
    ))

#-(or sbcl clozure allegro)
(eval-when (:compile-toplevel)
  (simple-style-warn
   "~A hasn't been ported to ~A. ~
    We fall back to a portable implementation of readtable iterators. ~
    This implementation has to grovel through all available characters. ~
    On Unicode-aware implementations this comes with some costs." 
   (package-name *package*) (lisp-implementation-type)))

#-(or sbcl clozure allegro)
(defun %make-readtable-iterator (readtable)
  (check-type readtable readtable)
  (let ((char-code 0))
    #'(lambda ()
        (prog ()
           :GROVEL
           (when (< char-code char-code-limit)
             (let* ((char (code-char char-code))
                    (fn   (get-macro-character char readtable)))
               (incf char-code)
               (when (not fn) (go :GROVEL))
               (multiple-value-bind (disp? alist)
                   (handler-case ; grovel dispatch macro characters.
                       (values t (loop for code from 0 below char-code-limit
                                       for subchar = (code-char code)
                                       for disp-fn = (get-dispatch-macro-character
                                                      char subchar readtable)
                                       when disp-fn
                                       collect (cons subchar disp-fn)))
                     (error () nil))
                 (return (values t char fn disp? alist)))))))))
