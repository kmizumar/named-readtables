;;; -*- Mode:Lisp -*-

(in-package :named-readtables-test)

(defun map-alist (car-fn cdr-fn alist)
  (mapcar #'(lambda (entry)
              (cons (funcall car-fn (car entry))
                    (funcall cdr-fn (cdr entry))))
          alist))

(defmacro signals-error-p (name &body body)
  `(handler-case (prog1 nil ,@body)
     (,(second name) () t)))

(defmacro continue-condition (name &body body)
  `(handler-bind ((,(second name) #'continue))
     ,@body))

(defun read-with-readtable (name string)
  (let ((*package* '#.*package*)
        (*readtable* (find-readtable name)))
    (values (read-from-string string))))

(defun empty-readtable ()
  (copy-readtable *empty-readtable*))

(defun random-named-readtable ()
  (let ((readtables (list-all-named-readtables)))
    (nth (random (length readtables)) readtables)))


(defun readtable-content (named-readtable-designator)
  (let ((readtable (ensure-readtable named-readtable-designator))
        (result '()))
    ;; Make sure to canonicalize the order and function designators so
    ;; we can compare easily.
    (do-readtable ((char reader-fn ntp disp? table) readtable)
      (setq table (sort (copy-list table) #'char< :key #'car))
      (push (list* char
                   (ensure-function reader-fn)
                   ntp
                   (and disp? (list (map-alist #'identity
                                               #'ensure-function
                                               table))))
            result))
    (sort result #'char< :key #'car)))

(defun readtable= (rt1 rt2)
  (tree-equal (readtable-content rt1) (readtable-content rt2)
              :test #'(lambda (x y)
                        (if (and (functionp x) (functionp y))
                            (function= x y)
                            (eql x y)))))


(defun read-A (stream c)
  (declare (ignore stream c))
  :a)

(defun read-A-as-X (stream c)
  (declare (ignore stream c))
  :x)

(defun read-B (stream c)
  (declare (ignore stream c))
  :b)

(defun read-sharp-paren (stream c n)
  (declare (ignore stream c n))
  'sharp-paren)

(defun read-C (stream c)
  (declare (ignore stream c))
  :c)

(defreadtable A
  (:macro-char #\A #'read-A))

(defreadtable A-as-X
  (:macro-char #\A #'read-A-as-X))

(defreadtable B
  (:macro-char #\B #'read-B))

(defreadtable C
  (:macro-char #\C #'read-C))

(defreadtable A+B+C
  (:merge A B C))

(defreadtable standard+A+B+C
  (:merge :standard A+B+C))

(defreadtable sharp-paren
  (:macro-char #\# :dispatch)
  (:dispatch-macro-char #\# #\( #'read-sharp-paren))


(deftest cruft.1
    (function= (get-macro-character #\" (copy-readtable nil))
               (get-macro-character #\" (copy-readtable nil)))
  t)

(deftest standard.1
    (read-with-readtable :standard "ABC")
  ABC)

(deftest standard.2
    (read-with-readtable :standard "(A B C)")
  (A B C))

(deftest standard.3
    (let ((x (find-readtable nil))
          (y (find-readtable :standard))
          (z (find-readtable :common-lisp)))
      (and (eq x y) (eq y z)))
  t)


(deftest modern.1
    (read-with-readtable :modern "FooF")
  |FooF|)


(deftest empty.1
    (null (readtable-content (empty-readtable)))
  t)

(deftest empty.2
    (let ((rt (copy-readtable (find-readtable :standard))))
      (readtable= (merge-readtables-into (empty-readtable) rt)
                  (merge-readtables-into rt (empty-readtable))))
  t)


(deftest basics.1
    (read-with-readtable 'A "A")
  :a)

(deftest basics.2
    (read-with-readtable 'A-as-X "A")
  :x)

(deftest basics.3
    (read-with-readtable 'A "B")
  B)

(deftest basics.4
    (read-with-readtable 'A "(A B C)")
  |(|)


(deftest unregister.1
    (let ((rt (find-readtable 'A)))
      (register-readtable 'does-not-exist rt)
      (values
        (and (find-readtable 'does-not-exist) t)
        (unregister-readtable 'does-not-exist)
        (and (find-readtable 'does-not-exist) t)))
  t t nil)


(deftest name.1
    (let ((rt (random-named-readtable)))
      (eq rt (find-readtable (readtable-name rt))))
  t)

(deftest ensure.1
    (unwind-protect
         (let* ((x (ensure-readtable 'does-not-exist (find-readtable 'A)))
                (y (find-readtable 'A))
                (z (find-readtable 'does-not-exist)))
           (and (eq x y) (eq y z)))
      (unregister-readtable 'does-not-exist))
  t)


(deftest merge.1
    (values
      (read-with-readtable 'A+B+C "A")
      (read-with-readtable 'A+B+C "B")
      (read-with-readtable 'A+B+C "C"))
  :a :b :c)

(deftest merge.2
    (read-with-readtable 'standard+A+B+C "(A B C)")
  (:a :b :c))

(deftest merge.3
    (read-with-readtable 'standard+A+B+C "#(A B C)")
  #(:a :b :c))

(deftest merge.4
    (let ((A+B+C+standard (merge-readtables-into (copy-named-readtable 'A+B+C)
                                                 :standard)))
      (readtable= 'standard+A+B+C A+B+C+standard))
  t)


(deftest rename.1
    (unwind-protect
         (progn (make-readtable 'A* :merge '(A))
                (rename-readtable 'A* 'A**)
                (values (and (find-readtable 'A*) t)
                        (and (find-readtable 'A**) t)))
      (unregister-readtable 'A*)
      (unregister-readtable 'A**))
  nil
  t)


(deftest reader-macro-conflict.1
    (signals-error-p 'reader-macro-conflict
      (merge-readtables-into (empty-readtable) 'A 'A-as-X))
  t)

(deftest reader-macro-conflict.2
    (signals-error-p 'reader-macro-conflict
      (merge-readtables-into (empty-readtable) :standard :standard))
  nil)

(deftest reader-macro-conflict.3
    (signals-error-p 'reader-macro-conflict
      (merge-readtables-into (empty-readtable) 'A+B+C 'A))
  nil)

(deftest reader-macro-conflict.4
    (signals-error-p 'reader-macro-conflict
      (merge-readtables-into (empty-readtable) :standard 'sharp-paren))
  t)


(deftest readtable-does-not-exist.1
    (signals-error-p 'readtable-does-not-exist
      (ensure-readtable 'does-not-exist))
  t)


(deftest readtable-does-already-exist.1
    (signals-error-p 'readtable-does-already-exist
      (make-readtable 'A))
  t)

(deftest readtable-does-already-exist.2
    (signals-error-p 'readtable-does-already-exist
      (make-readtable 'A))
  t)

(deftest readtable-does-already-exist.3
    (let ((rt (make-readtable 'does-not-exist :merge '(:standard A B))))
      (declare (ignore rt))
      (unwind-protect
           (read-with-readtable (continue-condition 'readtable-does-already-exist
                                  (make-readtable 'does-not-exist
                                                  :merge '(:standard A C)))
                       
                                "(A B C)")
        (unregister-readtable 'does-not-exist)))
    (:a B :c))