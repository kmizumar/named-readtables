;;;;
;;;; Copyright (c) 2007 - 2009 Tobias C. Rittweiler <tcr@freebits.de>
;;;; Copyright (c) 2007, Robert P. Goldman <rpgoldman@sift.info> and SIFT, LLC
;;;;
;;;; All rights reserved.
;;;;
;;;; See LICENSE for details.
;;;;

(in-package :editor-hints.named-readtables)

;;;
;;;  ``This is enough of a foothold to implement a more elaborate
;;;    facility for using readtables in a localized way.''
;;;
;;;                               (X3J13 Cleanup Issue IN-SYNTAX)
;;;


;;; TODO:
;;;
;;;    * Remove redefinition style-warning in DEFREADTABLE
;;;
;;;    * Add style-warning if redefined readtable contains entries
;;;      not specified in the DEFREADTABLE form (cf. SBCL's DEFPACKAGE)
;;;
;;;    * Add style-warning if :MERGE overwrites entries
;;;
;;;    * Add :FUZE that's like :MERGE without the style-warnings
;;;
;;;    * Think about MAKE-DISPATCHING-MACRO-CHARACTER in DEFREADTABLE


;;;;;; DEFREADTABLE &c.

(defmacro defreadtable (name &body options)
  "Define a new named readtable, whose name is given by the symbol NAME.
Or, if a readtable is already registered under that name, redefine that one.

The readtable can be populated using the following OPTIONS:

  (:MERGE &REST READTABLE-DESIGNATORS) 

      Merge the readtables designated into the new readtable, using
      `merge-readtables-into'.  It is mandatory to supply at least one :MERGE
      clause.
 
      DEFREADTABLE accepts some special readtable designators, including NIL
      or :STANDARD for the standard readtable and :CURRENT for the current
      readtable, as well as the names of programmer-defined readtables.

      Note that the process of merging readtables is _not_ commutative, so that
      macros in later entries will overwrite earlier ones.

  (:DISPATCH-MACRO-CHAR MACRO-CHAR SUB-CHAR FUNCTION)

      Define a new sub character SUB-CHAR for the dispatching macro character
      MACRO-CHAR, per `set-dispatch-macro-character'. You probably have to
      define MACRO-CHAR as a dispatching macro character by the following option
      first.

  (:MACRO-CHAR MACRO-CHAR FUNCTION &OPTIONAL NON-TERMINATING-P)

      Define a new macro character in the readtable, per `set-macro-character'.
      If FUNCTION is the keyword :DISPATCH, MACRO-CHAR is made a dispatching
      macro character, per `make-dispatch-macro-character'.

  (:SYNTAX-FROM FROM-READTABLE-DESIGNATOR FROM-CHAR TO-CHAR)

      Set the character syntax of TO-CHAR in the readtable being defined to the
      same syntax as FROM-CHAR in FROM-READTABLE-DESIGNATOR as per
      `set-syntax-from-char'.

  (:CASE CASE-MODE) 

      Defines the /case sensititivy mode/ of the resulting readtable.

Any number of option clauses may appear. The options are grouped by their
type, but in each group the textual order the options appeared is preserved.
The following groups exist and are executed in this order: :MERGE, :CASE, 
:MACRO-CHAR and :DISPATCH-MACRO-CHAR (one group), finally :SYNTAX-FROM.

The readtable is defined at load-time. If you want to have it available at
compilation time -- say to use its reader-macros in the same file as its
definition -- you have to wrap the DEFREADTABLE form in an explicit EVAL-WHEN.
"
  (check-type name symbol)
  (when (reserved-readtable-name-p name)
    (error "~A is the designator for a predefined readtable. ~
            Not acceptable as a user-specified readtable name." name))
  (flet ((process-option (option var)
           (destructure-case option
             ((:merge &rest readtable-designators)
	      `(merge-readtables-into ,var ,@readtable-designators))
             ((:dispatch-macro-char disp-char sub-char function)
              `(set-dispatch-macro-character ,disp-char ,sub-char ,function ,var))
             ((:macro-char char function &optional non-terminating-p)
	      (if (eq function :dispatch)
		  ;; FIXME: This is not perfect as SBCL signals an
		  ;; error on already existing dispatch macros. I sent
		  ;; an appropriate patch upstream.
		  `(make-dispatch-macro-character ,char ,non-terminating-p ,var)
		  `(set-macro-character ,char ,function ,non-terminating-p ,var)))
	     ((:syntax-from from-rt-designator from-char to-char)
	      `(set-syntax-from-char ,to-char ,from-char 
				     ,var (find-readtable ,from-rt-designator)))
	     ((:case mode)
	      `(setf (readtable-case ,var) ,mode))))
	 (remove-clauses (clauses options)
	   (setq clauses (if (listp clauses) clauses (list clauses)))
	   (remove-if-not #'(lambda (x) (member x clauses)) 
			  options :key #'first)))
    (let* ((merge-clauses  (remove-clauses :merge options))
	   (case-clauses   (remove-clauses :case  options))
	   (macro-clauses  (remove-clauses '(:macro-char :dispatch-macro-char)
					   options))
	   (syntax-clauses (remove-clauses :syntax-from options))
	   (other-clauses  (set-difference options 
					   (append merge-clauses case-clauses 
						   macro-clauses syntax-clauses))))
      (cond 
	((null merge-clauses)
	 (error "You must specify at least one :MERGE clause in DEFREADTABLE."))
	((not (null other-clauses))
	 (error "Bogus DEFREADTABLE clauses: ~{~A~^, ~}" other-clauses))
	(t
	 `(eval-when (:load-toplevel :execute)
	    (handler-bind ((readtable-does-already-exist
			    #'(lambda (c)
				(simple-style-warn 
				 "Overwriting previously existing readtable ~S."
				 (existing-readtable-name c))
				(continue c))))
	      ;; The (FIND-READTABLE ...) is important for proper redefinition
	      ;; semantics.
	      (let ((readtable (find-readtable ',name)))
		(setq readtable
		      (make-readtable ',name
				      ;; We have to provide an explicit :MERGE
				      ;; argument, because otherwise the
				      ;; :STANDARD readtable would be used which
				      ;; is not necessarily what we want.
				      :merge ',(rest (first merge-clauses))))
		;; We have to grovel all MERGE-CLAUSES for the redefinition case.
		,@(loop for option in merge-clauses
			collect (process-option option 'readtable))
		,@(loop for option in case-clauses
			collect (process-option option 'readtable))
		,@(loop for option in macro-clauses
			collect (process-option option 'readtable))
		,@(loop for option in syntax-clauses
			collect (process-option option 'readtable))
		readtable))))))))

(defmacro in-readtable (name)
  "Bind the *readtable* to the readtable referred to by NAME, raising
an error if no such readtable can be found."
  (check-type name symbol)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *readtable* (ensure-readtable ',name))
     (when (find-package :swank)
       (%frob-swank-readtable-alist *package* *readtable*))
     ))

;;; KLUDGE:
;;;   We need support for this in Slime itself, because we want IN-READTABLE
;;;   to work on a per-file basis, and not on a per-package basis.
;;; 
(defun %frob-swank-readtable-alist (package readtable)
  (let ((readtable-alist (find-symbol (string '#:*readtable-alist*) 
				      (find-package :swank))))
    (when (boundp readtable-alist)
      (pushnew (cons (package-name package) readtable)
	       (symbol-value readtable-alist)
	       :test #'(lambda (entry1 entry2)
			 (destructuring-bind (pkg-name1 . rt1) entry1
			   (destructuring-bind (pkg-name2 . rt2) entry2
			     (and (string= pkg-name1 pkg-name2)
				  (eq rt1 rt2)))))))))


(defun make-readtable (name &key merge)
  "Makes and returns a new readtable under the specified NAME. The
keyarg :MERGE takes a list of named-readtable-designators and specifies
the readtables the new readtable is created from. (See the :MERGE
clauses of DEFREADTABLE for details.) If :MERGE wasn't given (or NIL),
the :STANDARD readtable is used instead."
  (let ((merge-list (or merge '(:standard))))
    (check-type merge-list list)
    (cond ((reserved-readtable-name-p name)
	   (error "~A is the designator for a predefined readtable. ~
                   Not acceptable as a user-specified readtable name." name))
	  ((let ((rt (find-readtable name)))
	     (and rt (cerror "Overwrite existing readtable." 
			     'readtable-does-already-exist :readtable rt))))
	  (t (let ((result (apply #'merge-readtables-into
				  ;; The first readtable specified in the :merge list is
				  ;; taken as the basis for all subsequent (destructive!)
				  ;; modifications (and hence it's copied.)
				  (copy-readtable (ensure-readtable (first merge-list)))
				  (rest merge-list))))
	       (register-readtable name result))))))

(defun rename-readtable (named-readtable-designator new-name)
  "Replaces the associated name of the readtable designated by
NAMED-READTABLE-DESIGNATOR with NEW-NAME If a readtable is already
registered under the new name, an error is raised."
  (check-type named-readtable-designator named-readtable-designator)
  (check-type new-name symbol)
  (when (find-readtable new-name)
    (error "A readtable named ~S already exists." new-name))
  (let* ((read-table (ensure-readtable named-readtable-designator))
	 (read-table-name (readtable-name read-table)))
    ;; We use the internal functions directly to omit repeated
    ;; type-checking.
    (%unassociate-name-from-readtable read-table-name read-table)
    (%unassociate-readtable-from-name read-table-name read-table)
    (%associate-name-with-readtable new-name read-table)
    (%associate-readtable-with-name new-name read-table)
    read-table))

(defun merge-readtables-into (result-table &rest named-readtable-designators)
  "Copy the contents of each readtable in NAMED-READTABLE-DESIGNATORS,
in turn, into RESULT-TABLE.  Because the readtables are merged in
turn, macro definitions in readtables later in the list will overwrite
definitions in readtables listed earlier.  Notice that the /readtable
case/ is also subject of the merge operation."
  (flet ((merge-into (rt1 rt2)
	   (with-readtable-iterator (rt2-iter rt2)
	     (loop
	      (multiple-value-bind (more? char reader-fn disp? table) (rt2-iter)
		(unless more? (return))
		(unless (standard-macro-char-p char rt2)
		  (let ((non-terminating-p (nth-value 1 (get-macro-character char rt2))))
		    (set-macro-character char reader-fn non-terminating-p rt1)))
		(when disp?
		  (loop for (subchar . subfn) in table do
			(unless (standard-dispatch-macro-char-p char subchar rt2)
			  (set-dispatch-macro-character char subchar subfn rt1)))))))
	   rt1))
    (setf result-table (ensure-readtable result-table))
    (dolist (table (mapcar #'ensure-readtable named-readtable-designators))
      (merge-into result-table table))
    result-table))

(defun list-all-named-readtables ()
  "Returns a list of all registered readtables. The returned list is
guaranteed to be fresh, but may contain duplicates."
  (mapcar #'ensure-readtable (%list-all-readtable-names)))


(deftype readtable-designator ()
  `(or null readtable))

(deftype named-readtable-designator ()
  `(or readtable-designator symbol))


(define-condition readtable-error (error) ())

(define-condition readtable-does-not-exist (readtable-error)
  ((readtable-name :initarg :readtable-name 
	           :initform (required-argument)
	           :accessor missing-readtable-name)))

(define-condition readtable-does-already-exist (readtable-error)
  ((readtable-name :initarg :readtable 
		   :initform (required-argument)
		   :accessor existing-readtable-name))
  (:report (lambda (condition stream)
             (format stream "A readtable named ~S already exists."
                     (existing-readtable-name  condition)))))


;;; Although there is no way to get at the standard readtable in
;;; Common Lisp (cf. /standard readtable/, CLHS glossary), we make
;;; up the perception of its existence by interning a copy of it.
;;;
;;; We do this for reverse lookup (cf. READTABLE-NAME), i.e. for
;;;
;;;   (equal (readtable-name (find-readtable :standard)) "STANDARD")
;;;
;;; holding true.
;;;
;;; We, however, inherit the restriction that the :STANDARD
;;; readtable _must not be modified_ (cf. CLHS 2.1.1.2), although it'd
;;; technically be feasible (as *STANDARD-READTABLE* will contain a
;;; mutable copy of the implementation-internal standard readtable.)
;;; We cannot enforce this restriction without shadowing
;;; CL:SET-MACRO-CHARACTER and CL:SET-DISPATCH-MACRO-FUNCTION which
;;; is out of scope of this library, though. So we just threaten
;;; with nasal demons.
;;;
(defvar *standard-readtable* (%standard-readtable))

(defun standard-macro-char-p (char rt)
  (multiple-value-bind (rt-fn rt-flag) (get-macro-character char rt)
    (multiple-value-bind (std-fn std-flag) (get-macro-character char *standard-readtable*)
      (and (eq rt-fn std-fn)
	   (eq rt-flag std-flag)))))

(defun standard-dispatch-macro-char-p (disp-char sub-char rt)
  (flet ((non-terminating-p (ch rt) (nth-value 1 (get-macro-character ch rt))))
    (and (eq (non-terminating-p disp-char rt)
	     (non-terminating-p disp-char *standard-readtable*))
	 (eq (get-dispatch-macro-character disp-char sub-char rt)
	     (get-dispatch-macro-character disp-char sub-char *standard-readtable*)))))

(defparameter *reserved-readtable-names* '(nil :standard :current))

(defun reserved-readtable-name-p (name)
  (and (member name *reserved-readtable-names*) t))

;;; In principle, we could DEFREADTABLE :STANDARD. But we do reserved
;;; readtable lookup seperately, since we can't register a readtable
;;; for :CURRENT anyway.

(defun find-reserved-readtable (reserved-name)
  (cond ((eq reserved-name nil)       *standard-readtable*)
	((eq reserved-name :standard) *standard-readtable*)
	((eq reserved-name :current)  *readtable*)
	(t (error "No such reserved readtable: ~S" reserved-name))))

(defun find-readtable (named-readtable-designator)
  "Looks for the readtable specified by NAMED-READTABLE-DESIGNATOR and 
returns it if it is found. Returns NIL otherwise."
  (check-type named-readtable-designator named-readtable-designator)
  (symbol-macrolet ((designator named-readtable-designator))
    (cond ((readtablep designator) designator)
	  ((reserved-readtable-name-p designator)
	   (find-reserved-readtable designator))
	  ((%find-readtable designator)))))

;;; FIXME: This doesn't take a NAMED-READTABLE-DESIGNATOR, but only a
;;; STRING-DESIGNATOR. (When fixing, heed interplay with compiler
;;; macros below.)
(defsetf find-readtable register-readtable)

(defun ensure-readtable (named-readtable-designator &optional (default nil default-p))
  "Looks up the readtable specified by NAMED-READTABLE-DESIGNATOR and
returns it if it is found.  If it is not found, it registers the
readtable designated by DEFAULT under the name represented by
NAMED-READTABLE-DESIGNATOR. If no default argument was given, an error
is signalled."
  (symbol-macrolet ((designator named-readtable-designator))
    (cond ((find-readtable designator))
	  ((not default-p)
	   (error "The name ~S does not designate any readtable." designator))
	  (t (setf (find-readtable designator) (ensure-readtable default))))))


(defun register-readtable (name read-table)
  "Associate READ-TABLE as the readtable associated with NAME."
  (check-type name symbol)
  (check-type read-table readtable)
  (check-type name (not (satisfies reserved-readtable-name-p)))
  (%associate-readtable-with-name name read-table)
  (%associate-name-with-readtable name read-table)
  read-table)

(defun unregister-readtable (named-readtable-designator)
  "Remove the readtable association of NAMED-READTABLE-DESIGNATOR.
Returns T if successfull, NIL otherwise."
  (let* ((read-table (ensure-readtable named-readtable-designator))
	 (read-table-name (readtable-name read-table)))
    (if (not read-table-name)
	nil
	(prog1 t
	  (check-type read-table-name (not (satisfies reserved-readtable-name-p)))
            (%unassociate-readtable-from-name read-table-name read-table)
            (%unassociate-name-from-readtable read-table-name read-table)))))

(defun readtable-name (named-readtable-designator)
  "Returns the name of the readtable designated by
NAMED-READTABLE-DESIGNATOR or NIL."
  (check-type named-readtable-designator named-readtable-designator)
  (let ((read-table (ensure-readtable named-readtable-designator)))
    (cond ((eq read-table *readtable*)          :current)
	  ((eq read-table *standard-readtable*) :standard)
	  (t (%readtable-name read-table)))))


;;;;; Compiler macros

;;; Since the :STANDARD read-table is interned, and we can't enforce
;;; its immutability, we signal a style-warning for suspicious uses
;;; that may result in strange behaviour:

;;; Modifying the standard readtable would, obviously, lead to a
;;; propagation of this change to all places which use the :STANDARD
;;; readtable (and thus rendering this readtable to be non-standard,
;;; in fact.)


(defun constant-standard-readtable-expression-p (thing)
  (cond ((symbolp thing) (or (eq thing 'nil) (eq thing :standard)))
	((consp thing)   (some (lambda (x) (equal thing x))
			       '((find-readtable nil)
				 (find-readtable :standard)
				 (ensure-readtable nil)
				 (ensure-readtable :standard))))
	(t nil)))

(defun signal-suspicious-registration-warning (name-expr read-table-expr)
  (simple-style-warn
   "Caution: ~<You're trying to register the :STANDARD readtable ~
    under a new name ~S. As modification of the :STANDARD readtable ~
    is not permitted, subsequent modification of ~S won't be ~
    permitted either. You probably want to wrap COPY-READTABLE ~
    around~@:>~%             ~S"
   (list name-expr name-expr) read-table-expr))

(let ()
  ;; Defer to runtime because compiler-macros are made available already
  ;; at compilation time. So without this two subsequent invocations of
  ;; COMPILE-FILE on this file would result in an undefined function
  ;; error because the two above functions are not yet available.
  ;; (This does not use EVAL-WHEN because of Fig 3.7, CLHS 3.2.3.1;
  ;; cf. last example in CLHS "EVAL-WHEN" entry.)
  
  (define-compiler-macro register-readtable (&whole form name read-table)
    (when (constant-standard-readtable-expression-p read-table)
      (signal-suspicious-registration-warning name read-table))
    form)

  (define-compiler-macro ensure-readtable (&whole form name &optional (default nil default-p))
    (when (and default-p (constant-standard-readtable-expression-p default))
      (signal-suspicious-registration-warning name default))
    form))


