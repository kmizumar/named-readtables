
;;; Copyright (c) 2007, Tobias C. Rittweiler <tcr@freebits.de>,
;;;                     Robert P. Goldman <rpgoldman@sift.info> and SIFT, LLC
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;;     * Redistributions of source code must retain the above copyright
;;;       notice, this list of conditions and the following disclaimer.
;;;     * Redistributions in binary form must reproduce the above copyright
;;;       notice, this list of conditions and the following disclaimer in the
;;;       documentation and/or other materials provided with the distribution.
;;;     * Neither the names of Tobias C. Rittweiler, Robert P. Goldman, SIFT, LLC nor the
;;;       names of its contributors may be used to endorse or promote products
;;;       derived from this software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY Tobias C. Rittweiler, Robert
;;; P. Goldman and SIFT, LLC ``AS IS'' AND ANY EXPRESS OR IMPLIED
;;; WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;; DISCLAIMED. IN NO EVENT SHALL Tobias C. Rittweiler, Robert
;;; P. Goldman or SIFT, LLC BE LIABLE FOR ANY DIRECT, INDIRECT,
;;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
;;; OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
;;; EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :editor-hints.named-readtables)

;;;
;;;  ``This is enough of a foothold to implement a more elaborate
;;;    facility for using readtables in a localized way.''
;;;
;;;                               (X3J13 Cleanup Issue IN-SYNTAX)
;;;


;;;;;; DEFREADTABLE &c.

(defmacro defreadtable (string-designator &rest options)
  "Define a new named readtable, whose name is given by STRING-DESIGNATOR.
Or, if a readtable is already registered under that name, redefines that one.

The readtable can be populated using the OPTIONS &rest argument, as follows:

  (:MERGE &REST READTABLE-DESIGNATORS) -- merge the readtables designated into
      the new readtable, using MERGE-READTABLES-INTO.  It is mandatory to supply 
      at least one :MERGE option naming a readtable to incorporate.
 
      Defreadtable accepts some special readtable designators, including
      NIL or :STANDARD for the standard readtable and :CURRENT for the current
      readtable, as well as the names of programmer-defined readtables.

      Note that the process of merging readtables is _not_ commutative, so that 
      macros in later entries will overwrite earlier ones, left to right in a single
      :MERGE directive, and one after another across multiple :MERGE directives.

  (:DISPATCH-MACRO-CHAR MACRO-CHAR SUB-CHAR FUNCTION) -- define a new
      dispatch macro character in the readtable, per SET-DISPATCH-MACRO-CHARACTER.

  (:MACRO-CHAR MACRO-CHAR FUNCTION &OPTIONAL NON-TERMINATING-P) -- define a 
      new macro character in the readtable, per SET-MACRO-CHARACTER.

  (:CASE CASE-MODE) -- defines the /case sensititivy mode/ of the resulting
      readtable.

  Any number of option clauses may appear.  The :merge directives are evaluated first,
then the :case directives are evaluated in the order they appear. At last the local
macro definitions in :dispatch-macro-char and :macro-char are evaluated in the order
they appear."
  (check-type string-designator string-designator)
  (when (reserved-readtable-name-p string-designator)
    (error "~A is the designator for a predefined readtable. ~
            Not acceptable as a user-specified readtable name." string-designator))
  (flet ((process-option (option var)
           (destructure-case option
             ((:merge &rest readtable-designators)
	      `(merge-readtables-into ,var ,@readtable-designators))
             ((:dispatch-macro-char disp-char sub-char function)
              `(set-dispatch-macro-character ,disp-char ,sub-char ,function ,var))
             ((:macro-char char function &optional non-terminating-p)
              `(set-macro-character ,char ,function ,non-terminating-p ,var))
	     ((:case mode)
	      `(setf (readtable-case ,var) ,mode))))
	 (member-of (&rest rest) #'(lambda (x) (member x rest))))
    (let* ((merge-clauses (remove-if-not (member-of :merge) options :key #'first))
	   (case-clauses  (remove-if-not (member-of :case)  options :key #'first))
	   (macro-clauses (remove-if-not (member-of :macro-char
						    :dispatch-macro-char) options
					 :key #'first))
	   (other-clauses (set-difference options (append merge-clauses case-clauses macro-clauses))))
      (cond ((null merge-clauses)
	     (error "Not at least one (:merge ...) clause given in DEFREADTABLE form."))
	    ((not (null other-clauses))
	     (error "Bogus DEFREADTABLE clauses: ~{~A~^, ~}" other-clauses))
	    (t
	     `(eval-when (:compile-toplevel :load-toplevel :execute)
		;; The (FIND-READTABLE ...) is important for proper redefinition semantics.
		(let ((read-table (find-readtable ,string-designator)))
		  (if read-table
		      (simple-style-warn "redefining ~A in ~A" ,string-designator 'defreadtable)
		      (setq read-table
			    (make-readtable ,string-designator
					    ;; We have to provide an explicit :MERGE argument,
					    ;; because otherwise the :STANDARD readtable would
					    ;; be used which is not necessarily what we want.
					    :merge ',(rest (first merge-clauses)))))
		  ;; We have to grovel all MERGE-CLAUSES for the redefinition case.
		  ,@(loop for option in merge-clauses
			  collect (process-option option 'read-table))
		  ,@(loop for option in case-clauses
			  collect (process-option option 'read-table))
		  ,@(loop for option in macro-clauses
			  collect (process-option option 'read-table))
		  read-table)))))))

(defmacro in-readtable (string-designator)
  "Bind the *readtable* to the readtable referred to by
STRING-DESIGNATOR, raising an error if no such readtable can
be found."
  (check-type string-designator string-designator)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *readtable* (ensure-readtable ,string-designator))))

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
	  ((and (find-readtable name)
		(cerror "Overwrite existing readtable."
			"A readtable named ~S already exists." name)))
	  (t (let ((result (apply #'merge-readtables-into
				  ;; The first readtable specified in the :merge list is
				  ;; taken as the basis for all subsequent (destructive!)
				  ;; modifications (and hence it's copied.)
				  (copy-readtable (ensure-readtable (first merge-list)))
				  (rest merge-list))))
	       (register-readtable name result))))))

(defun rename-readtable (named-readtable-designator string-designator)
  "Replaces the associated name of the readtable designated by
NAMED-READTABLE-DESIGNATOR with STRING-DESIGNATOR. If a readtable is
already registered under the new name, an error is raised."
  (check-type named-readtable-designator named-readtable-designator)
  (check-type string-designator string-designator)
  (when (find-readtable string-designator)
    (error "A readtable named ~S already exists." string-designator))
  (let* ((read-table (ensure-readtable named-readtable-designator))
	 (read-table-name (readtable-name read-table)))
    ;; We use the internal functions directly to omit repeated
    ;; type-checking.
    (%unregister-readtable-name read-table)
    (%unregister-readtable read-table-name)
    (%register-readtable-name string-designator read-table)
    (%register-readtable string-designator read-table)
    read-table))

(defun merge-readtables-into (result-table &rest named-readtable-designators)
  "Copy the contents of each readtable in NAMED-READTABLE-DESIGNATORS,
in turn, into RESULT-TABLE.  Because the readtables are merged in
turn, macro definitions in readtables later in the list will overwrite
definitions in readtables listed earlier.  Notice that the /readtable
case/ is also subject of the merge operation."
  (flet ((merge-into (rt1 rt2)
	   (%with-readtable-iterator (rt2-iter rt2)
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


(deftype string-designator ()
  `(or string symbol character))

(deftype readtable-designator ()
  `(or null readtable))

(deftype named-readtable-designator ()
  `(or readtable-designator string-designator))

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
  (and (member (string name) *reserved-readtable-names*
	       :test #'string= :key #'symbol-name)
       t))

;;; In principle, we could DEFREADTABLE :STANDARD. But we do reserved
;;; readtable lookup seperately, since we can't register a readtable
;;; for :CURRENT anyway.

(defun find-reserved-readtable (reserved-name)
  (cond ((string= reserved-name nil)       *standard-readtable*)
	((string= reserved-name :standard) *standard-readtable*)
	((string= reserved-name :current)  *readtable*)
	(t (error "No such reserved readtable: ~S" reserved-name))))

(defun find-readtable (named-readtable-designator)
  "Looks for the readtable specified by NAMED-READTABLE-DESIGNATOR and 
returns it if it is found. Returns NIL otherwise."
  (check-type named-readtable-designator named-readtable-designator)
  (symbol-macrolet ((designator named-readtable-designator))
    (cond ((readtablep designator) designator)
	  ((reserved-readtable-name-p designator)
	   (find-reserved-readtable designator))
	  ((%find-readtable-from-name designator)))))

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


(defun register-readtable (string-designator read-table)
  "Associate READ-TABLE as the readtable associated with STRING-DESIGNATOR."
  (check-type string-designator string-designator)
  (check-type read-table readtable)
  (check-type string-designator (not (satisfies reserved-readtable-name-p)))
  (%register-readtable-name string-designator read-table)
  (%register-readtable string-designator read-table)
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
	  (%unregister-readtable-name read-table)
	  (%unregister-readtable read-table-name)))))

(defun readtable-name (named-readtable-designator)
  "Returns the name (as string) of the readtable designated by
NAMED-READTABLE-DESIGNATOR or NIL."
  (check-type named-readtable-designator named-readtable-designator)
  (let ((read-table (ensure-readtable named-readtable-designator)))
    (cond ((eq read-table *readtable*)          "CURRENT")
	  ((eq read-table *standard-readtable*) "STANDARD")
	  (t (%readtable-name read-table)))))


;;;;; Compiler macros

;;; Since the :STANDARD read-table is interned, and we can't enforce
;;; its immutability, we signal a style-warning for suspicious uses
;;; that may result in strange behaviour:

;;; Modifying the standard readtable would, obviously, lead to a
;;; propagation of this change to all places which use the :STANDARD
;;; readtable (and thus rendering this readtable to be non-standard,
;;; in fact.)

(define-condition simple-style-warning (style-warning simple-warning)
  ())

(defun simple-style-warn (format-control &rest format-args)
  (warn 'simple-style-warning
	 :format-control format-control
	 :format-arguments format-args))

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

(define-compiler-macro register-readtable (&whole form name read-table)
  (when (constant-standard-readtable-expression-p read-table)
    (signal-suspicious-registration-warning name read-table))
  form)

(define-compiler-macro ensure-readtable (&whole form name &optional (default nil default-p))
  (when (and default-p (constant-standard-readtable-expression-p default))
    (signal-suspicious-registration-warning name default))
  form)


