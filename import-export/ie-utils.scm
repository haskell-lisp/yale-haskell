
;;; This file contains utilities, globals, and macros used by the
;;; import-export system.

(define *new-exports-found?* '#f)  ; used by the fixpoint iteration

;;; A group is a collection of related symbols.  It is represented
;;; by a list of (name,def) pairs.  The first element is the head
;;; of the group; the group is entered in the export table under the
;;; name of the head only.  Groups for vars and synonyms have only the
;;; head.  Data types and classes have the constructors or methods in
;;; the tail of the group.

(define (group-name x)  ; name of the head
  (tuple-2-1 (car x)))

(define (group-definition x) ; definition of the head
  (tuple-2-2 (car x)))

;;; The name & entry are the head of the group.  Others is a list of
;;; name - definition pairs.
(define (make-group name entry . others)
  (if (null? others)
      (list (cons name entry))
      (cons (cons name entry) (car others))))

(define (hidden-constructors? group)
  (null? (cdr group)))

(define (strip-constructors group)
  (list (car group)))

;;; rename-group applies the current renaming  to every
;;;  name in a group.  When uses, a renaming is marked to allow unused
;;;  renamings to be detected.

(define (rename-group g renamings)
  (if (null? renamings)
      g
      (map (lambda (n-d)
	     (let* ((def (tuple-2-2 n-d))
		    (keep-name? (or (con? def) (var? def)))
		    (n (tuple-2-1 n-d))
		    (name (if keep-name? n (add-con-prefix/symbol n)))
		    (renaming (locate-renaming name renamings)))
	       (cond (renaming
		      (let ((new-name
			     (if keep-name?
				 (renaming-to renaming)
				 (remove-con-prefix/symbol
				   (renaming-to renaming)))))
			(when (and (def-prelude? def)
				   (not (eq? (def-name def) new-name)))
			    (signal-prelude-renaming def new-name)
			    (setf new-name (def-name def)))
			(setf (renaming-referenced? renaming) '#t)
			(tuple new-name def)))
		     (else n-d))))
	   g)))

(define (locate-renaming name renamings)
  (if (null? renamings)
      '#f
      (if (eq? name (renaming-from (car renamings)))
	  (car renamings)
	  (locate-renaming name (cdr renamings)))))

(define (gather-algdata-group name def)
  (cons (tuple name def)
	(gather-group (algdata-constrs def))))

(define (gather-class-group name def)
  (cons (tuple name def)
	(gather-group (class-method-vars def))))

(define (gather-group defs)
  (if (null? defs)
      '()
      (let ((local-name (local-name (car defs))))
	(if (eq? local-name '#f)
	    '()
	    (cons (tuple local-name (car defs))
		  (gather-group (cdr defs)))))))

;;; These deal with `hiding' lists.

;;; Note: as per the new report, no need to worry about anything but the
;;; group head and the entity name since only var, Class(..),Alg(..) allowed

(define (in-hiding-list? group hiding)
  (cond ((null? hiding)
	 '#f)
	((eq? (entity-name (car hiding)) (group-name group))
	 '#t)
	(else (in-hiding-list? group (cdr hiding)))))

(define (remove-entity group hiding)
  (cond ((eq? (entity-name (car hiding)) (group-name group))
	 (cdr hiding))
	(else (cons (car hiding) (remove-entity group (cdr hiding))))))

;;; This moves fixity information to the local symbols.  This must be
;;; called after local symbols are installed but before imported
;;; symbols arrive.

(define (attach-fixities)
  (dolist (fixity-decl (module-fixities *module*))
    (let ((fixity (fixity-decl-fixity fixity-decl)))
      (dolist (op (fixity-decl-names fixity-decl))
        (let ((def (resolve-toplevel-name op)))
	  (cond ((or (eq? def '#f) (not (eq? *module-name* (def-module def))))
		 ;;; ***This is WRONG!  Inner fixities may be found.
		 (signal-non-local-fixity op))
		((var? def)
		 (setf (var-fixity def) fixity)
		 (setf (table-entry *fixity-table* op) fixity))
		((con? def)
		 (setf (con-fixity def) fixity)
		 (setf (table-entry *fixity-table* op) fixity))
		(else (signal-fixity-not-var/con op))))))))

