;;; This deals with declarations (let & letrec).  The input is a list of
;;; declarations (valdefs) which may contain recursive-decl-groups, as
;;; introduced in dependency analysis.  This function alters the list
;;; of non-generic type variables.  Expressions containing declarations
;;; need to rebind the non-generic list around the decls and all expressions
;;; within their scope.

;;; This returns an updated decl list with recursive decl groups removed.

(define (type-decls decls)
  (cond ((null? decls)
	 '())
	((is-type? 'recursive-decl-group (car decls))
	 (let ((d (recursive-decl-group-decls (car decls))))
 	   (type-recursive d)
	   (append d (type-decls (cdr decls)))))
	(else
	 (type-non-recursive (car decls))
	 (cons (car decls)
	       (type-decls (cdr decls))))))

;;; This typechecks a mutually recursive group of declarations (valdefs).
;;; Generate a monomorphic variable for each declaration and unify it with
;;; the lhs of the decl.  The variable all-vars collects all variables defined
;;; by the declaration group.  Save the values of placeholders and ng-list
;;; before recursing.

;;; The type of each variable is marked as recursive.

(define (type-recursive decls)
  (let ((old-ng (dynamic *non-generic-tyvars*))
	(old-placeholders (dynamic *placeholders*))
	(all-vars '())
	(new-tyvars '())
	(decls+tyvars '()))
    ;; on a type error set all types to `a' and give up.
    (setf (dynamic *placeholders*) '())
    (recover-type-error 
       (lambda (r)
	 (make-dummy-sigs decls)
	 (setf (dynamic *dict-placeholders*) old-placeholders)
	 (funcall r))
       ;; Type the lhs of each decl and then mark each variable bound
       ;; in the decl as recursive.
       (dolist (d decls)
        (fresh-type lhs-type
	  (push lhs-type (dynamic *non-generic-tyvars*))
	  (push lhs-type new-tyvars)
	  (type-decl-lhs d lhs-type)
	  (push (tuple d lhs-type) decls+tyvars))
	(dolist (var-ref (collect-pattern-vars (valdef-lhs d)))
	  (let ((var (var-ref-var var-ref)))
	    (push var all-vars)
	    (setf (var-type var)
		  (make recursive-type (type (var-type var))
			(placeholders '()))))))

;;; This types the decl right hand sides.  Each rhs type is unified with the
;;; tyvar corresponding to the lhs.  Before checking the signatures, the
;;; ng-list is restored.  

       (dolist (d decls+tyvars)
	 (let ((rhs-type (type-decl-rhs (tuple-2-1 d)))
	       (lhs-type (tuple-2-2 d)))
	   (type-unify lhs-type rhs-type
		 (type-mismatch (tuple-2-1 d)
			  "Decl type mismatch" lhs-type rhs-type))))
       (setf (dynamic *non-generic-tyvars*) old-ng)
       (let ((sig-contexts (check-user-signatures all-vars)))

;;; This generalizes the signatures of recursive decls.  First, the
;;; context of the declaration group is computed.  Any tyvar in the
;;; bodies with a non-empty context must appear in all signatures that
;;; are non-ambiguous.
	  
	 (let* ((all-tyvars (collect-tyvars/l new-tyvars))
		(overloaded-tyvars '()))
	   (dolist (tyvar all-tyvars)
	      (when (and (ntyvar-context tyvar) (not (non-generic? tyvar)))
		 (push tyvar overloaded-tyvars)))
	   (reconcile-sig-contexts overloaded-tyvars sig-contexts)
	 ;; We should probably also emit a warning about inherently
	 ;; ambiguous decls.
	   (when (and overloaded-tyvars
		      (apply-pattern-binding-rule? decls))
		 (setf (dynamic *non-generic-tyvars*)
		       (do-pattern-binding-rule
			decls overloaded-tyvars old-ng))
		 (setf overloaded-tyvars '()))
	 ;; The next step is to compute the signatures of the defined
	 ;; variables and to define all recursive placeholders.  When
	 ;; there is no context the placeholders become simple var refs.
	 ;; and the types are simply converted.
	   (cond ((null? overloaded-tyvars)
		  (dolist (var all-vars)
		    (let ((r (var-type var)))
		      (setf (var-type var) (recursive-type-type (var-type var)))
		      (dolist (p (recursive-type-placeholders r))
		        (setf (recursive-placeholder-exp p)
			      (**var/def var)))
		      (generalize-type var))))
	 ;; When the declaration has a context things get very hairy.
	 ;; First, grap the recursive placeholders before generalizing the
	 ;; types.
		 (else
		  ;; Mark the overloaded tyvars as read-only.  This prevents
		  ;; signature unification from changing the set of tyvars
		  ;; defined in the mapping.
		  (dolist (tyvar overloaded-tyvars)
		     (setf (ntyvar-read-only? tyvar) '#t))
		  (let ((r-placeholders '()))
		    (dolist (var all-vars)
		     (let ((rt (var-type var)))
		      (dolist (p (recursive-type-placeholders rt))
			(push p r-placeholders))
		      (setf (var-type var) (recursive-type-type rt))))
	 ;; Now compute a signature for each definition and do dictionary
	 ;; conversion.  The var-map defines the actual parameter associated
	 ;; with each of the overloaded tyvars.
		    (let ((var-map (map (lambda (decl)
					 (tuple (decl-var decl)
					  (generalize-overloaded-type
					   decl overloaded-tyvars)))
					decls)))
	 ;; Finally discharge each recursive placeholder.
		      (dolist (p r-placeholders)
			(let ((ref-to (recursive-placeholder-var p))
			      (decl-from
			       (search-enclosing-decls
				 (recursive-placeholder-enclosing-decls p)
				 decls)))
			  (setf (recursive-placeholder-exp p)
				(recursive-call-code decl-from ref-to var-map)))
			)))))
	   (setf (dynamic *placeholders*)
		 (process-placeholders
		  (dynamic *placeholders*) old-placeholders decls)))))))

;;; Non-recursive decls are easier.  Save the placeholders, use a fresh type
;;; for the left hand side, check signatures, and generalize.

(define (type-non-recursive decl)
 (remember-context decl
  (fresh-type lhs-type
    (let ((old-placeholders (dynamic *placeholders*))
	  (all-vars (map (lambda (x) (var-ref-var x))
			    (collect-pattern-vars (valdef-lhs decl)))))
     (setf (dynamic *placeholders*) '())
     (recover-type-error
      (lambda (r)
        (make-dummy-sigs (list decl))
	(setf (dynamic *placeholders*) old-placeholders)
        (funcall r))
      (type-decl-lhs decl lhs-type)
      (let ((rhs-type (type-decl-rhs decl)))
	(type-unify lhs-type rhs-type
           (type-mismatch decl
	       "Decl type mismatch" lhs-type rhs-type)))
      (check-user-signatures all-vars)
      (let ((all-tyvars (collect-tyvars lhs-type))
	    (overloaded-tyvars '()))
	(dolist (tyvar all-tyvars)
	  (when (ntyvar-context tyvar)
	     (push tyvar overloaded-tyvars)))
	(when (and overloaded-tyvars
		   (apply-pattern-binding-rule? (list decl)))
	 (setf (dynamic *non-generic-tyvars*)
	   (do-pattern-binding-rule
	    (list decl) overloaded-tyvars (dynamic *non-generic-tyvars*)))
	 (setf overloaded-tyvars '()))
	(if (null? overloaded-tyvars)
	    (dolist (var all-vars)
	      (generalize-type var))
	    (generalize-overloaded-type decl '()))
	(setf (dynamic *placeholders*)
	      (process-placeholders
	       (dynamic *placeholders*) old-placeholders (list decl)))))))))

;;; These functions type check definition components.

;;; This unifies the type of the lhs pattern with a type variable.

(define (type-decl-lhs object type)
 (dynamic-let ((*enclosing-decls* (cons object (dynamic *enclosing-decls*))))
  (remember-context object
   (type-check valdef lhs pat-type
    (type-unify type pat-type #f)))))


;;; This types the right hand side.  The *enclosing-decls* variable is
;;; used to keep track of which decl the type checker is inside.  This
;;; is needed for both defaulting (to find which module defaults apply)
;;; and recursive types to keep track of the dictionary parameter variables
;;; for recursive references.

(define (type-decl-rhs object)
 (dynamic-let ((*enclosing-decls* (cons object (dynamic *enclosing-decls*))))
  (remember-context object
   (type-check/unify-list valdef definitions res-type
       (type-mismatch/list object
	   "Right hand sides have different types")
       res-type))))


;;; This is similar to typing lambda.

(define-type-checker single-fun-def
  (fresh-monomorphic-types (length (single-fun-def-args object)) tyvars
    (type-check/list single-fun-def args arg-types
      (unify-list tyvars arg-types)
      (type-check/decls single-fun-def where-decls
        (type-check/unify-list single-fun-def rhs-list rhs-type
           (type-mismatch/list object
			"Bodies have incompatible types")
	  (return-type object (**arrow/l-2 arg-types rhs-type)))))))


;;; These functions are part of the generalization process.

;;; This function processes user signature declarations for the set of
;;; variables defined in a declaration.  Since unification of one signature
;;; may change the type associated with a previously verified signature,
;;; signature unification is done twice unless only one variable is
;;; involved.  The context of the signatures is returned to compare
;;; with the overall context of the declaration group.

(define (check-user-signatures vars)
  (cond ((null? (cdr vars))
	 (let* ((var (car vars))
		(sig (var-signature var)))
	   (if (eq? sig '#f)
	       '()
	       (list (tuple var (check-var-signature var sig))))))
	(else
	 (let ((sigs '()))
	   (dolist (var vars)
	     (let ((sig (var-signature var)))
	       (unless (eq? sig '#f)
		 (check-var-signature var sig))))
	   (dolist (var vars)
	     (let ((sig (var-signature var)))
	       (unless (eq? sig '#f)
		 (push (tuple var (check-var-signature var sig)) sigs))))
	   sigs))))


(define (check-var-signature var sig)
  (mlet (((sig-type sig-vars) (instantiate-gtype/newvars sig)))
    (dolist (tyvar sig-vars)
      (setf (ntyvar-read-only? tyvar) '#t))
    (type-unify (remove-recursive-type (var-type var)) sig-type
	     (signature-mismatch var))
    (dolist (tyvar sig-vars)
      (setf (ntyvar-read-only? tyvar) '#f))
    sig-vars))
  
;;; Once the declaration context is computed, it must be compared to the
;;; contexts given by the user.  All we need to check is that all tyvars
;;; constrained in the user signatures are also in the decl-context.
;;; All user supplied contexts are correct at this point - we just need
;;; to see if some ambiguous portion of the context exists.

;;; This error message needs work.  We need to present the contexts.

(define (reconcile-sig-contexts overloaded-tyvars sig-contexts)
  (dolist (sig sig-contexts)
    (let ((sig-vars (tuple-2-2 sig)))
      (dolist (d overloaded-tyvars)
	(when (not (memq d sig-vars))
	  (type-error
"Declaration signature has insufficiant context in declaration~%~A~%"
            (tuple-2-1 sig)))))))

;;; This is used for noisy type inference

(define (report-typing var)
 (when (memq 'type (dynamic *printers*))
  (let* ((name (symbol->string (def-name var))))
    (when (not (or (string-starts? "sel-" name)
		   (string-starts? "i-" name)
		   (string-starts? "default-" name)
		   (string-starts? "dict-" name)))
      (format '#t "~A :: ~A~%" var (var-type var))))))

;;; This is used during error recovery.  When a type error occurs, all
;;; variables defined in the enclosing declaration are set to type `a'
;;; and typing is resumed.

(define (make-dummy-sigs decls)
  (let ((dummy-type (make gtype (context '(()))
			        (type (**gtyvar 0)))))
    (dolist (d decls)
      (dolist (var-ref (collect-pattern-vars (valdef-lhs d)))
        (let ((var (var-ref-var var-ref)))
	  (setf (var-type var) dummy-type))))))


;;; This is used to generalize the variable signatures.  If there is
;;; an attached signature, the signature is used.  Otherwise the ntype
;;; is converted to a gtype.

(define (generalize-type var)
  (if (eq? (var-signature var) '#f)
      (setf (var-type var) (ntype->gtype (var-type var)))
      (setf (var-type var) (var-signature var)))
  (report-typing var))
      
;;; For overloaded types, it is necessary to map the declaration context
;;; onto the generalized type.  User signatures may provide different but
;;; equivilant contexts for different declarations in a decl goup.

;;; The overloaded-vars argument allows ambiguous contexts.  This is not
;;; needed for non-recursive vars since the context cannot be ambiguous.

(define (generalize-overloaded-type decl overloaded-vars)
  (let* ((var (decl-var decl))
	 (sig (var-signature var))
	 (new-tyvars '()))
    (cond ((eq? sig '#f)
	   (mlet (((gtype tyvars)
		   (ntype->gtype/env (var-type var) overloaded-vars)))
	      (setf (var-type var) gtype)
	      (setf new-tyvars tyvars)))
	  (else
	   (mlet (((ntype tyvars) (instantiate-gtype/newvars sig)))
	     (unify ntype (var-type var))
	     (setf (var-type var) sig)
	     (setf new-tyvars (prune/l tyvars)))))
    (report-typing var)
    (dictionary-conversion/definition decl new-tyvars)
    new-tyvars))

(define (remove-recursive-type ty)
  (if (recursive-type? ty)
      (recursive-type-type ty)
      ty))

