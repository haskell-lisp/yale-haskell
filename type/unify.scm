
;;; File: type/unify.scm   Author: John

;;; This is the basic unification algorithm used in type checking.

;;; Unification failure invokes the current type error handler

;;; Start by removing instantiated type variables from the type.

(define (unify type1 type2)
  (unify-1 (prune type1) (prune type2)))

;;; The only real tweak here is the read-only bit on type variables.
;;; The rule is that a RO tyvar can be unified only with a generic
;;; non-RO tyvar which has the same or more general context.

;;; Aside from this, this is standard unification except that context
;;; propagation is needed when a tyvar with a non-empty context is
;;; instantiated.

;;; If type2 is a tyvar and type1 is not they are switched.

(define (unify-1 type1 type2)
    (cond ((eq? type1 type2)  ;; this catches variable to variable unify
	   'OK)
	  ((ntyvar? type1)
	   (cond ((occurs-in-type type1 type2)
		  (type-error "Circular type: cannot unify ~A with ~A"
			      type1 type2))
		 ((ntyvar-read-only? type1)
		  (cond ((or (not (ntyvar? type2)) (ntyvar-read-only? type2))
			 (type-error
			  "Signature too general: cannot unify ~A with ~A"
			  type1 type2))
			(else
			 (unify-1 type2 type1))))
		 ((and (ntyvar? type2)
		       (ntyvar-read-only? type2)
		       (non-generic? type1))
		  (type-error
 "Type signature cannot be used: monomorphic type variables present."))
		 (else
		  (instantiate-tyvar type1 type2)
		  (let ((classes (ntyvar-context type1)))
		    (if (null? classes)
			'OK
			(propagate-contexts/ntype type1 type2 classes))))))
	  ((ntyvar? type2)
	   (unify-1 type2 type1))
	  ((eq? (ntycon-tycon type1) (ntycon-tycon type2))
	   (unify-list (ntycon-args type1) (ntycon-args type2)))
	  (else
	   (let ((etype1 (expand-ntype-synonym type1))
		 (etype2 (expand-ntype-synonym type2)))
	    (if (same-tycon? (ntycon-tycon etype1) (ntycon-tycon etype2))
		(unify-list (ntycon-args etype1) (ntycon-args etype2))
		;; This error message should probably show both the original
		;; and the expanded types for clarity.
		(type-error
		      "Type conflict: type ~A does not match ~A"
			    etype1 etype2))))))


(define-integrable (instantiate-tyvar tyvar val)
  (setf (ntyvar-value tyvar) val))

;;; This is needed since interface files may leave multiple def's
;;; for the same tycon sitting around.

(define (same-tycon? ty1 ty2)
  (or (eq? ty1 ty2)
      (and (eq? (def-name ty1) (def-name ty2))
	   (eq? (def-module ty1) (def-module ty2)))))


;;; unifies two lists of types pairwise.  Used for tycon args.

(define (unify-list args1 args2)
  (if (null? args1)
      'OK
      (begin (unify-list (cdr args1) (cdr args2))
	     (unify (car args1) (car args2)))))

;;; combines a list of types into a single type.  Used in constructs
;;; such as [x,y,z] and case expressions.

(define (unify-list/single-type types)
  (when (not (null? types))
    (let ((type (car types)))
      (dolist (type2 (cdr types))
        (unify type type2)))))

;;; This propagates the context from a just instantiated tyvar to the
;;; instantiated value.  If the value is a tycon, instances must be
;;; looked up.  If the value is a tyvar, the context is added to that of
;;; other tyvar.

;;; This is used to back out of the unification on errors.  This is a
;;; poor mans trail stack!  Without this, error messages get very
;;; obscure.

(define *instantiated-tyvar* '())

(define (propagate-contexts/ntype tyvar type classes)
 (dynamic-let ((*instantiated-tyvar* tyvar))
    (propagate-contexts/inner type classes)))

(define (propagate-contexts/inner type classes)
 (let ((type (prune type)))
  (if (ntyvar? type)
      (if (ntyvar-read-only? type)
	  (if (context-implies? (ntyvar-context type) classes)
	      'OK ; no need for context propagation here
	      (begin 
		(setf (ntyvar-value (dynamic *instantiated-tyvar*)) '#f)
		(type-error "Signature context is too general")))
	  (if (null? (ntyvar-context type))
	      (setf (ntyvar-context type) classes)
	      (setf (ntyvar-context type)
		    (merge-contexts classes (ntyvar-context type)))))
      (propagate-contexts-1 (expand-ntype-synonym type) classes))))

;;; The type has now been expanded.  This propagates each class constraint
;;; in turn.

(define (propagate-contexts-1 type classes)
  (dolist (class classes)
     (propagate-single-class type class)))

;;; Now we have a single class & data type.  Either an instance decl can
;;; be found or a type error should be signalled.  Once the instance
;;; decl is found, contexts are propagated to the component types.

(define (propagate-single-class type class)
  (let ((instance (lookup-instance (ntycon-tycon type) class)))
    (cond ((eq? instance '#f)
	   ;; This remove the instantiation which caused the type
	   ;; error - perhaps stop error propagation & make
	   ;; error message better.
	   (setf (ntyvar-value (dynamic *instantiated-tyvar*)) '#f)
	   (type-error "Type ~A is not in class ~A" type class))
	  (else
	   ;; The instance contains a list of class constraints for
	   ;; each argument.  This loop pairs the argument to the
	   ;; type constructor with the context required by the instance
	   ;; decl.
	   (dolist2 (classes (instance-gcontext instance))
		    (arg (ntycon-args type))
	     (propagate-contexts/inner arg classes)))))
  'OK)

;;; The routines which handle contexts (merge-contexts and context-implies?)
;;; are in type-utils.  The occurs check is also there.

