;;; This type checks a variable.  Possible cases:
;;;  a) recursive variables
;;;  b) method variables
;;;  c) generalized variables 
;;;  d) other variables

(define-type-checker var-ref
 (let* ((var (var-ref-var object))
	(type (var-type var)))
   (cond ((method-var? var)
;;; The context of a method variable always has the carrier class
;;; first.
	  (mlet (((ntype new-tyvars) (instantiate-gtype/newvars type))
		 (carrier-tyvar (car new-tyvars))
		 (extra-context (cdr new-tyvars))
		 (p (**method-placeholder
		     var carrier-tyvar (dynamic *enclosing-decls*) object))
		 (new-object (insert-dict-placeholders p extra-context object)))
	    (remember-placeholder p)
	    (return-type (**save-old-exp object new-object) ntype)))
	 ((recursive-type? type)
	  (let ((placeholder (**recursive-placeholder
			      var (dynamic *enclosing-decls*))))
	    (push placeholder (recursive-type-placeholders type))
	    (return-type placeholder (recursive-type-type type))))
	 ((gtype? type)
	  (mlet (((ntype new-vars) (instantiate-gtype/newvars type))
		 (object1 (insert-dict-placeholders object new-vars object)))
            (return-type (if (eq? object1 object)
			     object
			     (**save-old-exp object object1))
			 ntype)))
	 (else
	  (return-type object type)))))

;;; This takes an expression and a context and returns an updated
;;; expression containing placeholders for the context information
;;; implied by the context.  Tyvars in the context are added to dict-vars.

(define (insert-dict-placeholders object tyvars var)
  (cond ((null? tyvars)
	 object)
	((null? (ntyvar-context (car tyvars)))
	 (insert-dict-placeholders object (cdr tyvars) var))
	(else
	 (let ((tyvar (car tyvars)))
	   (insert-dict-placeholders
	    (insert-dict-placeholders/tyvar
	     tyvar (ntyvar-context tyvar) object var)
	    (cdr tyvars)
	    var)))))

(define (insert-dict-placeholders/tyvar tyvar classes object var)
  (if (null? classes)
      object
      (let ((p (**dict-placeholder
		 (car classes) tyvar (dynamic *enclosing-decls*) var)))
	(remember-placeholder p)
	(insert-dict-placeholders/tyvar tyvar (cdr classes) 
					(**app object p) var))))
