;;; This file contains typecheckers for all expressions except vars and
;;; declarations.

;;; From valdef-structs:
;;;   valdef, single-fun-def are in type-decls

(define-type-checker guarded-rhs
  (type-check guarded-rhs rhs rhs-type
    (type-check guarded-rhs guard guard-type
      (type-unify guard-type *bool-type*
          (type-mismatch/fixed (guarded-rhs-guard object)
	   "Guards must be of type Bool" guard-type))
      (return-type object rhs-type))))

;;; These type checkers deal with patterns.

(define-type-checker as-pat
  (type-check as-pat pattern as-type
    (setf (var-type (var-ref-var (as-pat-var object))) as-type)
    (return-type object as-type)))

(define-type-checker irr-pat
  (type-check irr-pat pattern pattern-type
    (return-type object pattern-type)))

(define-type-checker var-pat
  (fresh-type var-type
    (setf (var-type (var-ref-var (var-pat-var object))) var-type)
    (return-type object var-type)))

(define-type-checker wildcard-pat
 (fresh-type pat-type
    (return-type object pat-type)))

;;; Constant patterns create a piece of code to actually to the
;;; match: ((==) k), where k is the constant.  This code is placed in the
;;; match-fn slot of the const-pat and is used by the cfn.

(define-type-checker const-pat
 (let* ((val (const-pat-value object))
	(match-fn (**app (**var/def (core-symbol "==")) val)))
   (setf (const-pat-match-fn object) match-fn)
   (type-check const-pat match-fn match-type
     (fresh-type res-type
       (type-unify match-type (**arrow res-type *bool-type*) #f)
       (return-type object res-type)))))

(define-type-checker plus-pat
  (let* ((kp (**int (plus-pat-k object)))
	 (km (**int (- (plus-pat-k object))))
	 (match-fn (**app (**var/def (core-symbol "<=")) kp))
	 (bind-fn (**app (**var/def (core-symbol "+")) km)))
    (setf (plus-pat-match-fn object) match-fn)
    (setf (plus-pat-bind-fn object) bind-fn)
    (fresh-type res-type
      (setf (ntyvar-context res-type) (list (core-symbol "Integral")))
      (type-check plus-pat match-fn match-type
        (type-check plus-pat bind-fn bind-type
          (type-check plus-pat pattern pat-type
	    (type-unify match-type (**arrow pat-type *bool-type*) #f)
	    (type-unify bind-type (**arrow pat-type pat-type) #f)
	    (type-unify res-type pat-type #f)
	    (return-type object res-type)))))))

(define-type-checker pcon
 (type-check/list pcon pats arg-types
   (fresh-type res-type
     (let ((con-type (instantiate-gtype (con-signature (pcon-con object)))))
       (type-unify con-type (**arrow/l-2 arg-types res-type) #f)
       (return-type object res-type)))))

(define-type-checker list-pat
  (if (null? (list-pat-pats object))
      (return-type object (instantiate-gtype
			     (algdata-signature (core-symbol "List"))))
      (type-check/unify-list list-pat pats element-type
	   (type-mismatch/list object
	     "List elements have different types")
	(return-type object (**list-of element-type)))))

;;; These are in the order defined in exp-structs.scm

(define-type-checker lambda
 (with-new-tyvars
  (fresh-monomorphic-types (length (lambda-pats object)) arg-vars
    (type-check/list lambda pats arg-types
     (unify-list arg-types arg-vars)
     (type-check lambda body body-type
      (return-type object (**arrow/l-2 arg-vars body-type)))))))

(define-type-checker let
  (type-check/decls let decls
    (type-check let body let-type
      (return-type object let-type))))

(define-type-checker if
  (type-check if test-exp test-type
    (type-unify test-type *bool-type*
        (type-mismatch/fixed object
	 "The test in an if statement must be of type Bool"
	 test-type))
    (type-check if then-exp then-type
      (type-check if else-exp else-type
        (type-unify then-type else-type
              (type-mismatch object
		   "then and else clauses have different types"
		   then-type else-type))
	(return-type object then-type)))))

(define-type-checker case
 (with-new-tyvars
  (let ((case-exp object))  ; needed since object is rebound later
   (fresh-monomorphic-type arg-type
    (type-check case exp exp-type
      (type-unify arg-type exp-type #f) ; just to make it monomorphic
      (fresh-type res-type
	(dolist (object (case-alts object))
	  (recover-type-error ;;; %%% Needs work
	   (type-check alt pat pat-type
	     (type-unify pat-type arg-type
                 (type-mismatch case-exp
		  "Case patterns type conflict."
		  pat-type arg-type))
	     (type-check/decls alt where-decls
	       (type-check/unify-list alt rhs-list rhs-type
                 (type-mismatch/list case-exp
		     "Guarded expressions must have the same type")
		 (type-unify rhs-type res-type
			      (type-mismatch case-exp
		   "Case expression alternatives must have the same type"
		                 rhs-type res-type)))))))
	(return-type case-exp res-type)))))))

;;; Expressions with signatures are transformed into let expressions
;;; with signatures.  

;;;    exp :: type   is rewritten as
;;;    let temp = exp
;;;        temp :: type
;;;     in temp

(define-type-checker exp-sign
 (type-rewrite
  (let* ((temp-var (create-temp-var "TC"))
	 (decl (**valdef (**var-pat/def temp-var) '() (exp-sign-exp object)))
	 (let-exp (**let (list decl) (**var/def temp-var)))
	 (signature (exp-sign-signature object)))
      (setf (var-signature temp-var)
	    (ast->gtype (signature-context signature)
			(signature-type signature)))
      let-exp)))

;;; Rather than complicate the ast structure with a new node for dictSel
;;; we recognize the dictSel primitive as an application and treat it
;;; specially.

(define-type-checker app
 (if (and (var-ref? (app-fn object))
	  (eq? (var-ref-var (app-fn object)) (core-symbol "dictSel")))
  (type-check-dict-sel (app-arg object))
  (type-check app fn fn-type
    (type-check app arg arg-type
      (fresh-type res-type
        (fresh-type arg-type-1
          (type-unify fn-type (**arrow arg-type-1 res-type)
              (type-mismatch/fixed object
		       "Attempt to call a non-function"
		       fn-type))
	  (type-unify arg-type-1 arg-type
              (type-mismatch object
		 "Argument type mismatch" arg-type-1 arg-type))
	  (return-type object res-type)))))))

;;; This is a special hack for typing dictionary selection as used in
;;; generic tuple functions.  This extracts a dictionary from a TupleDict
;;; object and uses is to resolve the overloading of a designated
;;; expression.  The expresion must generate exactly one new context.

(define (type-check-dict-sel arg)
  (when (or (not (app? arg))
	    (not (app? (app-fn arg))))
     (dict-sel-error))
  (let* ((exp (app-fn (app-fn arg)))
	 (dict-var (app-arg (app-fn arg)))
	 (i (app-arg arg))
	 (p (dynamic *placeholders*)))
    (mlet (((object exp-type) (dispatch-type-check exp)))
	  ; check for exactly one new context
      (when (or (eq? (dynamic *placeholders*) p)
		(not (eq? (cdr (dynamic *placeholders*)) p)))
	 (dict-sel-error))
	(mlet ((placeholder (car (dynamic *placeholders*)))
	       (tyvar (placeholder-tyvar placeholder))
	       ((dict-var-ast dict-var-type) (dispatch-type-check dict-var))
	       ((index-ast index-type) (dispatch-type-check i)))
	   (setf (ntyvar-context tyvar) '())  ; prevent context from leaking out
	   (setf (dynamic *placeholders*) p)
           (type-unify dict-var-type
			  (**ntycon (core-symbol "TupleDicts") '()) #f)
	   (type-unify index-type *int-type* #f)
	   (cond ((method-placeholder? placeholder)
		  (dict-sel-error))  ; I am lazy.  This means that
		 ; dictSel must not be passed a method
		 (else
		  (setf (placeholder-exp placeholder)
			(**app (**var/def (core-symbol "dictSel"))
			       dict-var-ast index-ast))))
	   (return-type object exp-type)))))

(define (dict-sel-error)
  (fatal-error 'dict-sel-error "Bad dictSel usage."))

(define-type-checker con-ref
  (return-type object (instantiate-gtype (con-signature (con-ref-con object)))))

(define-type-checker integer-const
  (cond ((const-overloaded? object)
	 (setf (const-overloaded? object) '#f)
	 (type-rewrite (**fromInteger object)))
	(else
	 (return-type object *Integer-type*))))

(define-type-checker float-const
  (cond ((const-overloaded? object)
	 (setf (const-overloaded? object) '#f)
	 (type-rewrite (**fromRational object)))
	(else
	 (return-type object *Rational-type*))))

(define-type-checker char-const
  (return-type object *char-type*))

(define-type-checker string-const
  (return-type object *string-type*))

(define-type-checker list-exp
  (if (null? (list-exp-exps object))
      (return-type object (instantiate-gtype
			     (algdata-signature (core-symbol "List"))))
      (type-check/unify-list list-exp exps element-type
	      (type-mismatch/list object
		 "List elements do not share a common type")
	(return-type object (**list-of element-type)))))

(define-type-checker sequence
  (type-rewrite (**enumFrom (sequence-from object))))

(define-type-checker sequence-to
  (type-rewrite (**enumFromTo (sequence-to-from object)
			      (sequence-to-to object))))

(define-type-checker sequence-then
  (type-rewrite (**enumFromThen (sequence-then-from object)
				(sequence-then-then object))))

(define-type-checker sequence-then-to
  (type-rewrite (**enumFromThenTo (sequence-then-to-from object)
				  (sequence-then-to-then object)
				  (sequence-then-to-to object))))

(define-type-checker list-comp
 (with-new-tyvars
  (dolist (object (list-comp-quals object))
    (if (is-type? 'qual-generator object)
	(fresh-type pat-type
	 (push pat-type (dynamic *non-generic-tyvars*))
	 (type-check qual-generator pat pat-type-1
	   (type-unify pat-type pat-type-1 #f)
	   (type-check qual-generator exp qual-exp-type
	     (type-unify (**list-of pat-type) qual-exp-type
                            (type-mismatch/fixed object
		 "Generator expression is not a list" qual-exp-type)))))
	 (type-check qual-filter exp filter-type
	   (type-unify filter-type *bool-type*
              (type-mismatch/fixed object
		"Filter must have type Bool" filter-type)))))
  (type-check list-comp exp exp-type
     (return-type object (**list-of exp-type)))))

(define-type-checker section-l
  (type-check section-l op op-type
    (type-check section-l exp exp-type
      (fresh-type a-type
        (fresh-type b-type
          (fresh-type c-type
            (type-unify op-type (**arrow a-type b-type c-type)
                (type-mismatch/fixed object
		     "Binary function required in section" op-type))
	    (type-unify b-type exp-type
                  (type-mismatch object
		      "Argument type mismatch" b-type exp-type))
	    (return-type object (**arrow a-type c-type))))))))

(define-type-checker section-r
  (type-check section-r op op-type
    (type-check section-r exp exp-type
      (fresh-type a-type
        (fresh-type b-type
          (fresh-type c-type
            (type-unify op-type (**arrow a-type b-type c-type)
                  (type-mismatch/fixed object
			 "Binary function required" op-type))
	    (type-unify exp-type a-type
                    (type-mismatch object
			 "Argument type mismatch" a-type exp-type))
	    (return-type object (**arrow b-type c-type))))))))

(define-type-checker omitted-guard
  (return-type object *bool-type*))

(define-type-checker con-number
  (let ((arg-type (instantiate-gtype
		   (algdata-signature (con-number-type object)))))
    (type-check con-number value arg-type1
      (type-unify arg-type arg-type1 #f)
      (return-type object *int-type*))))

(define-type-checker sel
  (let ((con-type (instantiate-gtype
		   (con-signature (sel-constructor object)))))
    (mlet (((res-type exp-type1) (get-ith-type con-type (sel-slot object))))
      (type-check sel value exp-type
        (type-unify exp-type exp-type1 #f)
	(return-type object res-type)))))

(define (get-ith-type type i)
 (let ((args (ntycon-args type)))  ; must be an arrow
  (if (eq? i 0)
      (values (car args) (get-ith-type/last (cadr args)))
      (get-ith-type (cadr args) (1- i)))))

(define (get-ith-type/last type)
  (if (eq? (ntycon-tycon type) (core-symbol "Arrow"))
      (get-ith-type/last (cadr (ntycon-args type)))
      type))

(define-type-checker is-constructor
  (let ((alg-type (instantiate-gtype
		   (algdata-signature
		    (con-alg (is-constructor-constructor object))))))
    (type-check is-constructor value arg-type
      (type-unify arg-type alg-type #f)
      (return-type object *bool-type*))))

(define-type-checker cast
  (type-check cast exp _
    (fresh-type res
      (return-type object res))))

;;; This is used for overloaded methods.  The theory is to avoid supplying
;;; the context at the class level.  This type checks the variable as if it had
;;; the supplied signature.

(define-type-checker overloaded-var-ref
  (let* ((var (overloaded-var-ref-var object))
	 (gtype (overloaded-var-ref-sig object))
	 (ovar-type (var-type var)))
    (when (recursive-type? ovar-type)
	 (error
	  "Implementation error: overloaded method found a recursive type"))
    (mlet (((ntype new-vars) (instantiate-gtype/newvars gtype))
	   (object1 (insert-dict-placeholders
		     (**var/def var) new-vars object)))
	  (return-type object1 ntype))))
