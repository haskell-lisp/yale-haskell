
;;; This file also contains some random globals for the type checker:

(define-walker type ast-td-type-walker)

;;; Some pre-defined types
(define *bool-type* '())
(define *char-type* '())
(define *string-type* '())
(define *int-type* '())
(define *integer-type* '())
(define *rational-type* '())

;;; These two globals are used throughout the typechecker to avoid
;;; passing lots of stuff in each function call.

(define *placeholders* '())
(define *non-generic-tyvars* '())
(define *enclosing-decls* '())

;;; Used by the defaulting mechanism

(define *default-decls* '())

;;; Used in error handling & recovery

(define *type-error-handlers* '())
(define *type-error-recovery* '())


;;; This associates a type checker function with an ast type.  The variable
;;; `object' is bound to the value being types.

(define-syntax (define-type-checker ast-type . cont)
  `(define-walker-method type ,ast-type (object)
     ,@cont))

;;; This recursively type checks a structure slot in the current object.
;;; This updates the ast in the slot (since type checking rewrites the ast)
;;; and binds the computed type to a variable.  The slot must contain an
;;; expression.

(define-syntax (type-check struct slot var . cont)
  `(mlet ((($$$ast$$$ ,var)
	   (dispatch-type-check (struct-slot ',struct ',slot object))))
	 (setf (struct-slot ',struct ',slot object) $$$ast$$$)
	 ,@cont))

;;; This is used to scope decls.

(define-syntax (with-new-tyvars . cont)
  `(dynamic-let ((*non-generic-tyvars* (dynamic *non-generic-tyvars*)))
     ,@cont))


;;; Similar to type-check, the slot must contain a list of decls.
;;; This must be done before any reference to a variable defined in the
;;; decls is typechecked.
		
(define-syntax (type-check/decls struct slot . cont)
  `(with-new-tyvars
    (let (($$$decls$$$
	  (type-decls (struct-slot ',struct ',slot object))))
     (setf (struct-slot ',struct ',slot object) $$$decls$$$)
     ,@cont)))

;;; The type checker returns an expression / type pair.  This
;;; abstracts the returned value.

(define-syntax (return-type object type)
  `(values ,object ,type))

;;; When an ast slot contains a list of expressions, there are two
;;; possibilities: the expressions all share the same type or each has
;;; an independant type.  In the first case, a single type (computed
;;; by unifying all types in the list) is bound to a variable.

(define-syntax (type-check/unify-list struct slot var error-handler . cont)
  `(mlet ((($$$ast$$$ $$$types$$$)
	   (do-type-check/list (struct-slot ',struct ',slot object))))
    (setf (struct-slot ',struct ',slot object) $$$ast$$$)
    (with-type-error-handler ,error-handler ($$$types$$$)
       (unify-list/single-type $$$types$$$)
       (let ((,var (car $$$types$$$)))
	 ,@cont))))

;;; When a list of expressions does not share a common type, the result is
;;; a list of types.

(define-syntax (type-check/list struct slot var . cont)
  `(mlet ((($$$ast$$$ ,var)
	   (do-type-check/list (struct-slot ',struct ',slot object))))
    (setf (struct-slot ',struct ',slot object) $$$ast$$$)
    ,@cont))

;;; This creates a fresh tyvar and binds it to a variable.

(define-syntax (fresh-type var . cont)
  `(let ((,var (**ntyvar)))
     ,@cont))

;;; This drives the unification routine.  Two types are unified and the
;;; context is updated.  Currently no error handling is implemented to
;;; deal with unification errors.

(define-syntax (type-unify type1 type2 error-handler)
  `(with-type-error-handler ,error-handler ()
     (unify ,type1 ,type2)))

;;; This generates a fresh set of monomorphic type variables.

(define-syntax (fresh-monomorphic-types n vars . cont)
  `(with-new-tyvars
     (let ((,vars '()))
       (dotimes (i ,n)
	   (let ((tv (**ntyvar)))
	     (push tv ,vars)
	     (push tv (dynamic *non-generic-tyvars*))))
       ,@cont)))

;;; This creates a single monomorphic type variable.

(define-syntax (fresh-monomorphic-type var . cont)
  `(let* ((,var (**ntyvar)))
     (with-new-tyvars
       (push ,var (dynamic *non-generic-tyvars*))
       ,@cont)))

;;; This is used to rewrite the current ast as a new ast and then
;;; recursively type check the new ast.  The original ast is saved for
;;; error message printouts.

(define-syntax (type-rewrite ast)
  `(mlet (((res-ast type) (dispatch-type-check ,ast))
	  (res (**save-old-exp object res-ast)))
      (return-type res type)))

;;; These are the type error handlers

(define-syntax (recover-type-error error-handler . body)
 (let ((temp (gensym))
       (err-fn (gensym)))
  `(let/cc ,temp
    (let ((,err-fn ,error-handler))
     (dynamic-let ((*type-error-recovery*
		    (cons (lambda ()
			    (funcall ,err-fn ,temp))
			  (dynamic *type-error-recovery*))))
        ,@body)))))

(define-syntax (with-type-error-handler handler extra-args . body)
  (if (eq? handler '#f)
      `(begin ,@body)
      `(dynamic-let ((*type-error-handlers*
		      (cons (lambda ()
			     (,(car handler) ,@extra-args ,@(cdr handler)))
			    (dynamic *type-error-handlers*))))
	    ,@body)))

