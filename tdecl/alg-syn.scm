
;;; Description: Convert algdata & synonym from ast to definition form.
;;;              Lots of error checking.

;;;  Algdata:
;;;   Errors detected:
;;;    Types & classes (deriving & context) resolved
;;;    context tyvars must be parameters
;;;    all parameter tyvars must be referenced
;;;    only parameter tyvars must be referenced

(define (algdata->def data-decl)
  (remember-context data-decl
   (with-slots data-decl (context simple constrs deriving annotations) data-decl
      (let* ((def (tycon-def simple))
	     (tyvars (simple-tyvar-list simple))
	     (enum? '#t)
	     (tag 0)
	     (derived-classes '())
	     (tyvars-referenced '())
	     (all-con-vars '())
	     (all-strict? (process-alg-strictness-annotation annotations))
	     (constr-defs
	      (map (lambda (constr)
		     (with-slots constr (constructor types) constr
		       (let ((constr-def (con-ref-con constructor))
			     (c-arity (length types))
			     (con-vars '())
			     (all-types '())
			     (strictness '()))
			 (when (not (eqv? c-arity 0))
			   (setf enum? '#f))
			 (dolist (type types)
			   (let* ((ty (tuple-2-1 type))
				  (anns (tuple-2-2 type))
				  (tyvars1 (resolve-type ty)))
			     (push ty all-types)
			     (push (get-constr-strictness anns all-strict?)
				   strictness)
			     (dolist (v tyvars1)
			       (if (not (memq v tyvars))
				   (signal-bad-algdata-tyvar v)))
			     (setf con-vars (append tyvars1 tyvars-referenced))
			     (setf tyvars-referenced
				   (append tyvars1 tyvars-referenced))))
			 (push (tuple constr con-vars) all-con-vars)
			 (update-slots con constr-def
		           (arity c-arity)
			   (types (reverse all-types))
			   (tag tag)
			   (alg def)
			   (infix? (con-ref-infix? constructor))
			   (slot-strict? (reverse strictness)))
			 (incf tag)
			 constr-def)))
		   constrs)))
	(dolist (class deriving)
	  (if (eq? (class-ref-name class) '|Printers|)
	      (setf (class-ref-class class) *printer-class*)
	      (resolve-class class))
	  (when (not (eq? (class-ref-class class) *undefined-def*))
	    (push (class-ref-class class) derived-classes)))
	(when (not (null? constrs))
	   (dolist (tyvar tyvars)
	      (when (not (memq tyvar tyvars-referenced))
		 (signal-unreferenced-tyvar-arg tyvar))))
	(resolve-signature-aux tyvars context)
	;; This computes a signature for the datatype as a whole.
	(let ((gtype (ast->gtype context simple)))
	  ;; This sets the signatures for the constructors
	  (dolist (con constr-defs)
	    (let* ((con-type (**arrow-type/l (append (con-types con)
						     (list simple))))
		   (con-context (restrict-context
				 context (tuple-2-2 (assq con all-con-vars))))
		   (con-signature (ast->gtype con-context con-type)))
	      (setf (con-signature con) con-signature)))
	  (update-slots algdata def
	    (n-constr (length constrs))
	    (constrs constr-defs)
	    (context context)
	    (tyvars tyvars)
	    (signature gtype)
	    (classes '())
	    (enum? enum?)
	    (tuple? (and (not (null? constrs)) (null? (cdr constrs))))
	    (real-tuple? '#f)
	    (deriving derived-classes)
	    ))
	(process-alg-annotations def)
	def))))


(define (process-alg-strictness-annotation anns)
  (let ((res '#f))
    (dolist (a anns)
     (if (and (annotation-value? a)
	      (eq? (annotation-value-name a) '|STRICT|)
	      (null? (annotation-value-args a)))
	 (setf res '#t)
	 (signal-unknown-annotation a)))
    res))

(define (get-constr-strictness anns all-strict?)
  (let ((res all-strict?))
    (dolist (a anns)
       (cond ((annotation-value? a)
	      (if (and (eq? (annotation-value-name a) '|STRICT|)
		       (null? (annotation-value-args a)))
		  (setf res '#t)
		  (signal-unknown-annotation a)))
	     (else (signal-unknown-annotation a))))
    res))

(define (process-alg-annotations alg)
  (dolist (a (module-annotations *module*))
    (when (and (annotation-value? a)
	       (or (eq? (annotation-value-name a) '|ImportLispType|)
		   (eq? (annotation-value-name a) '|ExportLispType|))
	       (assq (def-name alg) (car (annotation-value-args a))))
      (if (eq? (annotation-value-name a) '|ImportLispType|)
	  (setf (algdata-implemented-by-lisp? alg) '#t)
	  (setf (algdata-export-to-lisp? alg) '#t))
      (let ((constrs (tuple-2-2 (assq (def-name alg)
				      (car (annotation-value-args a))))))
	(dolist (c constrs)
          (process-annotated-constr
	   alg
	   (lookup-alg-constr (tuple-2-1 c) (algdata-constrs alg))
	   (tuple-2-2 c)))))))

(define (lookup-alg-constr name constrs)
  (if (null? constrs)
      (fatal-error 'bad-constr-name "Constructor ~A not in algdata~%"
		   name)
      (if (eq? name (def-name (car constrs)))
	  (car constrs)
	  (lookup-alg-constr name (cdr constrs)))))

(define (process-annotated-constr alg con lisp-fns)
  ;; For nullary tuples, allow a single annotation to represent a constant
  ;; and generate the test function by default.
  (when (and (eqv? (con-arity con) 0)
	     lisp-fns
	     (null? (cdr lisp-fns)))
	(push `(lambda (x) (eq? x ,(car lisp-fns))) lisp-fns))
  ;; Insert an implicit test function for tuples (never used anyway!)
  (when (and (algdata-tuple? alg)
	     (eqv? (+ 1 (con-arity con)) (length lisp-fns)))
	(push '(lambda (x) '#t) lisp-fns))
  (when (or (not (null? (con-lisp-fns con)))
	    (not (eqv? (length lisp-fns) (+ 2 (con-arity con)))))
      (fatal-error 'bad-constr-annotation
		   "Bad annotation for ~A in ~A~%" con alg))
  (setf (con-lisp-fns con) lisp-fns))

(define (signal-unknown-annotation a)
  (recoverable-error 'bad-annotation "Bad or misplaced annotation: ~A%"
      a))

(define (restrict-context context vars)
  (if (null? context)
      '()
      (let ((rest (restrict-context (cdr context) vars)))
	(if (memq (context-tyvar (car context)) vars)
	    (cons (car context) rest)
	    rest))))

(define (signal-bad-algdata-tyvar tyvar)
  (phase-error 'bad-algdata-tyvar
    "~a is referenced on the right-hand side of a data type declaration,~%~
     but is not bound as a type variable."
    tyvar))

(define (signal-unreferenced-tyvar-arg tyvar)
  (phase-error 'unreferenced-tyvar-arg
    "~a is bound as a type variable in a data type declaration,~%~
     but is not referenced on the right-hand side."
    tyvar))

;;; Synonyms

;;; Errors detected:

(define (synonym->def synonym-decl)
 (remember-context synonym-decl
  (with-slots synonym-decl (simple body) synonym-decl
    (let* ((def (tycon-def simple))
	   (tyvars (simple-tyvar-list simple))
	   (tyvars-referenced (resolve-type body)))
      (dolist (v tyvars)
	(if (not (memq v tyvars-referenced))
	  (signal-unreferenced-synonym-arg v)))
      (dolist (v tyvars-referenced)
	(if (not (memq v tyvars))
	    (signal-bad-synonym-tyvar v)))
      (update-slots synonym def
	 (args tyvars)
	 (body body))
      (push (cons def (gather-synonyms body '())) *synonym-refs*)
      def))))

(define (signal-bad-synonym-tyvar tyvar)
  (phase-error 'bad-synonym-tyvar
    "~a is referenced on the right-hand side of a type synonym declaration,~%~
     but is not bound as a type variable."
    tyvar))

(define (signal-unreferenced-synonym-arg tyvar)
  (haskell-warning 'unreferenced-synonym-arg
    "~a is bound as a type variable in a type synonym declaration,~%~
     but is not referenced on the right-hand side."
    tyvar))

(define (gather-synonyms type acc)
  (cond ((tyvar? type)
	 acc)
	((and (synonym? (tycon-def type))
	      (eq? *unit* (def-unit (tycon-def type))))
	 (gather-synonyms/list (tycon-args type)
			       (cons (tycon-def type) acc)))
	(else
	 (gather-synonyms/list (tycon-args type) acc))))

(define (gather-synonyms/list types acc)
  (if (null? types)
      acc
      (gather-synonyms/list (cdr types) (gather-synonyms (car types) acc))))
