;;; File: parser/typedecl-parser     Author: John

(define (parse-type-decl interface?)
 (save-parser-context
  (let* ((sig (parse-signature))
	 (contexts (signature-context sig))
	 (simple (signature-type sig))
	 (deriving '())
	 (constrs '()))
    ;; #t = builtins ([] (,,) ->) not allowed
    (check-simple simple '#t "type declaration")
    (let ((annotations (parse-constr-annotations)))
     (token-case
      (= (setf constrs (parse-constrs))
	 (token-case
	  (|deriving|
	   (setf deriving
	     (token-case
		 (\( 
		  (token-case
		   (\) '())
		   (else (parse-class-list))))
		 (tycon (list (class->ast)))
		 (else (signal-invalid-syntax "a deriving clause")))))))
      (else
       (when (not interface?)
	 (signal-missing-constructors))))
    (make data-decl (context contexts) (simple simple)
		    (constrs constrs) (deriving deriving)
		    (annotations annotations))))))

(define (signal-missing-constructors)
  (parser-error 'missing-constructors
		"Data type definition requires constructors"))

(define (check-simple simple fresh? where)
  (when (not (tycon? simple))
    (signal-not-simple where))
  (when (and fresh? (not (eq? (tycon-def simple) *undefined-def*)))
    (signal-not-simple where))
  (let ((tyvars (map (lambda (arg)
		       (when (not (tyvar? arg))
			     (signal-not-simple where))
		       (tyvar-name arg))
		     (tycon-args simple))))
    (when (not (null? (find-duplicates tyvars)))
      (signal-unique-tyvars-required))))

(define (signal-unique-tyvars-required)
  (parser-error 'unique-tyvars-required
		"Duplicate type variables appear in simple."))

(define (signal-not-simple where)
  (parser-error 'not-simple "Simple type required in ~a." where))

(define (parse-constrs)
  (let ((constr (parse-constr)))
    (token-case
     (\| (cons constr (parse-constrs)))
     (else (list constr)))))

(define (parse-constr)
 (save-parser-context
  (let ((saved-excursion (save-scanner-state)))
    (token-case
     (consym/paren
      (parse-prefix-constr))
     (else
      (let ((type1 (parse-btype))
	    (anns (parse-constr-annotations)))
	(token-case
	 (conop
	  (parse-infix-constr (tuple type1 anns)))
	 (else
	  (restore-excursion saved-excursion)
	  (parse-prefix-constr)))))))))

(define (parse-prefix-constr)
  (token-case
   (con
    (let* ((con (con->ast))
	   (types (parse-constr-type-list)))
      (make constr (constructor con) (types types))))
   (else
    (signal-missing-token "<con>" "constrs list"))))

(define (parse-constr-type-list)
  (token-case
    (atype-start
     (let* ((atype (parse-atype))
	    (anns (parse-constr-annotations)))
       (cons (tuple atype anns)
	     (parse-constr-type-list))))
    (else '())))

(define (parse-infix-constr t+a1)
  (let* ((con (conop->ast))
	 (type2 (parse-btype))
	 (anns (parse-constr-annotations)))
    (make constr (constructor con) (types (list t+a1 (tuple type2 anns))))))

(define (parse-class-list)
  (token-case
   (tycon (let ((class (class->ast)))
	     (token-case
	      (\, (cons class (parse-class-list)))
	      (\) (list class))
	      (else (signal-missing-token "`)' or `,'" "deriving clause")))))
   (else (signal-missing-token "<tycon>" "deriving clause"))))

(define (parse-constr-annotations)
  (token-case
   ((begin-annotation no-advance)
    (let ((annotations (parse-annotations)))
      (append annotations (parse-constr-annotations))))
   (else '())))

(define (parse-synonym-decl)
 (save-parser-context
  (let* ((sig (parse-signature))
	 (contexts (signature-context sig))
	 (simple (signature-type sig)))
    (check-simple simple '#t "type synonym declaration")
    (when (not (null? contexts))
      (signal-no-context-in-synonym))
    (require-token = (signal-missing-token "`='" "type synonym declaration"))
    (let ((body (parse-type)))
      (make synonym-decl (simple simple) (body body))))))

(define (signal-no-context-in-synonym)
  (parser-error 'no-context-in-synonym
		"Context is not permitted in type synonym declaration."))

(define (parse-class-decl)
 (save-parser-context
  (let ((supers (parse-optional-context)))
    (token-case
     (tycon
      (let ((class (class->ast)))
	(token-case
	 (tyvar
	  (let* ((class-var (token->symbol))
		 (decls (parse-where-decls)))
	    (make class-decl (class class) (super-classes supers)
		             (class-var class-var) (decls decls))))
	 (else
	  (signal-missing-token "<tyvar>" "class declaration")))))
     (else (signal-missing-token "<tycon>" "class declaration"))))))

(define (parse-instance-decl interface?)
 (save-parser-context
  (let ((contexts (parse-optional-context))
	(decls '()))
    (token-case
     (tycon
      (let* ((class (class->ast))
	     (simple (parse-type)))
	(when (not interface?)
	   (setf decls (parse-where-decls)))
	(check-simple simple '#f "instance declaration")
	(make instance-decl (context contexts) (class class)
	                    (simple simple) (decls decls))))
     (else (signal-missing-token "<tycon>" "instance declaration"))))))
