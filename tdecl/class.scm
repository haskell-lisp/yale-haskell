;;; Before classes are converted, the super class relation is computed.
;;; This sets up the super and super* field of each class and
;;; checks for the following errors:
;;;  Wrong tyvar in context
;;;  cyclic class structure
;;;  Non-class in context

(define (compute-super-classes modules)
  (let ((all-classes '()))
    (walk-modules modules
     (lambda ()       
      (dolist (c (module-classes *module*))
       (remember-context c
	(with-slots class-decl (super-classes class class-var) c
	  (let* ((def (class-ref-class class))
		 (local-ctxts '())
		 (super '()))
	    (dolist (context super-classes)
              (with-slots context (class tyvar) context
		(when (not (eq? class-var tyvar))
		  (signal-super-class-tyvar-error class class-var tyvar))
		(resolve-class class)
		(let ((super-def (class-ref-class class)))
		  (when (not (eq? super-def *undefined-def*))
		    (push super-def super)
		    (when (eq? *unit* (def-unit super-def))
		      (push super-def local-ctxts))))))
	    (update-slots class def
	       (super super)
	       (tyvar class-var))
	    (push (cons def local-ctxts) all-classes)))))))
    (multiple-value-bind (status sorted) (topsort all-classes)
      (when (eq? status 'cyclic)
	(signal-cyclic-class-structure sorted))
      (dolist (c sorted)
        (let* ((super (class-super c))
	       (super* super))
	   (dolist (s super)
	     (setf super* (set-union super* (class-super* s)))
	     (setf (class-super* c) super*)))))))

(define (signal-super-class-tyvar-error class class-var tyvar)
  (recoverable-error 'super-class-tyvar-error
    "The context for class ~A must only refer to type variable ~A.~%~
     Type variable ~A cannot be used here."
    (class-ref-name class) class-var tyvar))

(define (signal-cyclic-class-structure classes)
  (fatal-error 'cyclic-class-structure
    "There is a cycle in the superclass relation involving these classes:~%~a"
    classes))


;;;  This sets up the following fields in the class entry:
;;;    instances '()
;;;    defaults = ast for defaults
;;;    kind
;;;    methods
;;;    signatures
;;;    method-vars
;;;    selectors
;;;  Each method is initialized with
;;;    class
;;;    signature
;;;    type
;;;  Errors detected:
;;;   signature doesnt reference class 

(define (class->def class-decl)
 (remember-context class-decl
   (let* ((class (class-ref-class (class-decl-class class-decl)))
	  (decls (class-decl-decls class-decl)))
     (setf (class-instances class) '())
     (setf (class-kind class) (find-class-kind class))
     (init-methods class decls)  ; sets up defaults, method signatures
     (setf (class-n-methods class) (length (class-method-vars class)))
     (setf (class-dict-size class)
	   (+ (class-n-methods class) (length (class-super* class))))
     class)))

(define (find-class-kind class)
  (cond ((not (module-prelude? *module*))
	 'other)
	((memq class
	       (list (core-symbol "Eq") (core-symbol "Ord")
		     (core-symbol "Text") (core-symbol "Binary")
		     (core-symbol "Ix") (core-symbol "Enum")))
	 'Standard)
	((memq class
	       (list (core-symbol "Num") (core-symbol "Real")
		     (core-symbol "Integral") (core-symbol "Fractional")
		     (core-symbol "Floating") (core-symbol "RealFrac")
		     (core-symbol "RealFloat")))
		     'Numeric)
	(else
	 'other)))

(define (init-methods class decls)
 (let* ((tyvar (class-tyvar class))
        (class-context (**context (**class/def class) tyvar)))
  (dolist (decl decls)
   (remember-context decl
    (cond ((is-type? 'signdecl decl)
	   (let* ((signature (signdecl-signature decl))
		  (vars (resolve-signature signature)))
	     (when (not (memq tyvar vars))
	       (signal-class-sig-ignores-type signature))
	     ;; Note: signature does not include defined class yet
	     (dolist (context (signature-context signature))
               (when (eq? tyvar (context-tyvar context))
		 (signal-method-constrains-class-tyvar context)))
	     (setf signature (rename-class-sig-vars signature tyvar))
	     (let ((gtype (ast->gtype (cons class-context
					    (signature-context signature))
				      (signature-type signature))))
 	       (dolist (var-ref (signdecl-vars decl))
	         (let ((var (var-ref-var var-ref)))
		   (setf (var-type var) gtype)
		   (setf (method-var-method-signature var) signature))))))
	  (else  ; decl must be a default definition
	   (let ((vars (collect-pattern-vars (valdef-lhs decl))))
	     (dolist (var-ref vars)
	       (resolve-var var-ref)
               (let* ((method-name (var-ref-name var-ref))
		      (method-var (var-ref-var var-ref)))
		 (when (not (eq? method-var *undefined-def*))
		  (if (and (method-var? method-var)
			   (eq? (method-var-class method-var) class))
		   (let ((default-var
			   (make-new-var
			     (string-append
			       "default-"
			       (symbol->string (def-name method-var))))))
		     (setf (var-ref-var var-ref) default-var)
		     (setf (var-ref-name var-ref) (def-name default-var))
		     (when (not (eq? (method-var-default method-var) '#f))
		       (signal-multiple-definition-of-default method-name))
		     (setf (method-var-default method-var) default-var)
		     (let* ((sig (method-var-method-signature method-var))
			    (context (cons class-context
					   (signature-context sig)))
			    (new-sig (**signature context
						  (signature-type sig))))
		       (add-new-module-signature default-var new-sig)))
		   (signal-default-not-in-class method-var class)))))
	     (add-new-module-decl decl))))))))

(define (signal-class-sig-ignores-type signature)
  (phase-error 'class-sig-ignores-type
    "The method signature ~a does not reference the overloaded type."
    signature))


;;; *** I don't understand this message.

(define (signal-method-constrains-class-tyvar context)
  (phase-error 'method-constrains-class-tyvar
    "Individual methods may not further constrain a class: ~A" context))


;;; *** I don't understand this message.

(define (signal-multiple-definition-of-default method-name)
  (phase-error 'multiple-definition-of-default
   "More that one default for ~A."
   method-name))


;;; *** I don't understand this message.

(define (signal-default-not-in-class method-var class)
  (phase-error 'default-not-in-class
	       "~A is not a method in class ~A."
	       method-var class))

	   
(define (create-selector-functions class)
  (let ((res '()))
    (dolist (c (cons class (class-super* class)))
      (dolist (m (class-method-vars c))
	(let* ((var (make-new-var
	        (string-append "sel-"
			       (symbol->string (def-name class))
			       "/"
			       (symbol->string (def-name m)))))
	       (sel-body (create-selector-code class m)))
	  (setf (var-selector-fn? var) '#t)
	  (push (tuple m var) res)
	  (when (not (eq? (module-type *module*) 'interface))
	     (add-new-module-def var sel-body)))))
    res))

(define (create-selector-code c m)
  (let ((var (create-local-definition '|d|)))
    (setf (var-force-strict? var) '#t)
    (let ((body (create-selector-code-1 c m (**var/def var))))
      (**lambda/pat (list (**var-pat/def var)) body))))

(define (create-selector-code-1 class method d)
  (let ((mcl (method-var-class method)))
    (cond ((eq? mcl class)
	   (**dsel/method class method d))
	  (else
	   (**dsel/method mcl method (**dsel/dict class mcl d))))))
	     
;;; The following code is for the alpha conversion of method
;;; signatures.  The class tyvar is unchanged; all others are renamed.
;;; This is needed because all method types are combined to form the
;;; dictionary signature and aliasing among different tyvars should be
;;; prevented.

(define (rename-class-sig-vars signature tyvar)
  (mlet (((new-context env1)
	  (rename-context-vars (signature-context signature)
			       (list (tuple tyvar tyvar))))
	 ((new-type _)
	  (rename-type-vars (signature-type signature) env1)))
      (**signature new-context new-type)))

(define (rename-context-vars contexts env)
  (if (null? contexts)
      (values '() env)
      (mlet (((new-tyvar env1)
	      (rename-sig-tyvar (context-tyvar (car contexts)) env))
	     ((rest env2)
	      (rename-context-vars (cdr contexts) env1)))
       (values (cons (**context (context-class (car contexts)) new-tyvar) rest)
	       env2))))

(define (rename-type-vars type env)
  (if (tyvar? type)
      (mlet (((tyvar env1)
	      (rename-sig-tyvar (tyvar-name type) env)))
	 (values (**tyvar tyvar) env1))
      (mlet (((new-types env1) (rename-type-vars/l (tycon-args type) env)))
        (values (**tycon/def (tycon-def type) new-types) env1))))

(define (rename-type-vars/l types env)
  (if (null? types)
      (values '() env)
      (mlet (((type1 env1) (rename-type-vars (car types) env))
	     ((new-types env2) (rename-type-vars/l (cdr types) env1)))
          (values (cons type1 new-types) env2))))

(define (rename-sig-tyvar tyvar env)
  (let ((res (assq tyvar env)))
    (if (eq? res '#f)
	(let ((new-tyvar (gentyvar (symbol->string tyvar))))
	  (values new-tyvar (cons (tuple tyvar new-tyvar) env)))
	(values (tuple-2-2 res) env))))

(define *tyvar-counter* 0)

;;; This generates a new interned tyvar name

(define (gentyvar root)
  (incf *tyvar-counter*)
  (string->symbol (format '#f "~A-~A" root *tyvar-counter*)))
