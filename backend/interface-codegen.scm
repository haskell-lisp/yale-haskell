
;;; This generates code for vars defined in an interface.  This looks at
;;; annotations and fills in the slots of the var definition.

(define (haskell-codegen/interface mods)
  (codegen/interface (car mods)))

(define (codegen/interface mod)
 (let ((code '()))
  (dolist (d (module-decls mod))
    (when (not (signdecl? d))
      (error 'bad-decl))
    (dolist (var (signdecl-vars d))
     (let ((v (var-ref-var var)))
      (setf (var-type v) (var-signature v))
      (setf (var-toplevel? v) '#t)
      (let ((a (lookup-annotation v '|Complexity|)))
	(when (not (eq? a '#f))
	  (setf (var-complexity v)
		(car (annotation-value-args a)))))
      (let ((a (lookup-annotation v '|LispName|)))
	(when (not (eq? a '#f))
	   (let ((lisp-entry (generate-lisp-entry v a)))
	     (push lisp-entry code)
	     (when (memq 'codegen (dynamic *printers*))
  	        (pprint* lisp-entry))))))))
  `(begin ,@code)))

(define (generate-lisp-entry v a)
  (let ((lisp-name (read-lisp-object (car (annotation-value-args a))))
	(type (maybe-expand-io-type (gtype-type (var-type v)))))
    (setf (var-optimized-entry v) lisp-name)
    (if (arrow-type? type)
	(codegen-lisp-fn v (gather-arg-types type))
	(codegen-lisp-const v type))))

(define (codegen-lisp-fn var arg-types)
  (let* ((aux-definition '())
	 (wrapper? (foreign-fn-needs-wrapper? var arg-types))
	 (strictness-annotation (lookup-annotation var '|Strictness|))
	 (strictness (determine-strictness strictness-annotation arg-types))
	 (temps (gen-temp-names strictness)))
    (setf (var-strict? var) '#t)
    (setf (var-arity var) (length strictness))
    (setf (var-strictness var) strictness)
    (when wrapper?
	  (mlet (((code name)
		  (make-wrapper-fn var (var-optimized-entry var) arg-types)))
	      (setf (var-optimized-entry var) name)
	      (setf aux-definition (list code))))
    `(begin ,@aux-definition
	    (define ,(fullname var)
		    ,(maybe-make-box-value
		       (codegen-curried-fn
			(if wrapper?
			    `(function ,(var-optimized-entry var))
			    `(lambda ,temps
			          (,(var-optimized-entry var) ,@temps)))
			 (var-strictness var))
		       '#t)))))

(define (determine-strictness a args)
  (if (eq? a '#f)
      (map (lambda (x) (declare (ignore x)) '#t) (cdr args))
      (parse-strictness (car (annotation-value-args a)))))

(define (codegen-lisp-const var type)
  (let ((conversion-fn (output-conversion-fn type)))
    (setf (var-strict? var) '#f)
    (setf (var-arity var) 0)
    (setf (var-strictness var) '())
    `(define ,(fullname var)
             (delay
	       ,(if (eq? conversion-fn '#f)
		    (var-optimized-entry var)
		    `(,@conversion-fn ,(var-optimized-entry var)))))))

(define (maybe-expand-io-type ty)
  (cond ((and (ntycon? ty)
	      (eq? (ntycon-tycon ty) (core-symbol "IO")))
	 (**ntycon (core-symbol "Arrow")
		   (list (**ntycon (core-symbol "SystemState") '())
			 (**ntycon (core-symbol "IOResult")
				   (ntycon-args ty)))))
	((arrow-type? ty)
	 (**ntycon (core-symbol "Arrow")
		   (list (car (ntycon-args ty))
			 (maybe-expand-io-type (cadr (ntycon-args ty))))))
	(else ty)))

(define (gather-arg-types type)
  (if (arrow-type? type)
      (let ((a (ntycon-args type)))
	(cons (car a) (gather-arg-types (cadr a))))
      (list type)))
	   
(define (input-conversion-fn ty)
  (if (ntycon? ty)
      (let ((tycon (ntycon-tycon ty)))
	(cond ((eq? tycon (core-symbol "String"))
	       (lambda (x) `(haskell-string->string ,x)))
	      ((eq? tycon (core-symbol "List"))  ; needs to convert elements
	       (let ((var (gensym "X"))
		     (inner-fn (input-conversion-fn (car (ntycon-args ty)))))
		 (lambda (x) `(haskell-list->list
			       (lambda (,var)
				 ,(if (eq? inner-fn '#f)
				      var
				      (funcall inner-fn var)))
			       ,x))))
	      ((eq? tycon (core-symbol "Char"))
	       (lambda (x) `(integer->char ,x)))
	      (else '#f)))
      '#f))

(define (output-conversion-fn ty)
  (if (ntycon? ty)
      (let ((tycon (ntycon-tycon ty)))
	(cond ((eq? tycon (core-symbol "String"))
	       (lambda (x) `(make-haskell-string ,x)))
	      ((eq? tycon (core-symbol "List"))
	       (let ((var (gensym "X"))
		     (inner-fn (output-conversion-fn (car (ntycon-args ty)))))
		 (lambda (x) `(list->haskell-list
			       (lambda (,var)
				 ,(if (eq? inner-fn '#f)
				      var
				      (funcall inner-fn var)))
			       ,x))))
	      ((eq? tycon (core-symbol "UnitType"))
	       (lambda (x) `(insert-unit-value ,x)))
	      ((eq? tycon (core-symbol "IOResult"))
	       (lambda (x)
		 (let ((c1 (output-conversion-fn (car (ntycon-args ty)))))
		   `(box ,(apply-conversion c1 x)))))
	      (else '#f)))
      '#f))

(define (apply-conversion fn x)
  (if (eq? fn '#f)
      x
      (funcall fn x)))

(define (foreign-fn-needs-wrapper? var args)
 (if (lookup-annotation var '|NoConversion|)
     '#f
     (ffnw-1 args)))

(define (ffnw-1 args)
  (if (null? (cdr args))
      (not (eq? (output-conversion-fn (car args)) '#f))
      (or (not (eq? (input-conversion-fn (car args)) '#f))
	  (systemstate? (car args))
	  (ffnw-1 (cdr args)))))

(define (make-wrapper-fn var fn args)
  (mlet ((new-fn (symbol-append (fullname var) '|/wrapper|))
	 (avars (gen-temp-names (cdr args)))
	 (ignore-state? (systemstate? (cadr (reverse args))))
	 ((arg-conversions res-conversion)
	  (collect-conversion-fns avars args)))
     (values
      `(define (,new-fn ,@avars)
	 ,@(if ignore-state? `((declare (ignore ,(car (last avars)))))
	                     '())
	 ,@arg-conversions
	 ,(apply-conversion res-conversion
			    `(,fn ,@(if ignore-state?
					(butlast avars)
					avars))))
      new-fn)))

(define (collect-conversion-fns avars args)
  (if (null? avars)
      (values '() (output-conversion-fn (car args)))
      (mlet ((fn (input-conversion-fn (car args)))
	     ((c1 r) (collect-conversion-fns (cdr avars) (cdr args))))
	 (values (if (eq? fn '#f)
		     c1
		     `((setf ,(car avars) ,(funcall fn (car avars))) ,@c1))
		 r))))

(define (arrow-type? x)
  (and (ntycon? x)
       (eq? (ntycon-tycon x) (core-symbol "Arrow"))))

(define (systemstate? x)
  (and (ntycon? x)
       (eq? (ntycon-tycon x) (core-symbol "SystemState"))))

(define (gen-temp-names l)
  (gen-temp-names-1 l '(A B C D E F G H I J K L M N O P)))

(define (gen-temp-names-1 l1 l2)
  (if (null? l1)
      '()
      (if (null? l2)
	  (gen-temp-names-1 l1 (list (gensym "T")))
	  (cons (car l2) (gen-temp-names-1 (cdr l1) (cdr l2))))))

