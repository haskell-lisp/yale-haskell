;;; codegen.scm -- compile flic code to Lisp
;;;
;;; Author :  Sandra Loosemore
;;; Date   :  29 Apr 1992
;;;
;;; to do:  check completeness of special cases for constructors
;;;         constants still need work
;;;         optimized entry points
;;;
;;; The code generated here uses the following helper functions:
;;; (make-curried-fn opt-fn strictness)
;;;   make a curried function that calls opt-fn after collecting the
;;;   arguments and processing them according to strictness.  Both
;;;   the arguments are evaluated.
;;; (make-tuple-constructor arity)
;;;   return a function that makes an untagged data structure with "arity" 
;;;   slots.  "arity" is a constant.
;;; (make-tuple . args)
;;;   uncurried version of the above
;;; (make-tagged-data-constructor n arity)
;;;   return a function that makes a data structure with tag "n" and
;;;   "arity" slots.
;;; (make-tagged-data n . args)
;;;   uncurried version of the above
;;; (tuple-select arity i object)
;;;   extract component "i" from untagged "object"
;;; (tagged-data-select arity i object)
;;;   extract component "i" from tagged "object"
;;; (constructor-number object)
;;;   return the tag from "object"
;;; (delay form)
;;;   returns a delay object with unevaluated "form".
;;; (box form)
;;;   returns a delay object with evaluated "form".
;;; (force delay)
;;;   return the value of the delay object.
;;; (make-haskell-string string)
;;;   Converts a Lisp string lazily to a haskell string (using a magic
;;;   delay function).  Returns an unboxed result.



;;;======================================================================
;;; Code walker
;;;======================================================================


;;; Here is the main entry point.

(define (codegen-top big-let)
  (do ((bindings (flic-let-bindings big-let) (cdr bindings))
       (result   '())
       (decls    '()))
      ((null? bindings) `(begin ,@(nreverse decls) ,@(nreverse result)))
    (let ((var  (car bindings)))
      (push `(predefine ,(fullname var)) decls)
      (push (codegen-definition var (var-value var)) result))))


;;; See box.scm for more information about this...

(define (do-codegen object)
  (let ((x               (codegen object))
	(unboxed?        (flic-exp-unboxed? object))
	(strict-result?  (flic-exp-strict-result? object))
	(cheap?          (flic-exp-cheap? object)))
    (if unboxed?
	(if strict-result?
	    x
	    (if cheap?
		`(unbox ,x)
		`(force ,x)))
	(if strict-result?
	    (if cheap?
		`(box ,x)
		`(delay ,x))
	    (if cheap?
		x
		`(delay (force ,x)))))))
    

(define (do-codegen-list list)
  (map (function do-codegen) list))


(define-flic-walker codegen (object))


;;; For top-level definitions bound to lambda expressions, make both
;;; a standard entry point (with possibly unboxed arguments) and
;;; a standard entry point.

(define (codegen-definition var exp)
  (let ((fullname  (fullname var)))
    (when (or (memq 'codegen (dynamic *printers*))
	      (memq 'codegen-flic (dynamic *printers*)))
;       (format '#t "~%Codegen of ~A [~A]  " (def-name var) (struct-hash var))
       (format '#t "~%Codegen of ~A  " (def-name var))
       (when (not (var-strict? var))
	   (format '#t "Nonstrict  "))
       (when (not (eq? (var-strictness var) '()))
	   (format '#t "Strictness: ")
	   (dolist (s (var-strictness var))
	       (format '#t (if s "S " "N "))))
       (when (var-simple? var)
	   (format '#t " Inline "))
       (format '#t "~%")
       (when (memq 'codegen-flic (dynamic *printers*))
          (pprint* exp)))
    (let ((lisp-code
	   (if (not (flic-lambda? exp))
	       `(define ,fullname ,(do-codegen exp))
	       (let* ((optname  (optname var))
		      (lambda   (codegen-lambda-aux exp))
		      (def      `(define (,optname ,@(cadr lambda))
				                   ,@(cddr lambda))))
		 (if (var-selector-fn? var)
   	             ;; Standard entry point for selectors is never used.
		     def
		     `(begin
			,def
			(define ,fullname
			  ,(maybe-make-box-value
			    (codegen-curried-fn
			     `(function ,optname) (var-strictness var))
			    (var-strict? var)))))))))
      (when (or (memq 'codegen (dynamic *printers*))
		(memq 'codegen-flic (dynamic *printers*)))
	    (pprint* lisp-code))
      lisp-code)))

(define (codegen-lambda-list vars)
  (map (function fullname) vars))

(define (codegen-curried-fn opt-fn strictness)
  (if (null? (cdr strictness))
      ;; one-argument special cases
      (if (car strictness)
	  `(make-curried-fn-1-strict ,opt-fn)
	  `(make-curried-fn-1-nonstrict ,opt-fn))
      ;; general case
      `(make-curried-fn ,opt-fn ',strictness)))


;;; Curry lambdas.  Functions always return an unboxed value.

(define-codegen flic-lambda (object)
  (codegen-curried-fn
    (codegen-lambda-aux object)
    (map (lambda (x) (var-strict? x)) (flic-lambda-vars object))))

(define (codegen-lambda-aux object)
  (let* ((vars    (flic-lambda-vars object))
	 (ignore  '())
	 (args    (codegen-lambda-list vars)))
    (dolist (v vars)
      (if (eqv? (var-referenced v) 0)
	  (push (fullname v) ignore)))
    `(lambda ,args
       ,@(if (not (null? ignore))
	     `((declare (ignore ,@ignore)))
	     '())
       ,(do-codegen (flic-lambda-body object)))))


;;; This is only for non-top-level lets.
;;; The boxing of the value of each of the bindings is controlled by its
;;; strict? property.

(define-codegen flic-let (object)
  (let ((bindings   (flic-let-bindings object))
	(body       (flic-let-body object))
	(recursive? (flic-let-recursive? object)))
    (if recursive?
	(codegen-letrec bindings body)
	(codegen-let*   bindings body))))


;;; For efficiency reasons, we want to make all the function bindings
;;; in the function namespace (some implementations do not do tail-recursion
;;; or other optimizations correctly otherwise).  This means we have
;;; to sort out the variable bindings from the function bindings here.

(define (codegen-letrec bindings body)
  (let ((let-bindings     '())
	(labels-bindings  '()))
    (dolist (var bindings)
      (let ((value    (var-value var))
	    (fullname (fullname var))
	    (strict?  (var-strict? var)))
	(if (flic-lambda? value)
	    ;; Some functions may need only the optimized or standard
	    ;; entry points, but not both.
	    (let ((optname     (optname var))
		  (lambda      (codegen-lambda-aux value))
		  (optimized?  (var-optimized-refs? var))
		  (standard?   (var-standard-refs? var)))
	      (when standard?
		(push (list fullname
			    (maybe-make-box-value
			      (codegen-curried-fn
			        (if optimized? `(function ,optname) lambda)
				(var-strictness var))
			      strict?))
		      let-bindings))
	      (when optimized?
		(push (cons optname (cdr lambda)) labels-bindings)))
	    (push (list fullname (do-codegen value)) let-bindings))))
    (setf let-bindings (nreverse let-bindings))
    (setf labels-bindings (nreverse labels-bindings))
    (cond ((null? let-bindings)
	   `(labels ,labels-bindings ,(do-codegen body)))
	  ((null? labels-bindings)
	   `(letrec ,let-bindings ,(do-codegen body)))
	  (t
	   `(let ,(map (lambda (b) `(,(car b) '#f)) let-bindings)
	      (labels ,labels-bindings
		      ,@(map (lambda (b) `(setf ,@b)) let-bindings)
		      ,(do-codegen body))))
	  )))

(define (codegen-let* bindings body)
  (if (null? bindings)
      (do-codegen body)
      (let* ((var       (car bindings))
	     (value     (var-value var))
	     (fullname  (fullname var))
	     (strict?   (var-strict? var))
	     (body      (codegen-let* (cdr bindings) body)))
	(if (flic-lambda? value)
	    ;; Some functions may need only the optimized or standard
	    ;; entry points, but not both.
	    (let ((optname     (optname var))
		  (lambda      (codegen-lambda-aux value))
		  (optimized?  (var-optimized-refs? var))
		  (standard?   (var-standard-refs? var)))
	      (when standard?
		(setf body
		      (add-let-binding
		        (list fullname
			      (maybe-make-box-value
			        (codegen-curried-fn
				  (if optimized? `(function ,optname) lambda)
				  (var-strictness var))
				strict?))
			body)))
	      (when optimized?
		(setf body `(flet ((,optname ,@(cdr lambda))) ,body)))
	      body)
	    (add-let-binding (list fullname (do-codegen value)) body)))))

(define (add-let-binding binding body)
  (if (and (pair? body) (eq? (car body) 'let*))
      `(let* (,binding ,@(cadr body)) ,@(cddr body))
      `(let* (,binding) ,body)))


(define-codegen flic-app (object)
  (let ((fn         (flic-app-fn object))
	(args       (flic-app-args object))
	(saturated? (flic-app-saturated? object)))
    (cond ((and saturated? (flic-pack? fn))
	   ;; Saturated call to constructor
	   (codegen-constructor-app-aux
	     (flic-pack-con fn)
	     (do-codegen-list args)))
	  ((and saturated? (flic-ref? fn))
	   ;; Saturated call to named function
	   (let* ((var     (flic-ref-var fn))
		  (optname (optname var))
		  (argcode (do-codegen-list args)))
	     `(,optname ,@argcode)))
	  (else
	   ;; Have to make a curried call to standard entry point.
	   (let ((fncode   (do-codegen fn))
		 (argcode  (do-codegen-list args)))
	     (if (and (pair? fncode)
		      (eq? (car fncode) 'force))
		 `(funcall-force ,(cadr fncode) ,@argcode)
		 `(funcall ,fncode ,@argcode))))
	  )))

(define (codegen-constructor-app-aux con argcode)
  (let ((alg  (con-alg con)))
    (cond ((eq? con (core-symbol ":"))
	   `(cons ,@argcode))
	  ((algdata-implemented-by-lisp? alg)
	   (apply-maybe-lambda (cadr (con-lisp-fns con)) argcode))
	  ((algdata-tuple? alg)
	   `(make-tuple ,@argcode))
	  (else
	   `(make-tagged-data ,(con-tag con) ,@argcode)))))


(define-codegen flic-ref (object)
  (fullname (flic-ref-var object)))


(define-codegen flic-const (object)
  (let ((value   (flic-const-value object)))
    (cond ((string? value)
	   `(make-haskell-string ,value))
	  ((char? value)
	   ;; *** I think the parser ought to convert characters to their
	   ;; *** ASCII codes instead of doing it here.  There are problems
	   ;; *** with valid Haskell characters that can't be represented
	   ;; *** portably as Lisp characters.
	   (char->integer value))
	  ((number? value)
	   value)
	  (else
	   ;; It must be a ratio.  This is a bit of a hack - this depends on
	   ;; the fact that 2 tuples are represented in the same manner as
	   ;; rationals.  Hacked for strict rationals - jcp
	   `(make-tuple ,(car value) ,(cadr value)))
	  )))


;;; Returns a function or constant, so doesn't need to delay result.
;;; See flic-app for handling of saturated constructor calls.

(define-codegen flic-pack (object)
  (let* ((con        (flic-pack-con object))
	 (arity      (con-arity con))
	 (alg        (con-alg con))
	 (tuple?     (algdata-tuple? alg))
	 (strictness (con-slot-strict? con))
	 (index      (con-tag con)))
    (cond ((eq? con (core-symbol "Nil"))
	   ''())
	  ((eq? con (core-symbol "True"))
	   ''#t)
	  ((eq? con (core-symbol "False"))
	   ''#f)
	  ((eq? con (core-symbol ":"))
	   '(function make-cons-constructor))
	  ((algdata-implemented-by-lisp? alg)
	   (let ((fn (cadr (con-lisp-fns con))))
	     (if (eqv? (con-arity con) 0)
		 fn
		 (codegen-curried-fn
		  (if (and (pair? fn) (eq? (car fn) 'lambda))
		      fn
		      `(function ,fn))
		  strictness))))
	  ((algdata-enum? alg)
	   ;; All constructors have 0 arity; represent them just
	   ;; by numbers.
	   index)
	  (tuple?
	   ;; Only a single constructor for this type.
	   (codegen-curried-fn
	     `(make-tuple-constructor ,arity)
	     strictness))
	  ((eqv? arity 0)
	   ;; No arguments to this constructor.
	   `(make-tagged-data ,index))
	  (else
	   ;; General case.
	   (codegen-curried-fn
	    `(make-tagged-data-constructor ,index ,arity)
	    strictness))
	  )))



;;; These expressions translate directly into their Lisp equivalents.

(define-codegen flic-case-block (object)
  `(block ,(flic-case-block-block-name object)
     ,@(do-codegen-list (flic-case-block-exps object))))

(define-codegen flic-return-from (object)
  `(return-from ,(flic-return-from-block-name object)
		,(do-codegen (flic-return-from-exp object))))

(define-codegen flic-and (object)
  `(and ,@(do-codegen-list (flic-and-exps object))))

(define-codegen flic-if (object)
  `(if ,(do-codegen (flic-if-test-exp object))
       ,(do-codegen (flic-if-then-exp object))
       ,(do-codegen (flic-if-else-exp object))))

(define-codegen flic-sel (object)
  (codegen-flic-sel-aux
    (flic-sel-con object)
    (flic-sel-i object)
    (do-codegen (flic-sel-exp object))))

(define (codegen-flic-sel-aux con index exp)
  (let* ((alg      (con-alg con))
	 (tuple?   (algdata-tuple? alg))
	 (arity    (con-arity con)))
    (cond ((eq? con (core-symbol ":"))
	   (if (eqv? index 0)
	       `(car ,exp)
	       `(cdr ,exp)))
	  ((algdata-implemented-by-lisp? alg)
	   (apply-maybe-lambda (list-ref (cddr (con-lisp-fns con)) index)
			       (list exp)))
	  (tuple?
	   `(tuple-select ,arity ,index ,exp))
	  (else
	   `(tagged-data-select ,arity ,index ,exp))
	  )))

(define-codegen flic-is-constructor (object)
  (codegen-flic-is-constructor-aux
    (flic-is-constructor-con object)
    (do-codegen (flic-is-constructor-exp object))))

(define (codegen-flic-is-constructor-aux con exp)
  (let ((type (con-alg con)))
    (cond ((eq? type (core-symbol "Bool"))
	   (if (eq? con (core-symbol "True"))
	       exp
	       `(not ,exp)))
	  ((eq? type (core-symbol "List"))
	   (if (eq? con (core-symbol ":"))
	       `(pair? ,exp)
	       `(null? ,exp)))
	  ((algdata-implemented-by-lisp? type)
	   (let ((fn (car (con-lisp-fns con))))
	     (apply-maybe-lambda fn (list exp))))
	  ((algdata-tuple? type)
	   ;; This should never happen.
	   ''#t)
	  ((algdata-enum? type)
	   `(eqv? (the fixnum ,exp) (the fixnum ,(con-tag con))))
	  (else
	   `(eqv? (the fixnum (constructor-number ,exp))
		  (the fixnum ,(con-tag con))))
	  )))


(define-codegen flic-con-number (object)
  (let ((type   (flic-con-number-type object))
	(exp    (do-codegen (flic-con-number-exp object))))
    `(the fixnum
	  ,(cond ((eq? type (core-symbol "Bool"))
		  `(if ,exp 1 0))
		 ((eq? type (core-symbol "List"))
		  `(if (pair? ,exp) 0 1))
		 ((algdata-tuple? type)
		  ;; This should never happen.
		  0)
		 ((algdata-implemented-by-lisp? type)
		  (let ((var (gensym)))
		    `(let ((,var ,exp))
		       (cond ,@(map (lambda (con)
				      `(,(apply-maybe-lambda
					  (car (con-lisp-fns con))
					  (list var))
					',(con-tag con)))
				    (algdata-constrs type))
			     (else (error "No constructor satisfies ~A.~%"
					  ',(def-name type)))))))
		 ((algdata-enum? type)
		  exp)
		 (else
		  `(constructor-number ,exp))
		 ))
    ))



;;;======================================================================
;;; Utility functions
;;;======================================================================

;;; Here are some helper functions for handing boxing and unboxing
;;; of values.
;;; maybe-make-box-delay is used to box forms that are "expensive" to
;;; compute; maybe-make-box-value is used to box forms like constants
;;; or functions that are "cheap" to compute eagerly.
;;; Maybe-unbox is used to unbox a form that returns a boxed result.

(define (maybe-make-box-delay form unboxed?)
  (if unboxed?
      form
      `(delay ,form)))

(define (maybe-make-box-value form unboxed?)
  (if unboxed?
      form
      `(box ,form)))

(define (maybe-unbox form unboxed?)
  (if unboxed?
      `(force ,form)
      form))


;;; These two var slots are filled in lazily by the code generator,
;;; since most vars generated don't need them.  You should always
;;; use these functions instead of accessing the structure slot
;;; directly.

(define (fullname var)
  (or (var-fullname var)
      (setf (var-fullname var)
	    (if (var-toplevel? var)
		;; For toplevel names, use module name glued onto base names.
		;; These are always interned symbols.
		(if (def-core? var)
		    (symbol-append '|*Core:| (def-name var))
		    (symbol-append (def-module var) '\: (def-name var)))
		;; Otherwise, make sure we have a gensym.
		;; The uniquification of interned symbols is required
		;; because there may be multiple nested bindings of the
		;; same name, and we want to be able to distinguish between
		;; the different bindings.
		(let ((name  (def-name var)))
		  (if (gensym? name)
		      name
		      (gensym (symbol->string name))))))
      ))

(define (optname var)
  (or (var-optimized-entry var)
      (let ((name  (string-append (symbol->string (fullname var)) "/OPT")))
	(setf (var-optimized-entry var)
	      (if (var-toplevel? var)
		  (string->symbol name)
		  (gensym name))))))



;;;======================================================================
;;; Exported functions
;;;======================================================================

;;; This handles types exported to lisp from Haskell
;;; *** Is this really supposed to create variable bindings as
;;; *** opposed to function bindings???
;;; *** I assume all of these functions want strict arguments and return
;;; *** strict results, even if the data structures contain boxed values.

(define (codegen-exported-types mods)
  (let ((defs '()))
    (dolist (m mods)
      (dolist (a (module-alg-defs m))
        (when (algdata-export-to-lisp? a)
	  (dolist (c (algdata-constrs a))
	    (setf defs (nconc (codegen-constr c) defs))))))
    `(begin ,@defs)))

(define (codegen-constr c)
  (let ((lisp-fns (con-lisp-fns c)))
    (if c
        (let ((res
	       `(,(codegen-lisp-predicate (car lisp-fns) c)
		 ,(codegen-lisp-constructor (cadr lisp-fns) c)
		 ,@(codegen-lisp-accessors
		    (cddr lisp-fns) (con-slot-strict? c) c 0))))
	  (when (memq 'codegen (dynamic *printers*))
	    (dolist (d res)
	      (pprint* d)))
	  res)
	'())))

(define (codegen-lisp-predicate name c)
  `(define (,name x)
     ,(codegen-flic-is-constructor-aux c 'x)))

(define (codegen-lisp-constructor name c)
  (let ((strictness (con-slot-strict? c))
	(args       '())
	(exps       '()))
    (dolist (s strictness)
      (let ((arg  (gensym)))
	(push arg args)
	(push (if s arg `(box ,arg)) exps)))
    `(define (,name ,@(nreverse args))
	 ,(codegen-constructor-app-aux c (nreverse exps)))))

(define (codegen-lisp-accessors names strictness c i)
  (declare (type fixnum i))
  (if (null? names)
      '()
      (let ((body  (codegen-flic-sel-aux c i 'x)))
	(when (not (car strictness))
	  (setf body `(force ,body)))
	(cons `(define (,(car names) x) ,body)
	      (codegen-lisp-accessors (cdr names) (cdr strictness) c (+ i 1))))
    ))



;;; This is a special hack needed due to brain-dead common lisp problems.
;;; This allows the user to place lambda defined functions in ImportLispType
;;; *** I'm not convinced this is necessary;  ((lambda ...) args)
;;; *** is perfectly valid Common Lisp syntax!

(define (apply-maybe-lambda fn args)
  (if (and (pair? fn)
	   (eq? (car fn) 'lambda))
      `(funcall ,fn ,@args)
      `(,fn ,@args)))
