;;; optimize.scm -- flic optimizer
;;;
;;; author :  Sandra Loosemore
;;; date   :  7 May 1992
;;;
;;;
;;; The optimizer does these kinds of program transformations:
;;;
;;; * remove unreferenced variable bindings.
;;;
;;; * constant folding and various other kinds of compile-time
;;;   evaluation.
;;;
;;; * beta reduction (replace references to variables bound to simple
;;;   expressions with the expression)
;;; 


;;; Since some of the optimizations can make additional transformations
;;; possible, we want to make multiple iteration passes.  But since each
;;; pass is likely to have diminishing benefits, we don't want to keep
;;; iterating indefinitely.  So establish a fairly arbitrary cutoff point.
;;; The value is based on empirical results from compiling the prelude.

(define *max-optimize-iterations* 5)
(define *optimize-foldr-iteration* 0)  ; when to inline foldr
(define *optimize-build-iteration* 0)  ; when to inline build
(define *current-optimize-iteration* 0)


;;; Flags for enabling various optimizations

(define *all-optimizers* '(foldr inline constant lisp))
(define *optimizers* *all-optimizers*)


;;; Used to note whether we are doing the various optimizations

(define-local-syntax (do-optimization? o)
  `(memq ,o (dynamic *optimizers*)))

(define *do-foldr-optimizations* (do-optimization? 'foldr))
(define *do-inline-optimizations* (do-optimization? 'inline))
(define *do-constant-optimizations* (do-optimization? 'constant))


;;; If the foldr optimization is enabled, bind the corresponding
;;; variables to these values instead of the defaults.

(define *foldr-max-optimize-iterations* 15)
(define *foldr-optimize-foldr-iteration* 8)
(define *foldr-optimize-build-iteration* 5)


;;; Some random other variables

(define *structured-constants* '())
(define *structured-constants-table* '#f)
(define *lambda-depth* 0)
(define *local-bindings* '())


;;; This is for doing some crude profiling.  
;;; Comment out the body of the macro to disable profiling.

;;; Here are current counts from compiling the prelude:
;;; (LET-REMOVE-UNUSED-BINDING . 5835) 
;;; (REF-INLINE-SINGLE-REF . 2890) 
;;; (REF-INLINE . 2692) 
;;; (LET-EMPTY-BINDINGS . 2192) 
;;; (APP-LAMBDA-TO-LET . 1537) 
;;; (APP-MAKE-SATURATED . 416) 
;;; (LET-HOIST-RETURN-FROM . 310) 
;;; (CASE-BLOCK-IDENTITY . 273) 
;;; (CASE-BLOCK-DEAD-CODE . 234) 
;;; (CASE-BLOCK-TO-IF . 212) 
;;; (SEL-FOLD-VAR . 211) 
;;; (APP-HOIST-LET . 190) 
;;; (LET-HOIST-LAMBDA . 181) 
;;; (FOLDR-INLINE . 176) 
;;; (AND-UNARY . 172) 
;;; (LAMBDA-COMPRESS . 168) 
;;; (APP-FOLD-SELECTOR . 141) 
;;; (BUILD-INLINE-LAMBDA . 134) 
;;; (LET-COMPRESS . 134) 
;;; (IF-FOLD . 128) 
;;; (INTEGER-TO-INT-CONSTANT-FOLD . 124) 
;;; (AND-COMPRESS . 94) 
;;; (APP-COMPRESS . 93) 
;;; (FOLDR-CONS-IDENTITY . 69) 
;;; (IF-COMPRESS-TEST . 65) 
;;; (IF-HOIST-LAMBDA . 61) 
;;; (APP-HOIST-STRUCTURED-CONSTANT . 60) 
;;; (FOLDR-PRIM-APPEND-INLINE . 55) 
;;; (FOLDR-BUILD-IDENTITY . 40) 
;;; (CASE-BLOCK-DISCARD-REDUNDANT-TEST . 37) 
;;; (FOLDR-NIL-IDENTITY . 36) 
;;; (LET-HOIST-INVARIANT-ARGS . 30) 
;;; (FOLDR-HOIST-LET . 28) 
;;; (CON-NUMBER-FOLD-TUPLE . 21) 
;;; (FOLDR-CONS-NIL-IDENTITY . 15) 
;;; (AND-CONTAINS-TRUE . 14) 
;;; (IF-IDENTITY-INVERSE . 8) 
;;; (IF-HOIST-RETURN-FROM . 7) 
;;; (CASE-BLOCK-HOIST-LET . 7) 
;;; (INTEGER-TO-INT-IDENTITY . 7) 
;;; (APP-PACK-IDENTITY . 2) 
;;; (CON-NUMBER-FOLD . 2) 
;;; (IF-IDENTITY . 2) 
;;; (INT-TO-INTEGER-CONSTANT-FOLD . 2) 
;;; (LET-HOIST-STRUCTURED-CONSTANT . 1) 


(define-local-syntax (record-hack type . args)
  (declare (ignore args))
  `',type
;  `(record-hack-aux ,type ,@args)
  )

(define *hacks-done* '())

(define (record-hack-aux type . args)
  ;; *** debug
  ;; (format '#t "~s ~s~%" type args)
  (declare (ignore args))
  (let ((stuff  (assq type (car (dynamic *hacks-done*)))))
    (if stuff
	(incf (cdr stuff))
	(push (cons type 1) (car (dynamic *hacks-done*))))))

(define (total-hacks)
  (let ((totals  '()))
    (dolist (alist *hacks-done*)
      (dolist (entry alist)
	(let ((stuff  (assq (car entry) totals)))
	  (if stuff
	      (setf (cdr stuff) (+ (cdr stuff) (cdr entry)))
	      (push (cons (car entry) (cdr entry)) totals)))))
    totals))


;;; This is the main entry point.

(define (optimize-top object)
  (dynamic-let ((*structured-constants*       '())
		(*structured-constants-table* (make-table))
		(*lambda-depth*               0)
		(*local-bindings*             '())
		(*do-inline-optimizations*
		  (do-optimization? 'inline))
		(*do-constant-optimizations*
		  (do-optimization? 'constant))
		(*max-optimize-iterations*
		  (if (do-optimization? 'foldr)
		      (dynamic *foldr-max-optimize-iterations*)
		      (dynamic *max-optimize-iterations*)))
		(*optimize-foldr-iteration*
		  (if (do-optimization? 'foldr)
		      (dynamic *foldr-optimize-foldr-iteration*)
		      (dynamic *optimize-foldr-iteration*)))
		(*optimize-build-iteration*
		  (if (do-optimization? 'foldr)
		      (dynamic *foldr-optimize-build-iteration*)
		      (dynamic *optimize-build-iteration*))))
    (setf *hacks-done* '())
    (dotimes (i (dynamic *max-optimize-iterations*))
      (dynamic-let ((*current-optimize-iteration*  i))
;; debug	    (*duplicate-object-table*      (make-table)))
	(when (memq 'optimize-extra (dynamic *printers*))
	  (format '#t "~%Optimize pass ~s:" i)
	  (pprint object))
        (push '() *hacks-done*)
	(setf object (optimize-flic-let-aux object '#t))))
    (setf (flic-let-bindings object)
	  (nconc (nreverse (dynamic *structured-constants*))
		 (flic-let-bindings object))))
  (install-uninterned-globals (flic-let-bindings object))
  (postoptimize object)
  object)


(define-flic-walker optimize (object))

;;; debugging stuff
;;; 
;;; (define *duplicate-object-table* (make-table))
;;; 
;;; (define (new-optimize object)
;;;   (if (table-entry (dynamic *duplicate-object-table*) object)
;;;       (error "Duplicate object ~s detected." object)
;;;       (begin
;;; 	(setf (table-entry (dynamic *duplicate-object-table*) object) '#t)
;;; 	(old-optimize object))))
;;; 
;;; (lisp:setf (lisp:symbol-function 'old-optimize)
;;; 	   (lisp:symbol-function 'optimize))
;;; (lisp:setf (lisp:symbol-function 'optimize)
;;;  	   (lisp:symbol-function 'new-optimize))

(define (optimize-list objects)
  (optimize-list-aux objects)
  objects)

(define (optimize-list-aux objects)
  (if (null? objects)
      '()
      (begin
        (setf (car objects) (optimize (car objects)))
	(optimize-list-aux (cdr objects)))))


;;; Compress nested lambdas.  This hack is desirable because saturating
;;; applications within the lambda body effectively adds additional 
;;; parameters to the function.

;;; *** Maybe this should look for hoistable constant lambdas too.

(define-optimize flic-lambda (object)
  (let ((vars  (flic-lambda-vars object)))
    (dynamic-let ((*lambda-depth*   (1+ (dynamic *lambda-depth*)))
		  (*local-bindings* (cons vars (dynamic *local-bindings*))))
      (dolist (var vars)
	(setf (var-referenced var) 0))
      (let ((new-body  (optimize (flic-lambda-body object))))
	(setf (flic-lambda-body object) new-body)
	(cond ((is-type? 'flic-lambda new-body)
	       (record-hack 'lambda-compress)
	       (setf (flic-lambda-vars object)
		     (nconc (flic-lambda-vars object)
			    (flic-lambda-vars new-body)))
	       (setf (flic-lambda-body object) (flic-lambda-body new-body)))
	      (else
	       '#f))
	object))))


;;; For let, first mark all variables as unused and check for "simple"
;;; binding values that permit beta reduction.  Then walk the subexpressions.
;;; Finally discard any bindings that are still marked as unused.
;;; *** This fails to detect unused recursive variables.

(define-optimize flic-let (object)
  (optimize-flic-let-aux object '#f))

(define (optimize-flic-let-aux object toplevel?)
  (let ((bindings      (flic-let-bindings object))
	(recursive?    (flic-let-recursive? object)))
    ;; *** This handling of *local-bindings* isn't quite right since
    ;; *** it doesn't account for the sequential nature of bindings
    ;; *** in a non-recursive let, but it's close enough.  We won't
    ;; *** get any semantic errors, but it might miss a few optimizations.
    (dynamic-let ((*local-bindings*
		    (if (and recursive? (not toplevel?))
			(cons bindings (dynamic *local-bindings*))
			(dynamic *local-bindings*))))
      (optimize-flic-let-bindings bindings recursive? toplevel?)
      (dynamic-let ((*local-bindings*
		      (if (and (not recursive?) (not toplevel?))
			  (cons bindings (dynamic *local-bindings*))
			  (dynamic *local-bindings*))))
	(setf (flic-let-body object) (optimize (flic-let-body object))))
      ;; Check for unused bindings and other rewrites.
      ;; Only do this for non-toplevel lets.
      (if toplevel?
	  object
	  (optimize-flic-let-rewrite object bindings recursive?)))))

(define (optimize-flic-let-bindings bindings recursive? toplevel?)
  ;; Initialize
  (dolist (var bindings)
    (setf (var-referenced var) 0)
    (setf (var-fn-referenced var) 0)
    (when (is-type? 'flic-lambda (var-value var))
      (dolist (v (flic-lambda-vars (var-value var)))
	(setf (var-arg-invariant? v) '#t)
	(setf (var-arg-invariant-value v) '#f))))
  ;; Traverse value subforms
  (do ((bindings bindings (cdr bindings)))
      ((null? bindings) '#f)
      (let* ((var  (car bindings))
	     (val  (var-value var)))
	(if (and (is-type? 'flic-app val)
		 (dynamic *do-constant-optimizations*)
		 (let ((fn   (flic-app-fn val))
		       (args (flic-app-args val)))
		   (if recursive?
		       (structured-constant-app-recursive?
			 fn args bindings (list var))
		       (structured-constant-app? fn args))))
	    ;; Variable is bound to a structured constant.  If this
	    ;; isn't already a top-level binding, replace the value
	    ;; of the constant with a reference to a top-level variable
	    ;; that is in turn bound to the constant expression.
	    ;; binding to top-level if this is a new constant.
	    ;; *** Maybe we should also look for variables bound
	    ;; *** to lambdas, that can also be hoisted to top level.
	    (when (not toplevel?)
	      (multiple-value-bind (con args cvar)
		  (enter-structured-constant-aux val '#t)
		(record-hack 'let-hoist-structured-constant)
		(if cvar
		    (setf (var-value var) (make-flic-ref cvar))
		    (add-new-structured-constant var con args))))
	    (begin
	      ;; If this is a function that's a candidate for foldr/build
	      ;; optimization, stash the value away prior to
	      ;; inlining the calls.
	      ;; *** We might try to automagically detect functions
	      ;; *** that are candidates for these optimizations here,
	      ;; *** but have to watch out for infinite loops!
	      (when (and (var-force-inline? var)
			 (eqv? (the fixnum
				    (dynamic *current-optimize-iteration*))
			       (the fixnum
				    (dynamic *optimize-build-iteration*)))
			 (is-type? 'flic-lambda val)
			 (or (is-foldr-or-build-app? (flic-lambda-body val))))
		(setf (var-inline-value var) (copy-flic-top val)))
	      ;; Then walk value normally.
	      (let ((new-val  (optimize val)))
		(setf (var-value var) new-val)
		(setf (var-simple? var)
		      (or (var-force-inline? var)
			  (and (not (var-selector-fn? var))
			       (can-inline?
				 new-val
				 (if recursive? bindings '())
				 toplevel?))))))
	  ))))


(define (is-foldr-or-build-app? exp)
  (typecase exp
    (flic-app
     (let ((fn  (flic-app-fn exp)))
       (and (is-type? 'flic-ref fn)
	    (or (eq? (flic-ref-var fn) (core-symbol "foldr"))
		(eq? (flic-ref-var fn) (core-symbol "build"))))))
    (flic-let
     (is-foldr-or-build-app? (flic-let-body exp)))
    (flic-ref
     (let ((val  (var-value (flic-ref-var exp))))
       (and val (is-foldr-or-build-app? val))))
    (else
     '#f)))


(define (optimize-flic-let-rewrite object bindings recursive?)
  ;; Delete unused variables from the list.
  (setf bindings
	(list-delete-if
	  (lambda (var)
	    (cond ((var-toplevel? var)
		   ;; This was a structured constant hoisted to top-level.
		   '#t)
	          ((eqv? (the fixnum (var-referenced var)) (the fixnum 0))
		   (record-hack 'let-remove-unused-binding var)
		   '#t)
		  ((eqv? (the fixnum (var-referenced var)) (the fixnum 1))
		   (setf (var-single-ref var) (dynamic *lambda-depth*))
		   '#f)
		  (else
		   (setf (var-single-ref var) '#f)
		   '#f)))
	  bindings))
  ;; Add extra bindings for reducing functions with invariant
  ;; arguments.  Hopefully some of the extra bindings will go
  ;; away in future passes!
  (setf (flic-let-bindings object)
	(setf bindings (add-stuff-for-invariants bindings)))
  ;; Look for other special cases.
  (cond ((null? bindings)
	 ;; Simplifying the expression by getting rid of the LET may
	 ;; make it possible to do additional optimizations on the 
	 ;; next pass.
	 (record-hack 'let-empty-bindings)
	 (flic-let-body object))
	((is-type? 'flic-return-from (flic-let-body object))
	 ;; Hoist return-from outside of LET.  This may permit
	 ;; further optimizations by an enclosing case-block.
	 (record-hack 'let-hoist-return-from)
	 (let* ((body       (flic-let-body object))
		(inner-body (flic-return-from-exp body)))
	   (setf (flic-return-from-exp body) object)
	   (setf (flic-let-body object) inner-body)
	   body))
	((and (not recursive?)
	      (is-type? 'flic-let (flic-let-body object))
	      (not (flic-let-recursive? (flic-let-body object))))
	 ;; This is purely to produce more compact code.
	 (record-hack 'let-compress)
	 (let ((body  (flic-let-body object)))
	   (setf (flic-let-bindings object)
		 (nconc bindings (flic-let-bindings body)))
	   (setf (flic-let-body object) (flic-let-body body))
	   object))
	((is-type? 'flic-lambda (flic-let-body object))
	 ;; Hoist lambda outside of LET.  This may permit
	 ;; merging of nested lambdas on a future pass.
	 (record-hack 'let-hoist-lambda)
	 (let* ((body       (flic-let-body object))
		(inner-body (flic-lambda-body body)))
	   (setf (flic-lambda-body body) object)
	   (setf (flic-let-body object) inner-body)
	   body))
	(else
	 object))
  )

;;; Look for constant-folding and structured constants here.

(define-optimize flic-app (object)
  (optimize-flic-app-aux object))

(define (optimize-flic-app-aux object)
  (let ((new-fn   (optimize (flic-app-fn object)))
	(new-args (optimize-list (flic-app-args object))))
    (typecase new-fn
      (flic-ref
       ;; The function is a variable.
       (let* ((var    (flic-ref-var new-fn))
	      (val    (var-value var))
	      (n      (length new-args))
	      (arity  (guess-function-arity var)))
	 (cond ((and arity (< (the fixnum n) (the fixnum arity)))
		;; This is a first-class call that is not fully saturated.
		;; Make it saturated by wrapping a lambda around it.
		(setf new-fn
		      (do-app-make-saturated object new-fn new-args arity n))
		(setf new-args '()))
	       ((var-selector-fn? var)
		;; This is a saturated call to a selector.  We might
		;; be able to inline the call.
		(multiple-value-bind (fn args)
		    (try-to-fold-selector var new-fn new-args)
		  (setf new-fn fn)
		  (setf new-args args)))
	       ((and (not (var-toplevel? var))
		     (is-type? 'flic-lambda val))
		;; This is a saturated call to a local function.
		;; Increment its reference count and note if any of
		;; the arguments are invariant.
		(incf (var-fn-referenced var))
		(note-invariant-args new-args (flic-lambda-vars val)))
	       (else
		(let ((magic  (magic-optimize-function var)))
		  (when magic
		    (multiple-value-bind (fn args)
			(funcall magic new-fn new-args)
		      (setf new-fn fn)
		      (setf new-args args)))))
	       )))
      (flic-lambda
       ;; Turn application of lambda into a let.
       (multiple-value-bind (fn args)
	   (do-lambda-to-let-aux new-fn new-args)
	 (setf new-fn fn)
	 (setf new-args args)))
      (flic-pack
       (let ((con  (flic-pack-con new-fn))
	     (temp '#f))
	 (when (eqv? (length new-args) (con-arity con))
	   (cond ((and (dynamic *do-constant-optimizations*)
		       (every-1 (function structured-constant?) new-args))
		  ;; This is a structured constant that
		  ;; can be replaced with a top-level binding.
		  (setf (flic-app-fn object) new-fn)
		  (setf (flic-app-args object) new-args)
		  (record-hack 'app-hoist-structured-constant object)
		  (setf new-fn (enter-structured-constant object '#t))
		  (setf new-args '()))
		 ((and (setf temp (is-selector? con 0 (car new-args)))
		       (is-selector-list? con 1 temp (cdr new-args)))
		  ;; This is an expression like (cons (car x) (cdr x)).
		  ;; Replace it with just plain x to avoid reconsing.
		  (record-hack 'app-pack-identity new-fn)
		  (setf new-fn (copy-flic-top temp))
		  (setf new-args '()))
		 ))))
      (flic-let
       ;; Hoist let to surround entire application.
       ;; Simplifying the function being applied may permit further
       ;; optimizations on next pass.
       ;; (We might try to hoist lets in the argument expressions, too,
       ;; but I don't think that would lead to any real simplification
       ;; of the code.)
       (record-hack 'app-hoist-let)
       (setf (flic-app-fn object) (flic-let-body new-fn))
       (setf (flic-app-args object) new-args)
       (setf new-args '())
       (setf (flic-let-body new-fn) object)
       )
      (flic-app
       ;; Try to compress nested applications.
       ;; This may make the call saturated and permit further optimizations
       ;; on the next pass.
       (record-hack 'app-compress)
       (setf new-args (nconc (flic-app-args new-fn) new-args))
       (setf new-fn (flic-app-fn new-fn)))
      )
    (if (null? new-args)
	new-fn
	(begin
	  (setf (flic-app-fn object) new-fn)
	  (setf (flic-app-args object) new-args)
	  object))
    ))

(define (guess-function-arity var)
  (or (let ((value  (var-value var)))
	(and value
	     (is-type? 'flic-lambda value)
	     (length (flic-lambda-vars value))))
      (var-arity var)))

(define (do-app-make-saturated app fn args arity nargs)
  (declare (type fixnum arity nargs))
  (record-hack 'app-make-saturated fn args)
  (let ((newvars  '())
	(newargs  '()))
    (dotimes (i (- arity nargs))
      (declare (type fixnum i))
      (let ((v  (init-flic-var (create-temp-var 'arg) '#f '#f)))
	(push v newvars)
	(push (make-flic-ref v) newargs)))
    (setf (flic-app-fn app) fn)
    (setf (flic-app-args app) (nconc args newargs))
    (make-flic-lambda newvars app)))



;;; If the function is a selector applied to a literal dictionary,
;;; inline it.

(define (try-to-fold-selector var new-fn new-args)
  (let ((exp  (car new-args)))
    (if (or (and (is-type? 'flic-ref exp)
		 ;; *** should check that var is top-level?
		 (is-bound-to-constructor-app? (flic-ref-var exp)))
	    (and (is-type? 'flic-app exp)
		 (is-constructor-app-prim? exp)))
	(begin
	  (record-hack 'app-fold-selector)
	  (setf new-fn (copy-flic-top (var-value var)))
	  (do-lambda-to-let-aux new-fn new-args))
	(values new-fn new-args))))


;;; Various primitive functions have special optimizer functions
;;; associated with them, that do constant folding and certain
;;; other identities.  The optimizer function is called with the 
;;; function expression and list of argument expressions (at least
;;; as many arguments as the arity of the function) and should return
;;; the two values.

;;; *** This should really use some kind of hash table, but we'd
;;; *** have to initialize the table dynamically because core-symbols
;;; *** aren't defined when this file is loaded.

(define (magic-optimize-function var)
  (cond ((eq? var (core-symbol "foldr"))
	 (function optimize-foldr-aux))
	((eq? var (core-symbol "build"))
	 (function optimize-build))
	((eq? var (core-symbol "primIntegerToInt"))
	 (function optimize-integer-to-int))
	((eq? var (core-symbol "primIntToInteger"))
	 (function optimize-int-to-integer))
	((eq? var (core-symbol "primRationalToFloat"))
	 (function optimize-rational-to-float))
	((eq? var (core-symbol "primRationalToDouble"))
	 (function optimize-rational-to-double))
	((or (eq? var (core-symbol "primNegInt"))
	     (eq? var (core-symbol "primNegInteger"))
	     (eq? var (core-symbol "primNegFloat"))
	     (eq? var (core-symbol "primNegDouble")))
	 (function optimize-neg))
	(else
	 '#f)))


;;; Foldr identities for deforestation

(define (optimize-foldr fn args)
  (multiple-value-bind (fn args)
      (optimize-foldr-aux fn args)
    (maybe-make-app fn args)))

(define (optimize-foldr-aux fn args)
  (let ((k     (car args))
	(z     (cadr args))
	(l     (caddr args))
	(tail  (cdddr args)))
    (cond ((and (is-type? 'flic-pack k)
		(eq? (flic-pack-con k) (core-symbol ":"))
		(is-type? 'flic-pack z)
		(eq? (flic-pack-con z) (core-symbol "Nil")))
	   ;; foldr (:) [] l ==> l
	   ;; (We arrange for build to be inlined before foldr
	   ;; so that this pattern can be detected.)
	   (record-hack 'foldr-cons-nil-identity)
	   (values l tail))
	  ((and (is-type? 'flic-app l)
		(is-type? 'flic-ref (flic-app-fn l))
		(eq? (flic-ref-var (flic-app-fn l))
		     (core-symbol "build"))
		(null? (cdr (flic-app-args l))))
	   ;; foldr k z (build g) ==> g k z
	   (record-hack 'foldr-build-identity)
	   (values
	     (car (flic-app-args l))
	     (cons k (cons z tail))))
	  ((and (is-type? 'flic-pack l)
		(eq? (flic-pack-con l) (core-symbol "Nil")))
	   ;; foldr k z [] ==> z
	   (record-hack 'foldr-nil-identity)
	   (values z tail))
	  ((short-string-constant? l)
	   ;; If the list argument is a string constant, expand it inline.
	   ;; Only do this if the string is fairly short, though.
	   (optimize-foldr-aux
	     fn
	     (cons k (cons z (cons (expand-string-constant l) tail)))))
	  ((and (is-type? 'flic-app l)
		(is-type? 'flic-pack (flic-app-fn l))
		(eq? (flic-pack-con (flic-app-fn l)) (core-symbol ":"))
		(eqv? (length (flic-app-args l)) 2))
	   ;; foldr k z x:xs ==> let c = k in c x (foldr c z xs)
	   (record-hack 'foldr-cons-identity)
	   (let ((x     (car (flic-app-args l)))
		 (xs    (cadr (flic-app-args l))))
	     (values 
	       (if (can-inline? k '() '#f)
		   (do-foldr-cons-identity k z x xs)
		   (let ((cvar  (init-flic-var (create-temp-var 'c) k '#f)))
		     (make-flic-let
		       (list cvar)
		       (do-foldr-cons-identity (make-flic-ref cvar) z x xs)
		       '#f)))
	       tail)))
	  ((is-type? 'flic-let l)
	   ;; foldr k z (let bindings in body) ==>
	   ;;   let bindings in foldr k z body
	   (record-hack 'foldr-hoist-let)
	   (setf (flic-let-body l)
		 (optimize-foldr fn (list k z (flic-let-body l))))
	   (values l tail))
	  ((not (eqv? (the fixnum (dynamic *current-optimize-iteration*))
		      (the fixnum (dynamic *optimize-foldr-iteration*))))
	   ;; Hope for more optimizations later.
	   (values fn args))
	  ((and (is-type? 'flic-pack k)
		(eq? (flic-pack-con k) (core-symbol ":")))
	   ;; Inline to special case, highly optimized append primitive.
	   ;; Could also look for (++ (++ l1 l2) l3) => (++ l1 (++ l2 l3))
	   ;; here, but I don't think that happens very often.
           (record-hack 'foldr-prim-append-inline)
	   (values
	     (make-flic-ref (core-symbol "primAppend"))
	     (cons l (cons z tail))))
	  (else
	   ;; Default inline.
	   (record-hack 'foldr-inline k z)
	   (let ((new-fn
		   (copy-flic-top (var-value (core-symbol "inlineFoldr")))))
	     (if (is-type? 'flic-lambda new-fn)
		 (do-lambda-to-let-aux new-fn args)
		 (values new-fn args))))
	  )))


;;; Mess with compile-time expansion of short string constants.

(define-integrable max-short-string-length 3)

(define (short-string-constant? l)
  (and (is-type? 'flic-const l)
       (let ((string  (flic-const-value l)))
	 (and (string? string)
	      (<= (the fixnum (string-length string))
		  (the fixnum max-short-string-length))))))

(define (expand-string-constant l)
  (let* ((string  (flic-const-value l))
	 (length  (string-length string)))
    (expand-string-constant-aux string 0 length)))

(define (expand-string-constant-aux string i length)
  (declare (type fixnum i length))
  (if (eqv? i length)
      (make-flic-pack (core-symbol "Nil"))
      (make-flic-app
        (make-flic-pack (core-symbol ":"))
	(list (make-flic-const (string-ref string i))
	      (expand-string-constant-aux string (+ 1 i) length))
	'#f)))


;;; Helper function for the case of expanding foldr applied to cons call.

(define (do-foldr-cons-identity c z x xs)
  (make-flic-app
    c
    (list x
	  (optimize-foldr
	    (make-flic-ref (core-symbol "foldr"))
	    (list (copy-flic-top c) z xs)))
    '#f))



;;; Short-circuit build inlining for the usual case where the
;;; argument is a lambda.  (It would take several optimizer passes
;;; for this simplification to fall out, otherwise.)

(define (optimize-build fn args)
  (let ((arg  (car args)))
    (cond ((not (eqv? (dynamic *current-optimize-iteration*)
		      (dynamic *optimize-build-iteration*)))
	   (values fn args))
	  ((is-type? 'flic-lambda arg)
	   (record-hack 'build-inline-lambda)
	   (do-lambda-to-let-aux
	     arg
	     (cons (make-flic-pack (core-symbol ":"))
		   (cons (make-flic-pack (core-symbol "Nil"))
			 (cdr args)))))
	  (else
	   (record-hack 'build-inline-other)
	   (let ((new-fn
		   (copy-flic-top (var-value (core-symbol "inlineBuild")))))
	     (if (is-type? 'flic-lambda new-fn)
		 (do-lambda-to-let-aux new-fn args)
		 (values new-fn args))))
	  )))


;;; Various simplifications on numeric functions.
;;; *** Obviously, could get much fancier about this.
		  
(define (optimize-integer-to-int fn args)
  (let ((arg  (car args)))
    (cond ((is-type? 'flic-const arg)
	   (record-hack 'integer-to-int-constant-fold)
	   (if (is-type? 'integer (flic-const-value arg))
	       (let ((value  (flic-const-value arg)))
		 (when (not (is-type? 'fixnum value))
		   ;; Overflow is a user error, not an implementation error.
		   (phase-error 'int-overflow
				"Int overflow in primIntegerToInt: ~s"
				value))
		 (values arg (cdr args)))
	       (error "Bad argument ~s to primIntegerToInt." arg)))
	  ((and (is-type? 'flic-app arg)
		(is-type? 'flic-ref (flic-app-fn arg))
		(eq? (flic-ref-var (flic-app-fn arg))
		     (core-symbol "primIntToInteger"))
		(null? (cdr (flic-app-args arg))))
	   (record-hack 'integer-to-int-identity)
	   (values (car (flic-app-args arg)) (cdr args)))
	  (else
	   (values fn args)))))

(define (optimize-int-to-integer fn args)
  (let ((arg  (car args)))
    (cond ((is-type? 'flic-const arg)
	   (record-hack 'int-to-integer-constant-fold)
	   (if (is-type? 'integer (flic-const-value arg))
	       (values arg (cdr args))
	       (error "Bad argument ~s to primIntToInteger." arg)))
	  ((and (is-type? 'flic-app arg)
		(is-type? 'flic-ref (flic-app-fn arg))
		(eq? (flic-ref-var (flic-app-fn arg))
		     (core-symbol "primIntegerToInt"))
		(null? (cdr (flic-app-args arg))))
	   (record-hack 'int-to-integer-identity)
	   (values (car (flic-app-args arg)) (cdr args)))
	  (else
	   (values fn args)))))

(predefine (prim.rational-to-float-aux n d))   ; in prims.scm
(predefine (prim.rational-to-double-aux n d))  ; in prims.scm

(define (optimize-rational-to-float fn args)
  (let ((arg  (car args)))
    (cond ((is-type? 'flic-const arg)
	   (record-hack 'rational-to-float-constant-fold)
	   (if (is-type? 'list (flic-const-value arg))
	       (let ((value  (flic-const-value arg)))
		 (setf (flic-const-value arg)
		       (prim.rational-to-float-aux (car value) (cadr value)))
		 (values arg (cdr args)))
	       (error "Bad argument ~s to primRationalToFloat." arg)))
	  (else
	   (values fn args)))))

(define (optimize-rational-to-double fn args)
  (let ((arg  (car args)))
    (cond ((is-type? 'flic-const arg)
	   (record-hack 'rational-to-double-constant-fold)
	   (if (is-type? 'list (flic-const-value arg))
	       (let ((value  (flic-const-value arg)))
		 (setf (flic-const-value arg)
		       (prim.rational-to-double-aux (car value) (cadr value)))
		 (values arg (cdr args)))
	       (error "Bad argument ~s to primRationalToDouble." arg)))
	  (else
	   (values fn args)))))

(define (optimize-neg fn args)
  (let ((arg  (car args)))
    (cond ((is-type? 'flic-const arg)
	   (record-hack 'neg-constant-fold)
	   (if (is-type? 'number (flic-const-value arg))
	       (begin
		 (setf (flic-const-value arg) (- (flic-const-value arg)))
		 (values arg (cdr args)))
	       (error "Bad argument ~s to ~s." arg fn)))
	  (else
	   (values fn args)))))



;;; Convert lambda applications to lets.
;;; If application is not saturated, break it up into two nested
;;; lambdas before doing the transformation.
;;; It's better to do this optimization immediately than hoping
;;; the call will become fully saturated on the next pass.
;;; Maybe we could also look for a flic-let with a flic-lambda as
;;; the body to catch the cases where additional arguments can
;;; be found on a later pass.

(define (do-lambda-to-let new-fn new-args)
  (multiple-value-bind (fn args)
      (do-lambda-to-let-aux new-fn new-args)
    (maybe-make-app fn args)))

(define (maybe-make-app fn args)
  (if (null? args)
      fn
      (make-flic-app fn args '#f)))

(define (do-lambda-to-let-aux new-fn new-args)
  (let ((vars     (flic-lambda-vars new-fn))
	(body     (flic-lambda-body new-fn))
	(matched  '()))
    (record-hack 'app-lambda-to-let)
    (do ()
	((or (null? new-args) (null? vars)))
	(let ((var  (pop vars))
	      (arg  (pop new-args)))
	  (setf (var-value var) arg)
	  (setf (var-simple? var) (can-inline? arg '() '#t))
	  (if (eqv? (var-referenced var) 1)
	      (setf (var-single-ref var) (dynamic *lambda-depth*)))
	  (push var matched)))
    (setf matched (nreverse matched))
    (if (not (null? vars))
	(setf body (make-flic-lambda vars body)))
    (setf new-fn (make-flic-let matched body '#f))
    (values new-fn new-args)))


;;; For references, check to see if we can beta-reduce.
;;; Don't increment reference count for inlineable vars, but do
;;; traverse the new value expression.

(define-optimize flic-ref (object)
  (optimize-flic-ref-aux object))

(define (optimize-flic-ref-aux object)
  (let ((var     (flic-ref-var object)))
    (cond ((var-single-ref var)
	   ;; (or (eqv? (var-single-ref var) (dynamic *lambda-depth*)))
	   ;; *** The lambda-depth test is too conservative to handle
	   ;; *** inlining of stuff necessary for foldr/build optimizations.
	   ;; Can substitute value no matter how hairy it is.
	   ;; Note that this is potentially risky; if the single
	   ;; reference detected on the previous pass appeared as 
	   ;; the value of a variable binding that is being inlined
	   ;; on the current pass, it might turn into multiple
	   ;; references again!
	   ;; We copy the value anyway to avoid problems with shared
	   ;; structure in the multiple reference case.
	   (record-hack 'ref-inline-single-ref var)
	   (optimize (copy-flic-top (var-value var))))
	  ((and (var-inline-value var) (dynamic *do-inline-optimizations*))
	   ;; Use the previously saved value in preference to the current
	   ;; value of the variable.
	   (record-hack 'ref-inline-foldr-hack)
	   (optimize (copy-flic-top (var-inline-value var))))
	  ((and (var-simple? var)
		(or (dynamic *do-inline-optimizations*)
		    (not (var-toplevel? var))))
	   ;; Can substitute, but must copy.
	   (record-hack 'ref-inline var)
	   (optimize (copy-flic-top (var-value var))))
	  ((eq? var (core-symbol "foldr"))
	   ;; Magic stuff for deforestation
	   (if (> (the fixnum (dynamic *current-optimize-iteration*))
		  (the fixnum (dynamic *optimize-foldr-iteration*)))
	       (begin
		 (record-hack 'ref-inline-foldr)
		 (optimize (make-flic-ref (core-symbol "inlineFoldr"))))
	       object))
	  ((eq? var (core-symbol "build"))
	   ;; Magic stuff for deforestation
	   (if (> (the fixnum (dynamic *current-optimize-iteration*))
		  (the fixnum (dynamic *optimize-build-iteration*)))
	       (begin
		 (record-hack 'ref-inline-build)
		 (optimize (make-flic-ref (core-symbol "inlineBuild"))))
	       object))
	  ((var-toplevel? var)
	   object)
	  (else
	   (incf (var-referenced var))
	   object))))


;;; Don't do anything exciting with constants.

(define-optimize flic-const (object)
  object)

(define-optimize flic-pack (object)
  object)



;;; Various simplifications on and

(define-optimize flic-and (object)
  (maybe-simplify-and
    object
    (optimize-and-exps (flic-and-exps object) '())))

(define (maybe-simplify-and object exps)
  (cond ((null? exps)
	 (record-hack 'and-empty)
	 (make-flic-pack (core-symbol "True")))
	((null? (cdr exps))
	 (record-hack 'and-unary)
	 (car exps))
	(else
	 (setf (flic-and-exps object) exps)
	 object)))

(define (optimize-and-exps exps result)
  (if (null? exps)
      (nreverse result)
      (let ((exp  (optimize (car exps))))
	(typecase exp
	  (flic-pack
	    (cond ((eq? (flic-pack-con exp) (core-symbol "True"))
		   ;; True appears in subexpressions.
		   ;; Discard this test only.
		   (record-hack 'and-contains-true)
		   (optimize-and-exps (cdr exps) result))
		  ((eq? (flic-pack-con exp) (core-symbol "False"))
		   ;; False appears in subexpressions.
		   ;; Discard remaining tests as dead code.
		   ;; Can't replace the whole and expression with false because
		   ;; of possible strictness side-effects.
		   (record-hack 'and-contains-false)
		   (nreverse (cons exp result)))
		  (else
		   ;; Should never happen.
		   (error "Non-boolean con ~s in and expression!" exp))))
	  (flic-and
	   ;; Flatten nested ands.
	   (record-hack 'and-compress)
	   (optimize-and-exps
	    (cdr exps)
	    (nconc (nreverse (flic-and-exps exp)) result)))
	  (else
	   ;; No optimization possible.
	   (optimize-and-exps (cdr exps) (cons exp result)))
	  ))))


;;; Case-block optimizations.  These optimizations are possible because
;;; of the restricted way this construct is used;  return-froms are
;;; never nested, etc.

(define-optimize flic-case-block (object)
  (let* ((sym  (flic-case-block-block-name object))
	 (exps (optimize-case-block-exps
		 sym (flic-case-block-exps object) '())))
    (optimize-flic-case-block-aux object sym exps)))

(define (optimize-flic-case-block-aux object sym exps)
  (cond ((null? exps)
	 ;; This should never happen.  It means all of the tests were
	 ;; optimized away, including the failure case!
	 (error "No exps left in case block ~s!" object))
	((and (is-type? 'flic-and (car exps))
	      (is-return-from-block?
	        sym
	        (car (last (flic-and-exps (car exps))))))
	 ;; The first clause is a simple and.  Hoist it out of the
	 ;; case-block and rewrite as if/then/else.
	 (record-hack 'case-block-to-if)
	 (let ((then-exp  (car (last (flic-and-exps (car exps))))))
	   (setf (flic-case-block-exps object) (cdr exps))
	   (make-flic-if
	     (maybe-simplify-and
	       (car exps)
	       (butlast (flic-and-exps (car exps))))
	     (flic-return-from-exp then-exp)
	     (optimize-flic-case-block-aux object sym (cdr exps)))))
	((is-return-from-block? sym (car exps))
	 ;; Do an identity reduction.
	 (record-hack 'case-block-identity)
	 (flic-return-from-exp (car exps)))
	((is-type? 'flic-let (car exps))
	 ;; The first clause is a let.  Since this clause is going
	 ;; to be executed anyway, hoisting the bindings to surround
	 ;; the entire case-block should not change their strictness
	 ;; properties, and it may permit some further optimizations.
	 (record-hack 'case-block-hoist-let)
	 (let* ((exp  (car exps))
		(body (flic-let-body exp)))
	   (setf (flic-let-body exp)
		 (optimize-flic-case-block-aux
		   object sym (cons body (cdr exps))))
	   exp))
	(else
	 (setf (flic-case-block-exps object) exps)
	 object)
	))


(define (optimize-case-block-exps sym exps result)
  (if (null? exps)
      (nreverse result)
      (let ((exp  (optimize (car exps))))
	(cond ((is-return-from-block? sym exp)
	       ;; Any remaining clauses are dead code and should be removed.
	       (if (not (null? (cdr exps)))
		   (record-hack 'case-block-dead-code))
	       (nreverse (cons exp result)))
	      ((is-type? 'flic-and exp)
	       ;; See if we can remove redundant tests.
	       (push (maybe-simplify-and
		       exp
		       (look-for-redundant-tests (flic-and-exps exp) result))
		     result)
	       (optimize-case-block-exps sym (cdr exps) result))
	      (else
	       ;; No optimization possible.
	       (optimize-case-block-exps sym (cdr exps) (cons exp result)))
	      ))))


;;; Look for case-block tests that are known to be either true or false
;;; because of tests made in previous clauses.
;;; For now, we only look at is-constructor tests.  Such a test is known
;;; to be true if previous clauses have eliminated all other possible
;;; constructors.  And such a test is known to be false if a previous
;;; clause has already matched this constructor.

(define (look-for-redundant-tests exps previous-clauses)
  (if (null? exps)
      '()
      (let ((exp  (car exps)))
	(cond ((and (is-type? 'flic-is-constructor exp)
		    (constructor-test-redundant? exp previous-clauses))
	       ;; Known to be true.
	       (record-hack 'case-block-discard-redundant-test)
	       (cons (make-flic-pack (core-symbol "True"))
		     (look-for-redundant-tests (cdr exps) previous-clauses)))

              ((and (is-type? 'flic-is-constructor exp)
		    (constructor-test-duplicated? exp previous-clauses))
	       ;; Known to be false.
	       (record-hack 'case-block-discard-duplicate-test)
	       (list (make-flic-pack (core-symbol "False"))))
	      (else
	       ;; No optimization.
	       (cons exp
		     (look-for-redundant-tests (cdr exps) previous-clauses)))
	      ))))


;;; In looking for redundant/duplicated tests, only worry about
;;; is-constructor tests that have an argument that is a variable.
;;; It's too hairy to consider any other cases.

(define (constructor-test-duplicated? exp previous-clauses)
  (let ((con  (flic-is-constructor-con exp))
	(arg  (flic-is-constructor-exp exp)))
    (and (is-type? 'flic-ref arg)
	 (constructor-test-present? con arg previous-clauses))))

(define (constructor-test-redundant? exp previous-clauses)
  (let ((con     (flic-is-constructor-con exp))
        (arg     (flic-is-constructor-exp exp)))
    (and (is-type? 'flic-ref arg)
	 (every-1 (lambda (c)
		    (or (eq? c con)
			(constructor-test-present? c arg previous-clauses)))
		  (algdata-constrs (con-alg con))))))

(define (constructor-test-present? con arg previous-clauses)
  (cond ((null? previous-clauses)
	 '#f)
	((constructor-test-present-1? con arg (car previous-clauses))
	 '#t)
	(else
	 (constructor-test-present? con arg (cdr previous-clauses)))))


;;; The tricky thing here is that, even if the constructor test is 
;;; present in the clause, we have to make sure that the entire clause won't
;;; fail due to the presence of some other test which fails.  So look
;;; for a very specific pattern here, namely
;;;  (and (is-constructor con arg) (return-from ....))

(define (constructor-test-present-1? con arg clause)
  (and (is-type? 'flic-and clause)
       (let ((exps  (flic-and-exps clause)))
	 (and (is-type? 'flic-is-constructor (car exps))
	      (is-type? 'flic-return-from (cadr exps))
	      (null? (cddr exps))
	      (let* ((inner-exp  (car exps))
		     (inner-con  (flic-is-constructor-con inner-exp))
		     (inner-arg  (flic-is-constructor-exp inner-exp)))
		(and (eq? inner-con con)
		     (flic-exp-eq? arg inner-arg)))))))



;;; No fancy optimizations for return-from by itself.

(define-optimize flic-return-from (object)
  (setf (flic-return-from-exp object)
	(optimize (flic-return-from-exp object)))
  object)



;;; Obvious simplification on if

(define-optimize flic-if (object)
  (let ((test-exp  (optimize (flic-if-test-exp object)))
	(then-exp  (optimize (flic-if-then-exp object)))
	(else-exp  (optimize (flic-if-else-exp object))))
    (cond ((and (is-type? 'flic-pack test-exp)
		(eq? (flic-pack-con test-exp) (core-symbol "True")))
	   ;; Fold constant test
	   (record-hack 'if-fold)
	   then-exp)
	  ((and (is-type? 'flic-pack test-exp)
		(eq? (flic-pack-con test-exp) (core-symbol "False")))
	   ;; Fold constant test
	   (record-hack 'if-fold)
	   else-exp)
	  ((and (is-type? 'flic-is-constructor test-exp)
		(eq? (flic-is-constructor-con test-exp) (core-symbol "True")))
	   ;; Remove redundant is-constructor test.
	   ;; Doing this as a general is-constructor identity
	   ;; backfires because it prevents some of the important case-block
	   ;; optimizations from being recognized, but it works fine here.
	   (record-hack 'if-compress-test)
	   (setf (flic-if-test-exp object) (flic-is-constructor-exp test-exp))
	   (setf (flic-if-then-exp object) then-exp)
	   (setf (flic-if-else-exp object) else-exp)
	   object)
	  ((and (is-type? 'flic-is-constructor test-exp)
		(eq? (flic-is-constructor-con test-exp) (core-symbol "False")))
	   ;; Remove redundant is-constructor test, flip branches.
	   (record-hack 'if-compress-test)
	   (setf (flic-if-test-exp object) (flic-is-constructor-exp test-exp))
	   (setf (flic-if-then-exp object) else-exp)
	   (setf (flic-if-else-exp object) then-exp)
	   object)
	  ((and (is-type? 'flic-return-from then-exp)
		(is-type? 'flic-return-from else-exp)
		(eq? (flic-return-from-block-name then-exp)
		     (flic-return-from-block-name else-exp)))
	   ;; Hoist return-from outside of IF.
	   ;; This may permit further case-block optimizations.
	   (record-hack 'if-hoist-return-from)
	   (let ((return-exp  then-exp))
	     (setf (flic-if-test-exp object) test-exp)
	     (setf (flic-if-then-exp object) (flic-return-from-exp then-exp))
	     (setf (flic-if-else-exp object) (flic-return-from-exp else-exp))
	     (setf (flic-return-from-exp return-exp) object)
	     return-exp))
	  ((and (is-type? 'flic-pack then-exp)
		(is-type? 'flic-pack else-exp)
		(eq? (flic-pack-con then-exp) (core-symbol "True"))
		(eq? (flic-pack-con else-exp) (core-symbol "False")))
	   ;; This if does nothing useful at all!
	   (record-hack 'if-identity)
	   test-exp)
	  ((and (is-type? 'flic-pack then-exp)
		(is-type? 'flic-pack else-exp)
		(eq? (flic-pack-con then-exp) (core-symbol "False"))
		(eq? (flic-pack-con else-exp) (core-symbol "True")))
	   ;; Inverse of previous case
	   (record-hack 'if-identity-inverse)
	   (make-flic-is-constructor (core-symbol "False") test-exp))
	  ((or (is-type? 'flic-lambda then-exp)
	       (is-type? 'flic-lambda else-exp))
	   ;; Hoist lambdas to surround entire if.  This allows us to
	   ;; do a better job of saturating them.
	   (record-hack 'if-hoist-lambda)
	   (multiple-value-bind (vars then-exp else-exp)
	       (do-if-hoist-lambda then-exp else-exp)
	     (setf (flic-if-test-exp object) test-exp)
	     (setf (flic-if-then-exp object) then-exp)
	     (setf (flic-if-else-exp object) else-exp)
	     (make-flic-lambda vars object)))
	  (else
	   ;; No optimization possible
	   (setf (flic-if-test-exp object) test-exp)
	   (setf (flic-if-then-exp object) then-exp)
	   (setf (flic-if-else-exp object) else-exp)
	   object)
	  )))



;;; Try to pull as many variables as possible out to surround the entire
;;; let.

(define (do-if-hoist-lambda then-exp else-exp)
  (let ((vars       '())
	(then-args  '())
	(else-args  '()))
    (do ((then-vars  (if (is-type? 'flic-lambda then-exp)
			 (flic-lambda-vars then-exp)
			 '())
		     (cdr then-vars))
	 (else-vars  (if (is-type? 'flic-lambda else-exp)
			 (flic-lambda-vars else-exp)
			 '())
		     (cdr else-vars)))
	((and (null? then-vars) (null? else-vars)) '#f)
	(let ((var  (init-flic-var (create-temp-var 'arg) '#f '#f)))
	  (push var vars)
	  (push (make-flic-ref var) then-args)
	  (push (make-flic-ref var) else-args)))
    (values
      vars
      (if (is-type? 'flic-lambda then-exp)
	  (do-lambda-to-let then-exp then-args)
	  (make-flic-app then-exp then-args '#f))
      (if (is-type? 'flic-lambda else-exp)
	  (do-lambda-to-let else-exp else-args)
	  (make-flic-app else-exp else-args '#f)))))

    

;;; Look for (sel (pack x)) => x

(define-optimize flic-sel (object)
  (optimize-flic-sel-aux object))

(define (optimize-flic-sel-aux object)
  (let ((new-exp  (optimize (flic-sel-exp object))))
    (setf (flic-sel-exp object) new-exp)
    (typecase new-exp
      (flic-ref
       ;; Check to see whether this is bound to a pack application
       (let ((val  (is-bound-to-constructor-app? (flic-ref-var new-exp))))
	 (if val
	     ;; Yup, it is.  Now extract the appropriate component,
	     ;; provided it is inlineable.
	     (let* ((i      (flic-sel-i object))
		    (args   (flic-app-args val))
		    (newval (list-ref args i)))
	       (if (can-inline? newval '() '#t)
		   (begin
		     (record-hack 'sel-fold-var)
		     (optimize (copy-flic-top newval)))
		   object))
	     ;; The variable was bound to something else.
	     object)))
      (flic-app
       ;; The obvious optimization.
       (if (is-constructor-app-prim? new-exp)
	   (begin
	     (record-hack 'sel-fold-app)
	     (list-ref (flic-app-args new-exp) (flic-sel-i object)))
	   object))
      (else
       object))))




;;; Do similar stuff for is-constructor.

(define-optimize flic-is-constructor (object)
  (let ((con      (flic-is-constructor-con object))
	(exp      (optimize (flic-is-constructor-exp object)))
	(exp-con  '#f))
    (cond ((algdata-tuple? (con-alg con))
	   ;; Tuples have only one constructor, so this is always true
	   (record-hack 'is-constructor-fold-tuple)
	   (make-flic-pack (core-symbol "True")))
	  ((setf exp-con (is-constructor-app? exp))
	   ;; The expression is a constructor application.
	   (record-hack 'is-constructor-fold)
	   (make-flic-pack
	     (if (eq? exp-con con)
		 (core-symbol "True")
		 (core-symbol "False"))))
	  (else
	   ;; No optimization possible
	   (setf (flic-is-constructor-exp object) exp)
	   object)
	  )))


(define-optimize flic-con-number (object)
  (let ((exp  (flic-con-number-exp object))
	(type (flic-con-number-type object)))
    ;; ***Maybe ast-to-flic should look for this one.
    (if (algdata-tuple? type)
	(begin
	  (record-hack 'con-number-fold-tuple)
	  (make-flic-const 0))
	(let* ((new-exp  (optimize exp))
	       (con      (is-constructor-app? new-exp)))
	  (if con
	      (begin
	        (record-hack 'con-number-fold)
		(make-flic-const (con-tag con)))
	      (begin
	        (setf (flic-con-number-exp object) new-exp)
		object)))
      )))

(define-optimize flic-void (object)
  object)


;;;===================================================================
;;; General helper functions
;;;===================================================================


;;; Lucid's built-in every function seems to do a lot of unnecessary
;;; consing.  This one is much faster.

(define (every-1 fn list)
  (cond ((null? list)
	 '#t)
	((funcall fn (car list))
	 (every-1 fn (cdr list)))
	(else
	 '#f)))



;;; Equality predicate on flic expressions

(define (flic-exp-eq? a1 a2)
  (typecase a1
    (flic-const
     (and (is-type? 'flic-const a2)
	  (equal? (flic-const-value a1) (flic-const-value a2))))
    (flic-ref
     (and (is-type? 'flic-ref a2)
	  (eq? (flic-ref-var a1) (flic-ref-var a2))))
    (flic-pack
     (and (is-type? 'flic-pack a2)
	  (eq? (flic-pack-con a1) (flic-pack-con a2))))
    (flic-sel
     (and (is-type? 'flic-sel a2)
	  (eq? (flic-sel-con a1) (flic-sel-con a2))
	  (eqv? (flic-sel-i a1) (flic-sel-i a2))
	  (flic-exp-eq? (flic-sel-exp a1) (flic-sel-exp a2))))
    (else
     '#f)))



;;; Predicates for testing whether an expression matches a pattern.

(define (is-constructor-app? exp)
  (typecase exp
    (flic-app
     ;; See if we have a saturated call to a constructor.
     (is-constructor-app-prim? exp))
    (flic-ref
     ;; See if we can determine anything about the value the variable
     ;; is bound to.
     (let ((value  (var-value (flic-ref-var exp))))
       (if value
	   (is-constructor-app? value)
	   '#f)))
    (flic-let
     ;; See if we can determine anything about the body of the let.
     (is-constructor-app? (flic-let-body exp)))
    (flic-pack
     ;; See if this is a nullary constructor.
     (let ((con  (flic-pack-con exp)))
       (if (eqv? (con-arity con) 0)
	   con
	   '#f)))
    (else
     '#f)))

(define (is-return-from-block? sym exp)
  (and (is-type? 'flic-return-from exp)
       (eq? (flic-return-from-block-name exp) sym)))

(define (is-constructor-app-prim? exp)
  (let ((fn    (flic-app-fn exp))
	(args  (flic-app-args exp)))
    (if (and (is-type? 'flic-pack fn)
	     (eqv? (length args) (con-arity (flic-pack-con fn))))
	(flic-pack-con fn)
	'#f)))

(define (is-bound-to-constructor-app? var)
  (let ((val  (var-value var)))
    (if (and val
	     (is-type? 'flic-app val)
	     (is-constructor-app-prim? val))
	val
	'#f)))

(define (is-selector? con i exp)
  (or (and (is-type? 'flic-ref exp)
	   (is-selector? con i (var-value (flic-ref-var exp))))
      (and (is-type? 'flic-sel exp)
	   (eq? (flic-sel-con exp) con)
	   (eqv? (the fixnum i) (the fixnum (flic-sel-i exp)))
	   (flic-sel-exp exp))
      ))

(define (is-selector-list? con i subexp exps)
  (declare (type fixnum i))
  (if (null? exps)
      subexp
      (let ((temp  (is-selector? con i (car exps))))
	(and (flic-exp-eq? subexp temp)
	     (is-selector-list? con (+ 1 i) subexp (cdr exps))))))



;;;===================================================================
;;; Inlining criteria
;;;===================================================================

;;; Expressions that can be inlined unconditionally are constants, variable
;;; references, and some functions.
;;; I've made some attempt here to arrange the cases in the order they
;;; are likely to occur.

(define (can-inline? exp recursive-vars toplevel?)
  (typecase exp
    (flic-sel
     ;; Listed first because it happens more frequently than
     ;; anything else.
     ;; *** Inlining these is an experiment.
     ;; *** This transformation interacts with the strictness
     ;; *** analyzer; if the variable referenced is not strict, then
     ;; *** it is probably not a good thing to do since it adds extra
     ;; *** forces.
     ;; (let ((subexp  (flic-sel-exp exp)))
     ;;   (and (is-type? 'flic-ref subexp)
     ;;        (not (memq (flic-ref-var subexp) recursive-vars))))
     '#f)
    (flic-lambda
     ;; Do not try to inline lambdas if the fancy inline optimization
     ;; is disabled.
     ;; Watch for problems with infinite loops with recursive variables.
     (if (dynamic *do-inline-optimizations*)
	 (simple-function-body? (flic-lambda-body exp)
				(flic-lambda-vars exp)
				recursive-vars
				toplevel?)
	 '#f))
    (flic-ref
     ;; We get into infinite loops trying to inline recursive variables.
     (not (memq (flic-ref-var exp) recursive-vars)))
    ((or flic-pack flic-const)
     '#t)
    (else
     '#f)))


;;; Determining whether to inline a function is difficult.  This is
;;; very conservative to avoid code bloat.  What we need to do is
;;; compare the cost (in program size mainly) of the inline call with
;;; an out of line call.  For an out of line call, we pay for one function
;;; call and a setup for each arg.  When inlining, we pay for function
;;; calls in the body and for args referenced more than once.  In terms of
;;; execution time, we win big when a functional parameter is called
;;; since this `firstifies' the program.

;;; Here's the criteria:
;;;  An inline function gets to reference no more that 2 non-parameter
;;;  values (including constants and repeated parameter references).
;;; For non-toplevel functions, be slightly more generous since the
;;; fixed overhead of binding the local function would go away.

(define (simple-function-body? exp lambda-vars recursive-vars toplevel?)
  (let ((c  (if toplevel? 2 4)))
    (>= (the fixnum (simple-function-body-1 exp lambda-vars recursive-vars c))
	0)))


;;; I've made some attempt here to order the cases by how frequently
;;; they appear.

(define (simple-function-body-1 exp lambda-vars recursive-vars c)
  (declare (type fixnum c))
  (if (< c 0)
      (values c '())
      (typecase exp
	(flic-ref
	 (let ((var (flic-ref-var exp)))
	   (cond ((memq var lambda-vars)
		  (values c (list-remove-1 var lambda-vars)))
		 ((memq var recursive-vars)
		  (values -1 '()))
		 (else
		  (values (the fixnum (1- c)) lambda-vars)))))
	(flic-app
	 (simple-function-body-1/l
	   (cons (flic-app-fn exp) (flic-app-args exp))
	   lambda-vars recursive-vars c))
	(flic-sel
	 (simple-function-body-1
	  (flic-sel-exp exp)
	  lambda-vars recursive-vars (the fixnum (1- c))))
	(flic-is-constructor
	 (simple-function-body-1
	  (flic-is-constructor-exp exp)
	  lambda-vars recursive-vars (the fixnum (1- c))))
	((or flic-const flic-pack)
	 (values (the fixnum (1- c)) lambda-vars))
	(else
         ;; case & let & lambda not allowed.
	 (values -1 '())))))

(define (list-remove-1 item list)
  (cond ((null? list)
	 '())
	((eq? item (car list))
	 (cdr list))
	(else
	 (cons (car list) (list-remove-1 item (cdr list))))
	))

(define (simple-function-body-1/l exps lambda-vars recursive-vars c)
  (declare (type fixnum c))
  (if (or (null? exps) (< c 0))
      (values c lambda-vars)
      (multiple-value-bind (c-1 lambda-vars-1)
	  (simple-function-body-1 (car exps) lambda-vars recursive-vars c)
	(simple-function-body-1/l
	  (cdr exps) lambda-vars-1 recursive-vars c-1))))



;;;===================================================================
;;; Constant structured data detection
;;;===================================================================


;;; Look to determine whether an object is a structured constant,
;;; recursively examining its components if it's an app.  This is
;;; necessary in order to detect constants with arbitrary circular
;;; reference to the vars in recursive-vars.

(define (structured-constant-recursive? object recursive-vars stack)
  (typecase object
    (flic-const
     '#t)
    (flic-ref
     (let ((var  (flic-ref-var object)))
       (or (memq var stack)
	   (var-toplevel? var)
	   (and (memq var recursive-vars)
		(structured-constant-recursive?
		 (var-value var) recursive-vars (cons var stack))))))
    (flic-pack
     '#t)
    (flic-app
     (structured-constant-app-recursive?
       (flic-app-fn object)
       (flic-app-args object)
       recursive-vars
       stack))
    (flic-lambda
     (lambda-hoistable? object))
    (else
     '#f)))

(define (structured-constant-app-recursive? fn args recursive-vars stack)
  (and (is-type? 'flic-pack fn)
       (eqv? (length args) (con-arity (flic-pack-con fn)))
       (every-1 (lambda (a)
		  (structured-constant-recursive? a recursive-vars stack))
		args)))


;;; Here's a non-recursive (and more efficient) version of the above.
;;; Instead of looking at the whole structure, it only looks one level
;;; deep.  This can't detect circular constants, but is useful in
;;; contexts where circularities cannot appear.

(define (structured-constant? object)
  (typecase object
    (flic-ref
     (var-toplevel? (flic-ref-var object)))
    (flic-const
     '#t)
    (flic-pack
     '#t)
    (flic-lambda
     (lambda-hoistable? object))
    (else
     '#f)))

(define (structured-constant-app? fn args)
  (and (is-type? 'flic-pack fn)
       (eqv? (length args) (con-arity (flic-pack-con fn)))
       (every-1 (function structured-constant?) args)))


;;; Determine whether a lambda can be hoisted to top-level.
;;; The main purpose of this code is to mark structured constants
;;; containing simple lambdas to permit later folding of sel expressions 
;;; on those constants.  Since the latter expression is permissible
;;; only on inlinable functions, stop if we hit an expression that
;;; would make the function not inlinable.

(define (lambda-hoistable? object)
  (and (can-inline? object '() '#t)
       (lambda-hoistable-aux
	 (flic-lambda-body object)
	 (flic-lambda-vars object))))

(define (lambda-hoistable-aux object local-vars)
  (typecase object
    (flic-ref
     (or (var-toplevel? (flic-ref-var object))
	 (memq (flic-ref-var object) local-vars)))
    ((or flic-const flic-pack)
     '#t)
    (flic-sel
     (lambda-hoistable-aux (flic-sel-exp object) local-vars))
    (flic-is-constructor
     (lambda-hoistable-aux (flic-is-constructor-exp object) local-vars))
    (flic-app
     (and (lambda-hoistable-aux (flic-app-fn object) local-vars)
	  (every-1 (lambda (x) (lambda-hoistable-aux x local-vars))
		   (flic-app-args object))))
    (else
     '#f)))


;;; Having determined that something is a structured constant,
;;; enter it (and possibly its subcomponents) in the hash table
;;; and return a var-ref.

(define (enter-structured-constant value recursive?)
  (multiple-value-bind (con args var)
      (enter-structured-constant-aux value recursive?)
    (when (not var)
      (setf var (create-temp-var 'constant))
      (add-new-structured-constant var con args))
    (make-flic-ref var)))

(define (enter-structured-constant-aux value recursive?)
  (let* ((fn   (flic-app-fn value))
	 (con  (flic-pack-con fn))
	 (args (if recursive?
		   (map (function enter-structured-constant-arg)
			(flic-app-args value))
		   (flic-app-args value))))
    (values con args (lookup-structured-constant con args))))

(define (enter-structured-constant-arg a)
  (if (is-type? 'flic-app a)
      (enter-structured-constant a '#t)
      a))

(define (lookup-structured-constant con args)
  (lookup-structured-constant-aux
    (table-entry *structured-constants-table* con) args))

(define (lookup-structured-constant-aux alist args)
  (cond ((null? alist)
	 '#f)
	((every (function flic-exp-eq?) (car (car alist)) args)
	 (cdr (car alist)))
	(else
	 (lookup-structured-constant-aux (cdr alist) args))))

(define (add-new-structured-constant var con args)
  (push (cons args var) (table-entry *structured-constants-table* con))
  (setf (var-toplevel? var) '#t)
  (setf (var-value var) (make-flic-app (make-flic-pack con) args '#t))
  (push var *structured-constants*)
  var)



;;;===================================================================
;;; Invariant argument stuff
;;;===================================================================


;;; When processing a saturated call to a locally defined function,
;;; note whether any of the arguments are always passed the same value.

(define (note-invariant-args args vars)
  (when (and (not (null? args)) (not (null? vars)))
    (let* ((arg  (car args))
	   (var  (car vars))
	   (val  (var-arg-invariant-value var)))
      (cond ((not (var-arg-invariant? var))
	     ;; This argument already marked as having more than one
	     ;; value.
	     )
	    ((and (is-type? 'flic-ref arg)
		  (eq? (flic-ref-var arg) var))
	     ;; This is a recursive call with the same argument.
	     ;; Don't update the arg-invariant-value slot.
	     )
	    ((or (not val)
		 (flic-exp-eq? arg val))
	     ;; Either this is the first call, or a second call with
	     ;; the same argument.
	     (setf (var-arg-invariant-value var) arg))
	    (else
	     ;; Different values for this argument are passed in
	     ;; different places, so we can't mess with it.
	     (setf (var-arg-invariant? var) '#f)))
      (note-invariant-args (cdr args) (cdr vars)))))


;;; After processing a let form, check to see if any of the bindings
;;; are for local functions with invariant arguments.
;;; Suppose we have something like
;;;   let foo = \ x y z -> <fn-body>
;;;     in <let-body>
;;; and y is known to be invariant; then we rewrite this as
;;;   let foo1 = \ x z -> let y = <invariant-value> in <fn-body>
;;;       foo = \ x1 y1 z1 -> foo1 x1 z1
;;;     in <let-body>
;;; The original foo binding is inlined on subsequent passes and 
;;; should go away.  Likewise, the binding of y should be inlined also.
;;; *** This is kind of bogus because of the way it depends on the
;;; *** magic force-inline bit.  It would be better to do a code walk
;;; *** now on the entire let expression to rewrite all the calls to foo.

(define (add-stuff-for-invariants bindings)
  (if (null? bindings)
      '()
      (let* ((var  (car bindings))
	     (val  (var-value var)))
	(setf (cdr bindings)
	      (add-stuff-for-invariants (cdr bindings)))
	(if (and (is-type? 'flic-lambda val)
		 ;; Don't mess with single-reference variable bindings,
		 ;; or things we are going to inline anyway.
		 (not (var-single-ref var))
		 (not (var-simple? var))
		 ;; All references must be in saturated calls to do this.
		 (eqv? (var-referenced var) (var-fn-referenced var))
		 ;; There is at least one argument marked invariant.
		 (some (function var-arg-invariant?) (flic-lambda-vars val))
		 ;; Every argument marked invariant must also be hoistable.
		 (every-1 (function arg-hoistable?) (flic-lambda-vars val)))
	    (hoist-invariant-args
	      var
	      val
	      bindings)
	    bindings))))

(define (arg-hoistable? var)
  (if (var-arg-invariant? var)
      (or (not (var-arg-invariant-value var))
	  (flic-invariant? (var-arg-invariant-value var)
			   (dynamic *local-bindings*)))
      '#t))

(define (hoist-invariant-args var val bindings)
  (let ((foo1-var       (copy-temp-var (def-name var)))
	(foo1-def-vars  '())
	(foo1-app-args  '())
	(foo1-let-vars  '())
	(foo-def-vars   '()))
    (push foo1-var bindings)
    (dolist (v (flic-lambda-vars val))
      (let ((new-v  (copy-temp-var (def-name v))))
	(push (init-flic-var new-v '#f '#f) foo-def-vars)
	(if (var-arg-invariant? v)
	    (when (var-arg-invariant-value v)
	      (push (init-flic-var
		      v (copy-flic-top (var-arg-invariant-value v)) '#f)
		    foo1-let-vars))
	    (begin
	      (push v foo1-def-vars)
	      (push (make-flic-ref new-v) foo1-app-args))
	  )))
    (setf foo1-def-vars (nreverse foo1-def-vars))
    (setf foo1-app-args (nreverse foo1-app-args))
    (setf foo1-let-vars (nreverse foo1-let-vars))
    (setf foo-def-vars (nreverse foo-def-vars))
    (record-hack 'let-hoist-invariant-args var foo1-let-vars)
    ;; Fix up the value of foo1
    (init-flic-var
      foo1-var
      (let ((body  (make-flic-let foo1-let-vars (flic-lambda-body val) '#f)))
	(if (null? foo1-def-vars)
	    ;; *All* of the arguments were invariant.
	    body
	    ;; Otherwise, make a new lambda
	    (make-flic-lambda foo1-def-vars body)))
      '#f)
    ;; Fix up the value of foo and arrange for it to be inlined.
    (setf (flic-lambda-vars val) foo-def-vars)
    (setf (flic-lambda-body val)
	  (if (null? foo1-app-args)
	      (make-flic-ref foo1-var)
	      (make-flic-app (make-flic-ref foo1-var) foo1-app-args '#t)))
    (setf (var-simple? var) '#t)
    (setf (var-force-inline? var) '#t)
    ;; Return modified list of bindings
    bindings))



;;;===================================================================
;;; Install globals
;;;===================================================================


;;; The optimizer, CFN, etc. can introduce new top-level variables that
;;; are not installed in the symbol table.  This causes problems if
;;; those variables are referenced in the .hci file (as in the inline
;;; expansion of some other variables).  So we need to fix up the 
;;; symbol table before continuing.

(define (install-uninterned-globals vars)
  (dolist (v vars)
    (let* ((module  (locate-module (def-module v)))
	   (name    (def-name v))
	   (table   (module-symbol-table module))
	   (def     (table-entry table name)))
      (cond ((not def)
	     ;; This def was not installed.  Rename it if it's a gensym
	     ;; and install it.
	     (when (gensym? name)
	       (setf name (rename-gensym-var v name table)))
	     (setf (table-entry table name) v))
	    ((eq? def v)
	     ;; Already installed.
	     '#t)
	    (else
	     ;; Ooops!  The symbol installed in the symbol table isn't 
             ;; this one!
	     (error "Duplicate defs ~s and ~s in symbol table for ~s!"
		    v def module))
	    ))))


(define (rename-gensym-var var name table)
  (setf name (string->symbol (symbol->string name)))
  (if (table-entry table name)
      ;; This name already in use; gensym a new one!
      (rename-gensym-var var (gensym (symbol->string name)) table)
      ;; OK, no problem
      (setf (def-name var) name)))



;;;===================================================================
;;; Postoptimizer
;;;===================================================================

;;; This is another quick traversal of the structure to determine 
;;; whether references to functions are fully saturated or not.
;;; Also makes sure that reference counts on variables are correct;
;;; this is needed so the code generator can generate ignore declarations
;;; for unused lambda variables.

(define-flic-walker postoptimize (object))

(define-postoptimize flic-lambda (object)
  (dolist (var (flic-lambda-vars object))
    (setf (var-referenced var) 0))
  (postoptimize (flic-lambda-body object)))

(define-postoptimize flic-let (object)
  (dolist (var (flic-let-bindings object))
    (setf (var-referenced var) 0)
    (let ((val  (var-value var)))
      (setf (var-arity var)
	    (if (is-type? 'flic-lambda val)
		(length (flic-lambda-vars val))
		0))))
  (dolist (var (flic-let-bindings object))
    (postoptimize (var-value var)))
  (postoptimize (flic-let-body object)))

(define-postoptimize flic-app (object)
  (let ((fn    (flic-app-fn object)))
    (typecase fn
      (flic-ref
       (let* ((var     (flic-ref-var fn))
	      (arity   (var-arity var)))
	 (if (not (var-toplevel? var)) (incf (var-referenced var)))
	 (when (not (eqv? arity 0))
	   (postoptimize-app-aux object var arity (flic-app-args object)))))
      (flic-pack
       (let* ((con    (flic-pack-con fn))
	      (arity  (con-arity con)))
	 (postoptimize-app-aux object '#f arity (flic-app-args object))))
      (else
       (postoptimize fn)))
    (dolist (a (flic-app-args object))
      (postoptimize a))))

(define (postoptimize-app-aux object var arity args)
  (declare (type fixnum arity))
  (let ((nargs   (length args)))
    (declare (type fixnum nargs))
    (cond ((< nargs arity)
	   ;; not enough arguments
	   (when var (setf (var-standard-refs? var) '#t)))
	  ((eqv? nargs arity)
	   ;; exactly the right number of arguments
	   (when var (setf (var-optimized-refs? var) '#t))
	   (setf (flic-app-saturated? object) '#t))
	  (else
	   ;; make the fn a nested flic-app
	   (multiple-value-bind (arghead argtail)
	       (split-list args arity)
	     (setf (flic-app-fn object)
		   (make-flic-app (flic-app-fn object) arghead '#t))
	     (setf (flic-app-args object) argtail)
	     (when var (setf (var-optimized-refs? var) '#t))
	     (dolist (a arghead)
	       (postoptimize a))))
	  )))

(define-postoptimize flic-ref (object)
  (let ((var  (flic-ref-var object)))
    (if (not (var-toplevel? var)) (incf (var-referenced var)))
    (setf (var-standard-refs? var) '#t)))

(define-postoptimize flic-const (object)
  object)

(define-postoptimize flic-pack (object)
  object)

(define-postoptimize flic-and (object)
  (for-each (function postoptimize) (flic-and-exps object)))

(define-postoptimize flic-case-block (object)
  (for-each (function postoptimize) (flic-case-block-exps object)))

(define-postoptimize flic-if (object)
  (postoptimize (flic-if-test-exp object))
  (postoptimize (flic-if-then-exp object))
  (postoptimize (flic-if-else-exp object)))

(define-postoptimize flic-return-from (object)
  (postoptimize (flic-return-from-exp object)))

(define-postoptimize flic-sel (object)
  (postoptimize (flic-sel-exp object)))

(define-postoptimize flic-is-constructor (object)
  (postoptimize (flic-is-constructor-exp object)))

(define-postoptimize flic-con-number (object)
  (postoptimize (flic-con-number-exp object)))

(define-postoptimize flic-void (object)
  object)
