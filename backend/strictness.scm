;;; strictness.scm -- strictness analyzer
;;;
;;; author :  Sandra Loosemore
;;; date   :  28 May 1992
;;;
;;; The algorithm used here follows Consel, "Fast Strictness Analysis
;;; Via Symbolic Fixpoint Interation".
;;;
;;; The basic idea is to do a traversal of the flic structure, building
;;; a boolean term that represents the strictness of each subexpression.
;;; The boolean terms are composed of ands & ors of the argument variables
;;; to each function.  After traversing the body of the function, we can
;;; determine which argument variables are strict by examining the 
;;; corresponding term, and then we can update the strictness attribute
;;; of the var that names the function.
;;;
;;; Another traversal needs to be done to attach strictness properties
;;; to locally bound variables.  


;;; Here's the main entry point.

(define (strictness-analysis-top big-let)
  (fun-strictness-walk big-let)
  (var-strictness-walk big-let '() '())
  ;; *** This probably belongs somewhere else?
  (do-box-analysis big-let '() '() '#t)
  big-let)


;;;======================================================================
;;; Function strictness analyzer code walk
;;;======================================================================

;;; This actually involves two code walkers.  The first merely traverses
;;; structure and identifies function definitions.  The second traverses
;;; the definitions of the functions to compute their strictness.


;;; Fun-strictness-walk is the walker to find function definitions.
;;; This is trivial for everything other than flic-let.

(define-flic-walker fun-strictness-walk (object))

(define-fun-strictness-walk flic-lambda (object)
  (fun-strictness-walk (flic-lambda-body object)))

(define-fun-strictness-walk flic-let (object)
  (if (flic-let-recursive? object)
      (fun-strictness-walk-letrec object)
      (fun-strictness-walk-let* object))
  (dolist (v (flic-let-bindings object))
    (fun-strictness-walk (var-value v)))
  (fun-strictness-walk (flic-let-body object)))

(define-fun-strictness-walk flic-app (object)
  (fun-strictness-walk (flic-app-fn object))
  (for-each (function fun-strictness-walk) (flic-app-args object)))

(define-fun-strictness-walk flic-ref (object)
  (declare (ignore object))
  '#f)

(define-fun-strictness-walk flic-pack (object)
  (declare (ignore object))
  '#f)

(define-fun-strictness-walk flic-const (object)
  (declare (ignore object))
  '#f)

(define-fun-strictness-walk flic-case-block (object)
  (for-each (function fun-strictness-walk) (flic-case-block-exps object)))

(define-fun-strictness-walk flic-return-from (object)
  (fun-strictness-walk (flic-return-from-exp object)))

(define-fun-strictness-walk flic-and (object)
  (for-each (function fun-strictness-walk) (flic-and-exps object)))

(define-fun-strictness-walk flic-if (object)
  (fun-strictness-walk (flic-if-test-exp object))
  (fun-strictness-walk (flic-if-then-exp object))
  (fun-strictness-walk (flic-if-else-exp object)))

(define-fun-strictness-walk flic-sel (object)
  (fun-strictness-walk (flic-sel-exp object)))

(define-fun-strictness-walk flic-is-constructor (object)
  (fun-strictness-walk (flic-is-constructor-exp object)))

(define-fun-strictness-walk flic-con-number (object)
  (fun-strictness-walk (flic-con-number-exp object)))

(define-fun-strictness-walk flic-void (object)
  (declare (ignore object))
  '#f)



;;; Here is the magic for let bindings of function definitions.
;;; Sequential bindings are easy.  For recursive bindings, we must 
;;; keep track of mutually recursive functions.
;;; If a function binding has a strictness annotation attached,
;;; do not mess with it further.

(define (fun-strictness-walk-let* object)
  (dolist (var (flic-let-bindings object))
    (let ((val  (var-value var)))
      (when (is-type? 'flic-lambda val)
	(if (var-strictness var)
	    (mark-argument-strictness
	      (var-strictness var) (flic-lambda-vars val))
	    (compute-function-strictness var val '())))
      )))

(define (fun-strictness-walk-letrec object)
  (let ((stack   '()))
    (dolist (var (flic-let-bindings object))
      (let ((val  (var-value var)))
	(if (and (is-type? 'flic-lambda val) (not (var-strictness var)))
	    (setf stack (add-recursive-function-1 var (init-var-env) stack)))))
    (dolist (var (flic-let-bindings object))
      (let ((val  (var-value var)))
	(when (is-type? 'flic-lambda val)
	  (if (var-strictness var)
	      (mark-argument-strictness
	        (var-strictness var) (flic-lambda-vars val))
	      (compute-function-strictness var val stack)))
	))))

(define (compute-function-strictness var val stack)
  (let* ((vars  (flic-lambda-vars val))
	 (env   (add-var-binding-n vars (map (function list) vars)
				   (init-var-env)))
	 (term  (compute-strictness-walk (flic-lambda-body val) env stack)))
    (when (eq? term '#t)
      (signal-infinite-loop-function var)
      (setf (flic-lambda-body val)
	    (make-infinite-loop-error
	      (format '#f "Function ~s has an infinite loop." var))))
    (setf (var-strictness var) (munge-strictness-terms term vars))))


(define (signal-infinite-loop-function var)
  (recoverable-error 'infinite-loop-function
    "Function ~s has an infinite loop."
    var))

(define (make-infinite-loop-error msg)
  (make-flic-app
    (make-flic-ref (core-symbol "error"))
    (list (make-flic-const msg))
    '#t))

  
;;; compute-strictness-walk is the traversal to compute strictness
;;; terms.
;;; The purpose of the env is to map locally bound variables onto 
;;; strictness terms which are expressed as lists of argument variables
;;; to the function being analyzed.
;;; The purpose of the stack is to keep track of recursive function
;;; walks and recognize when we've reached a fixed point.

(define-flic-walker compute-strictness-walk (object env stack))


;;; Making a function never forces anything.

(define-compute-strictness-walk flic-lambda (object env stack)
  (declare (ignore object env stack))
  '#f)


;;; For let, add bindings to environment and get strictness of body.

(define-compute-strictness-walk flic-let (object env stack)
  (let ((bindings    (flic-let-bindings object))
	(body        (flic-let-body object))
	(recursive?  (flic-let-recursive? object)))
    (if recursive?
	;; Must add stuff to env and stack before traversing anything.
	(begin
	  (dolist (var bindings)
	    (setf env (add-var-binding-1 var '#f env)))
	  (dolist (var bindings)
	    (let ((val  (var-value var)))
	      (when (is-type? 'flic-lambda val)
		(setf stack (add-recursive-function-1 var env stack)))))
	  (dolist (var bindings)
	    (let ((val  (var-value var)))
	      (set-var-env var env (compute-strictness-walk val env stack)))))
	;; Otherwise just do things sequentially.
	;; Note that even though there is no possibility of recursion
	;; here, we must add stuff to the stack anyway so that we can
	;; walk calls in the correct env.
	(dolist (var bindings)
	  (let ((val  (var-value var)))
	    (when (is-type? 'flic-lambda val)
	      (setf stack (add-recursive-function-1 var env stack)))
	    (setf env
		  (add-var-binding-1
		    var (compute-strictness-walk val env stack) env)))))
    (compute-strictness-walk body env stack)))


;;; Treat explicit, saturated calls to named functions specially.

(define-compute-strictness-walk flic-app (object env stack)
  (let ((fn         (flic-app-fn object))
	(args       (flic-app-args object))
	(saturated? (flic-app-saturated? object)))
    (cond ((and (is-type? 'flic-ref fn) saturated?)
	   ;; Special handling for named functions.
	   (compute-application-strictness
	     (flic-ref-var fn)
	     args env stack))
	  ((and (is-type? 'flic-pack fn) saturated?)
	   ;; Similarly for constructor applications, but we always
	   ;; know which arguments are strict in advance.
	   (compute-application-strictness-aux
	      (con-slot-strict? (flic-pack-con fn))
	      args env stack))
	  (else
	   ;; Otherwise, we know that the function expression is going to
	   ;; be forced, but all of its arguments are lazy.  So ignore the
	   ;; arguments in computing the strictness of the whole expression.
	   (compute-strictness-walk fn env stack)))))


(define (compute-application-strictness var args env stack)
  (let* ((strictness          (var-strictness var))
	 (info                '#f)
	 (arg-strictness-list '#f))
    (cond ((eq? var (core-symbol "error"))
	   ;; This expression will return bottom no matter what.
	   'error)
	  (strictness
	   ;; We've already completed the walk for this function and
	   ;; determined which of its arguments are strict.
	   ;; The strictness expression for the application is the
	   ;; OR of the strictness of its non-lazy arguments.
	   (compute-application-strictness-aux strictness args env stack))
	  ((get-recursive-function-trace
	     (setf arg-strictness-list
		   (map (lambda (a) (compute-strictness-walk a env stack))
			args))
	     (setf info (get-recursive-function var stack)))
	   ;; We're already tracing this call.  Return true to
	   ;; terminate the fixpoint iteration.
	   '#t)
	  (else
	   ;; Otherwise, begin a new trace instance.
	   ;; Add stuff to the saved var-env to map references to
	   ;; the argument variables to the strictness terms for
	   ;; the actual arguments at this call site.
	   ;; References to closed-over variables within the function
	   ;; use the strictness values that were stored in the env
	   ;; at the point of function definition.
	   (let* ((env      (get-recursive-function-env info))
		  (lambda   (var-value var))
		  (body     (flic-lambda-body lambda))
		  (vars     (flic-lambda-vars lambda))
		  (result   '#f))
	     (push-recursive-function-trace arg-strictness-list info)
	     (setf result
		   (compute-strictness-walk
		     body
		     (add-var-binding-n vars arg-strictness-list env)
		     stack))
	     (pop-recursive-function-trace info)
	     result))
	  )))


(define (compute-application-strictness-aux strictness args env stack)
  (make-or-term
    (map (lambda (strict? arg)
	   (if strict? (compute-strictness-walk arg env stack) '#f))
	 strictness args)))


;;; For a reference, look up the term associated with the variable in env.
;;; If not present in the environment, ignore it; the binding was established
;;; outside the scope of the function being analyzed.

(define-compute-strictness-walk flic-ref (object env stack)
  (declare (ignore stack))
  (get-var-env (flic-ref-var object) env))
	

;;; References to constants or constructors never fail.

(define-compute-strictness-walk flic-const (object env stack)
  (declare (ignore object env stack))
  '#f)

(define-compute-strictness-walk flic-pack (object env stack)
  (declare (ignore object env stack))
  '#f)


;;; The first clause of a case-block is the only one that is always
;;; executed, so it is the only one that affects the strictness of
;;; the overall expression.

(define-compute-strictness-walk flic-case-block (object env stack)
  (compute-strictness-walk (car (flic-case-block-exps object)) env stack))


;;; Return-from fails if its subexpression fails.

(define-compute-strictness-walk flic-return-from (object env stack)
  (compute-strictness-walk (flic-return-from-exp object) env stack))


;;; For and, the first subexpression is the only one that is always
;;; executed, so it is the only one that affects the strictness of
;;; the overall expression.

(define-compute-strictness-walk flic-and (object env stack)
  (compute-strictness-walk (car (flic-and-exps object)) env stack))


;;; The strictness of an IF is the strictness of the test OR'ed
;;; with the AND of the strictness of its branches.

(define-compute-strictness-walk flic-if (object env stack)
  (make-or-term-2
    (compute-strictness-walk (flic-if-test-exp object) env stack)
    (make-and-term-2
      (compute-strictness-walk (flic-if-then-exp object) env stack)
      (compute-strictness-walk (flic-if-else-exp object) env stack))))


;;; Selecting a component of a data structure causes it to be forced,
;;; so propagate the strictness of the subexpression upwards.

(define-compute-strictness-walk flic-sel (object env stack)
  (compute-strictness-walk (flic-sel-exp object) env stack))


;;; Is-constructor and con-number force their subexpressions.

(define-compute-strictness-walk flic-is-constructor (object env stack)
  (compute-strictness-walk (flic-is-constructor-exp object) env stack))

(define-compute-strictness-walk flic-con-number (object env stack)
  (compute-strictness-walk (flic-con-number-exp object) env stack))

(define-compute-strictness-walk flic-void (object env stack)
  (declare (ignore object env stack))
  '#f)



;;;======================================================================
;;; Utilities for managing the env
;;;======================================================================

;;; The env is just an a-list.

(define (init-var-env)
  '())

(define (add-var-binding-1 var binding env)
  (cons (cons var binding) env))

(define (add-var-binding-n vars bindings env)
  (if (null? vars)
      env
      (add-var-binding-n (cdr vars) (cdr bindings)
			 (cons (cons (car vars) (car bindings)) env))))

(define (get-var-env var env)
  (let ((stuff  (assq var env)))
    (if stuff
	(cdr stuff)
	'#f)))

(define (set-var-env var env new-value)
  (let ((stuff  (assq var env)))
    (if stuff
	(setf (cdr stuff) new-value)
	(error "Can't find binding for ~s in environment." var))))
  


;;;======================================================================
;;; Utilities for managing the stack
;;;======================================================================

;;; For now, the stack is just an a-list too.
;;; Some sort of hashing scheme could also be used instead of a linear
;;; search, but if the iteration depth for the fixpoint analysis is
;;; small, it's probably not worth the trouble.

(define (add-recursive-function-1 var env stack)
  (cons (list var env '()) stack))

(define (get-recursive-function var stack)
  (or (assq var stack)
      (error "Can't find entry for ~s in stack." var)))

(define (get-recursive-function-env entry)
  (cadr entry))

(define (push-recursive-function-trace new-args entry)
  (push new-args (caddr entry)))

(define (pop-recursive-function-trace entry)
  (pop (caddr entry)))

(define (get-recursive-function-trace args entry)
  (get-recursive-function-trace-aux args (caddr entry)))

(define (get-recursive-function-trace-aux args list)
  (cond ((null? list)
	 '#f)
	((every (function term=) args (car list))
	 '#t)
	(else
	 (get-recursive-function-trace-aux args (cdr list)))))


;;;======================================================================
;;; Utilities for boolean terms
;;;======================================================================


;;; A term is either #t, #f, the symbol 'error, or a list of variables 
;;; (which are implicitly or'ed together).
;;; #t and 'error are treated identically, except that #t indicates
;;; failure because of infinite recursion and 'error indicates failure
;;; due to a call to the error function.
;;; In general, AND terms add nothing to the result, so to reduce
;;; needless computation we generally reduce (and a b) to #f.

;;; Make an OR term.  First look for some obvious special cases as an
;;; efficiency hack, otherwise fall through to more general code.

(define (make-or-term terms)
  (cond ((null? terms)
	 '#f)
	((null? (cdr terms))
	 (car terms))
	((eq? (car terms) '#t)
	 '#t)
	((eq? (car terms) 'error)
	 'error)
	((eq? (car terms) '#f)
	 (make-or-term (cdr terms)))
	(else
	 (make-or-term-2 (car terms) (make-or-term (cdr terms))))))

(define (make-or-term-2 term1 term2)
  (cond ((eq? term2 '#t)
	 '#t)
	((eq? term2 'error)
	 'error)
	((eq? term2 '#f)
	 term1)
	((eq? term1 '#t)
	 '#t)
	((eq? term1 'error)
	 'error)
	((eq? term1 '#f)
	 term2)
	;; At this point we know both terms are variable lists.
	((implies? term2 term1)
	 term2)
	((implies? term1 term2)
	 term1)
	(else
	 (merge-list-terms term1 term2))))


;;;  Merge the two lists, throwing out duplicate variables.

(define (merge-list-terms list1 list2)
  (cond ((null? list1)
	 list2)
	((null? list2)
	 list1)
	((eq? (car list1) (car list2))
	 (cons (car list1) (merge-list-terms (cdr list1) (cdr list2))))
	((var< (car list1) (car list2))
	 (cons (car list1) (merge-list-terms (cdr list1) list2)))
	(else
	 (cons (car list2) (merge-list-terms list1 (cdr list2))))))


;;; Helper function: does term1 imply term2?
;;; True if every subterm of term2 is also included in term1.

(define (implies? term1 term2)
  (every (lambda (v2) (memq v2 term1)) term2))


;;; Make an AND term.  Because we don't want to build up arbitrarily
;;; complex AND expressions, basically just compute an OR list that 
;;; represents the intersection of the subterms.

(define (make-and-term terms)
  (cond ((null? terms)
	 '#f)
	((null? (cdr terms))
	 (car terms))
	((eq? (car terms) '#t)
	 (make-and-term (cdr terms)))
	((eq? (car terms) 'error)
	 (make-and-term (cdr terms)))
	((eq? (car terms) '#f)
	 '#f)
	(else
	 (make-and-term-2 (car terms) (make-and-term (cdr terms))))))

(define (make-and-term-2 term1 term2)
  (cond ((eq? term2 '#t)
	 term1)
	((eq? term2 'error)
	 term1)
	((eq? term2 '#f)
	 '#f)
	((eq? term1 '#t)
	 term2)
	((eq? term1 'error)
	 term2)
	((eq? term1 '#f)
	 '#f)
	;; At this point we know both terms are variable lists.
	((implies? term2 term1)
	 term1)
	((implies? term1 term2)
	 term2)
	(else
	 (let ((result  '()))
	   (dolist (v term1)
	     (if (memq v term2)
		 (push v result)))
	   (if (null? result)
	       '#f
	       (nreverse result))))
	))


;;; Subterms of an and/or term are always sorted, so that to compare
;;; two terms we can just compare subterms componentwise.

(define (term= term1 term2)
  (or (eq? term1 term2)
      (and (pair? term1)
	   (pair? term2)
	   (eq? (car term1) (car term2))
	   (term= (cdr term1) (cdr term2)))))


;;; Variables within an OR-list are sorted alphabetically by names.

(define (var< var1 var2)
  (string<? (symbol->string (def-name var1))
	    (symbol->string (def-name var2))))


;;; Determine which of the vars are present in the term.

(define (munge-strictness-terms term vars)
  (map (lambda (v)
	 (setf (var-strict? v)
	       (cond ((var-force-strict? v)
		      '#t)
		     ((eq? term '#t)
		      '#t)
		     ((eq? term 'error)
		      '#t)
		     ((eq? term '#f)
		      '#f)
		     ((memq v term)
		      '#t)
		     (else
		      '#f))))
       vars))

(define (mark-argument-strictness strictness vars)
  (map (lambda (s v) (setf (var-strict? v) s)) strictness vars))



;;;======================================================================
;;; Variable strictness propagation code walk
;;;======================================================================

;;; Walk the code, marking any vars found in strict contexts as strict.
;;; Locally bound variables are consed onto the varlist.  This is
;;; used to determine which variables can be marked as strict when they
;;; appear in strict contexts.
;;; When walking something that does not appear in a strict context
;;; or that is not always evaluated, reinitialize varlist to the empty
;;; list.
;;; The stack is used to keep track of variables that have not been
;;; initialized yet, so that we can detect some kinds of infinite loops.
;;; When walking something that is not always evaluated, reset this to 
;;; the empty list.

(define-flic-walker var-strictness-walk (object varlist stack))



;;; Since the body of the lambda might not be evaluated, reset
;;; both varlist and stack.

(define-var-strictness-walk flic-lambda (object varlist stack)
  (declare (ignore varlist stack))
  (var-strictness-walk (flic-lambda-body object) '() '()))


;;; The basic idea for let is to find the variables that are strict in 
;;; the body first, and propagate that information backwards to the 
;;; binding initializers.

(define-var-strictness-walk flic-let (object varlist stack)
  (let ((bindings  (flic-let-bindings object)))
    (var-strictness-walk-let-aux
      bindings
      (flic-let-body object)
      (append bindings varlist)
      (append bindings stack)
      (flic-let-recursive? object))))

(define (var-strictness-walk-let-aux bindings body varlist stack recursive?)
  (if (null? bindings)
      (var-strictness-walk body varlist stack)
      (begin
	(var-strictness-walk-let-aux
	  (cdr bindings) body varlist (cdr stack) recursive?)
	(let* ((var  (car bindings))
	       (val  (var-value var)))
	  (cond ((var-strict? var)
		 ;; Recursive variables have to be set back to unstrict
		 ;; because the value form might contain forward references.
		 ;; The box analyzer will set them to strict again if the
		 ;; value forms are safe.
		 (when recursive? (setf (var-strict? var) '#f))
		 ;; Detect x = 1 + x circularities here
		 (var-strictness-walk val varlist stack))
		((flic-exp-strict-result? val)
		 ;; The val is going to be wrapped in a delay.
		 (var-strictness-walk val '() '()))
		(else
		 ;; Watch out for x = x and x = cdr x circularities.
		 ;; *** I am still a little confused about this.  It
		 ;; *** seems like the stack should be passed through
		 ;; *** when walking already-boxed values that appear as
                 ;; *** non-strict function arguments as well, but doing
		 ;; *** so generates some apparently bogus complaints
		 ;; *** about infinite loops.  So maybe doing it here
		 ;; *** is incorrect too, and we just haven't run across
		 ;; *** a test case that triggers it???
		 (var-strictness-walk val '() stack))
		)))))


(define (flic-exp-strict-result? val)
  (cond ((is-type? 'flic-ref val)
	 (var-strict? (flic-ref-var val)))
	((is-type? 'flic-sel val)
	 (list-ref (con-slot-strict? (flic-sel-con val)) (flic-sel-i val)))
	(else
	 '#t)))

(define-var-strictness-walk flic-app (object varlist stack)
  (let ((fn           (flic-app-fn object))
	(args         (flic-app-args object))
	(saturated?   (flic-app-saturated? object)))
    (cond ((and saturated? (is-type? 'flic-ref fn))
	   ;; Strictness of function should be stored on var
	   (do-var-strictness-flic-app-aux
	     (var-strictness (flic-ref-var fn))
	     fn args varlist stack))
	  ((and saturated? (is-type? 'flic-pack fn))
	   ;; Strictness of constructor should be stored on con
	   (do-var-strictness-flic-app-aux
	     (con-slot-strict? (flic-pack-con fn))
	     fn args varlist stack))
	  (else
	   ;; All arguments are non-strict
	   (var-strictness-walk fn varlist stack)
	   (dolist (a args)
	     (var-strictness-walk a '() '()))))))

(define (do-var-strictness-flic-app-aux strictness fn args varlist stack)
  (when (not strictness)
    (error "Can't find strictness for function ~s." fn))
  (dolist (a args)
    (if (pop strictness)
	(var-strictness-walk a varlist stack)
	(var-strictness-walk a '() '()))))


(define-var-strictness-walk flic-ref (object varlist stack)
  (let ((var  (flic-ref-var object)))
    (cond ((memq var stack)
	   ;; Circular variable definition detected.
	   (signal-infinite-loop-variable var)
	   (setf (var-value var)
		 (make-infinite-loop-error
		   (format '#f "Variable ~s has an infinite loop." var))))
	  ((memq var varlist)
	   (setf (var-strict? var) '#t))
	  (else
	   '#f))))

(define (signal-infinite-loop-variable var)
  (recoverable-error 'infinite-loop-variable
    "Variable ~s has an infinite loop."
    var))

(define-var-strictness-walk flic-const (object varlist stack)
  (declare (ignore object varlist stack))
  '#f)

(define-var-strictness-walk flic-pack (object varlist stack)
  (declare (ignore object varlist stack))
  '#f)

(define-var-strictness-walk flic-case-block (object varlist stack)
  (var-strictness-walk (car (flic-case-block-exps object)) varlist stack)
  (dolist (exp (cdr (flic-case-block-exps object)))
    (var-strictness-walk exp '() '())))

(define-var-strictness-walk flic-return-from (object varlist stack)
  (var-strictness-walk (flic-return-from-exp object) varlist stack))

(define-var-strictness-walk flic-and (object varlist stack)
  (var-strictness-walk (car (flic-and-exps object)) varlist stack)
  (dolist (exp (cdr (flic-and-exps object)))
    (var-strictness-walk exp '() '())))

(define-var-strictness-walk flic-if (object varlist stack)
  (var-strictness-walk (flic-if-test-exp object) varlist stack)
  (var-strictness-walk (flic-if-then-exp object) '() '())
  (var-strictness-walk (flic-if-else-exp object) '() '()))

(define-var-strictness-walk flic-sel (object varlist stack)
  (var-strictness-walk (flic-sel-exp object) varlist stack))

(define-var-strictness-walk flic-is-constructor (object varlist stack)
  (var-strictness-walk (flic-is-constructor-exp object) varlist stack))

(define-var-strictness-walk flic-con-number (object varlist stack)
  (var-strictness-walk (flic-con-number-exp object) varlist stack))

(define-var-strictness-walk flic-void (object varlist stack)
  (declare (ignore object varlist stack))
  '#f)



;;;======================================================================
;;; Printer support
;;;======================================================================

(define (strictness-analysis-printer big-let)
  (print-strictness big-let 0))

(define (print-strictness-list list depth)
  (dolist (o list)
    (print-strictness o depth)))

(define (print-strictness-indent depth)
  (dotimes (i (* 2 depth))
    (declare (ignorable i))
    (write-char #\space)))

(define (strictness-string bool)
  (if bool "#t" "#f"))

(define-flic-walker print-strictness (object depth))

(define-print-strictness flic-lambda (object depth)
  (print-strictness-indent depth)
  (format '#t "In anonymous function:~%")
  (print-strictness (flic-lambda-body object) (1+ depth)))

(define-print-strictness flic-let (object depth)
  (dolist (var (flic-let-bindings object))
    (let ((val  (var-value var)))
      (if (is-type? 'flic-lambda val)
	  (begin
	    (print-strictness-indent depth)
	    (format '#t "Function ~s has argument strictness ~a.~%"
		    var
		    (map (function strictness-string) (var-strictness var)))
	    (print-strictness (flic-lambda-body val) (1+ depth)))
	  (begin
	    (print-strictness-indent depth)
	    (format '#t "Variable ~s has strictness ~a.~%"
		    var
		    (strictness-string (var-strict? var)))
	    (print-strictness val depth)))))
  (print-strictness (flic-let-body object) depth))

(define-print-strictness flic-app (object depth)
  (print-strictness (flic-app-fn object) depth)
  (print-strictness-list (flic-app-args object) depth))

(define-print-strictness flic-ref (object depth)
  (declare (ignore object depth))
  '#f)

(define-print-strictness flic-const (object depth)
  (declare (ignore object depth))
  '#f)

(define-print-strictness flic-pack (object depth)
  (declare (ignore object depth))
  '#f)

(define-print-strictness flic-case-block (object depth)
  (print-strictness-list (flic-case-block-exps object) depth))

(define-print-strictness flic-return-from (object depth)
  (print-strictness (flic-return-from-exp object) depth))

(define-print-strictness flic-and (object depth)
  (print-strictness-list (flic-and-exps object) depth))

(define-print-strictness flic-if (object depth)
  (print-strictness (flic-if-test-exp object) depth)
  (print-strictness (flic-if-then-exp object) depth)
  (print-strictness (flic-if-else-exp object) depth))

(define-print-strictness flic-sel (object depth)
  (print-strictness (flic-sel-exp object) depth))

(define-print-strictness flic-is-constructor (object depth)
  (print-strictness (flic-is-constructor-exp object) depth))

(define-print-strictness flic-con-number (object depth)
  (print-strictness (flic-con-number-exp object) depth))

(define-print-strictness flic-void (object depth)
  (declare (ignore object depth))
  '#f)

