;;; pattern.scm -- cfn processing of pattern-related AST structures
;;;
;;; author :  Sandra Loosemore
;;; date   :  27 Feb 1992
;;;
;;; This file contains specialized CFN walkers for lambda, case, and valdef
;;; structures.



;;;=====================================================================
;;; Top-level walkers
;;;=====================================================================


;;; The calls to remember-context are so an appropriate error message
;;; can be produced for pattern-matching failures.

(define-walker-method cfn lambda (object)
  (remember-context object
    (do-cfn-lambda (lambda-pats object) (lambda-body object))))


(define-walker-method cfn case (object)
  (remember-context object
    (do-cfn-case
      (case-exp object)
      (case-alts object))))




;;; Valdefs are always processed as a list.

(define (cfn-valdef-list list-of-valdefs)
  (if (null? list-of-valdefs)
      '()
      (nconc (cfn-valdef (car list-of-valdefs))
	     (cfn-valdef-list (cdr list-of-valdefs)))))

(define (cfn-valdef object)
  (remember-context object
    (if (null? (single-fun-def-args (car (valdef-definitions object))))
	;; This is a pattern binding.
	(do-cfn-pattern-def-top object)
	;; This is a function binding.
	;; Branch on single-headed/multi-headed definition.
	(list (add-dict-params
	        object
		(if (null? (cdr (valdef-definitions object)))
		    (do-cfn-function-def-simple object)
		    (do-cfn-function-def-general object))))
      )))


;;; This adds the dictionary parameters needed by the type system.  A valdef
;;; structure has a dictionary-args field which contains the variables to be
;;; bound to dicationary arguments.

(define (add-dict-params original-valdef generated-valdef)
  (let ((vars (valdef-dictionary-args original-valdef)))
    (when (not (null? vars))
      (let* ((sfd  (car (valdef-definitions generated-valdef)))
	     (rhs  (car (single-fun-def-rhs-list sfd)))
	     (exp  (guarded-rhs-rhs rhs))
	     (pats (map (function **var-pat/def) vars)))
	(if (is-type? 'lambda exp)
	    (setf (lambda-pats exp)
		  (nconc pats (lambda-pats exp)))
	    (setf (guarded-rhs-rhs rhs)
		  (**lambda/pat pats exp))))))
  generated-valdef)


;;;=====================================================================
;;; Lambda rewriting
;;;=====================================================================


;;; For lambda, make all the argument patterns into var pats.
;;; Rewrite the body as a CASE to do any more complicated pattern
;;; matching.
;;; The CFN output for lambda is a modified lambda expression with
;;; all var-pats as arguments.

(define (do-cfn-lambda pats body)
  (let ((new-args  '())
	(new-vars  '())
	(new-pats  '()))
    (dolist (p pats)
      (typecase p
	(wildcard-pat
	  (push (**var-pat/def (create-temp-var 'arg)) new-args))
        (var-pat
	  (push p new-args))
	(as-pat
	  (let ((var  (var-ref-var (as-pat-var p))))
	    (push (**var-pat/def var) new-args)
	    (push (**var/def var) new-vars)
	    (push (as-pat-pattern p) new-pats)))
	(else
	  (let ((var  (create-temp-var 'arg)))
	    (push (**var-pat/def var) new-args)
	    (push (**var/def var) new-vars)
	    (push p new-pats)))))
    (setf new-args (nreverse new-args))
    (setf new-vars (nreverse new-vars))
    (setf new-pats (nreverse new-pats))
    (**lambda/pat
      new-args
      (cond ((null? new-vars)
	     ;; No fancy pattern matching necessary.
	     (cfn-ast-1 body))
	    ((null? (cdr new-vars))
	     ;; Exactly one argument to match on.
	     (do-cfn-case (car new-vars)
			  (list (**alt/simple (car new-pats) body))))
	    (else
	     ;; Multiple arguments to match on.
	     (do-cfn-case-tuple
	       new-vars
	       (list (**alt/simple (**tuple-pat new-pats) body))))
	    ))))


;;;=====================================================================
;;; Function definitions
;;;=====================================================================


;;; The output of the CFN for function definitions is a simple 
;;; valdef which binds a variable to a lambda expression.


;;; The simple case:  there is only one set of arguments.

(define (do-cfn-function-def-simple object)
  (let* ((pat    (valdef-lhs object))
	 (sfd    (car (valdef-definitions object))))
    (**valdef/pat
      pat
      (do-cfn-lambda
        (single-fun-def-args sfd)
	(rewrite-guards-and-where-decls
	  (single-fun-def-where-decls sfd)
	  (single-fun-def-rhs-list sfd)
	  '#f)))))


;;; The general case:  generate new variables as the formal parameters 
;;; to the resulting lambda, then use case to do the pattern matching.

(define (do-cfn-function-def-general object)
  (let ((pat   (valdef-lhs object))
	(vars  (map (lambda (p)
		      (declare (ignore p))
		      (create-temp-var 'arg))
		    (single-fun-def-args (car (valdef-definitions object)))))
	(alts  (map (lambda (sfd)
		      (**alt (**tuple-pat (single-fun-def-args sfd))
			     (single-fun-def-rhs-list sfd)
			     (single-fun-def-where-decls sfd)))
		    (valdef-definitions object))))
    (**valdef/pat
      pat
      (**lambda/pat
        (map (function **var-pat/def) vars)
	(if (null? (cdr vars))
	    ;; one-argument case
	    (do-cfn-case (**var/def (car vars)) alts)
	    ;; multi-argument case
	    (do-cfn-case-tuple (map (function **var/def) vars) alts))))
    ))


;;;=====================================================================
;;; Case
;;;=====================================================================


;;; For case, add failure alt, then call helper function to generate
;;; pattern matching tests.
;;; The CFN output for case is a case-block construct.

(define (do-cfn-case exp alts)
  (setf alts
	(append alts
		(list (**alt/simple (**wildcard-pat) (make-failure-exp)))))
  (let ((list-of-pats  	(map (lambda (a) (list (alt-pat a))) alts)))
    (if (is-type? 'var-ref exp)
	(match-pattern-list (list exp) list-of-pats alts)
	(let ((temp  (create-temp-var 'cfn)))
	  (**let (list (**valdef/def temp (cfn-ast-1 exp)))
		 (match-pattern-list
		   (list (**var/def temp))
		   list-of-pats
		   alts)))
      )))



;;; Here's a special case, for when the exp being matched is a tuple
;;; of var-refs and all the alts also have tuple pats.

(define (do-cfn-case-tuple exps alts)
  (setf alts
	(append alts
		(list
		  (**alt/simple
		    (**tuple-pat
		      (map (lambda (e) (declare (ignore e)) (**wildcard-pat))
			   exps))
		    (make-failure-exp)))))
  (match-pattern-list
    exps
    (map (lambda (a) (pcon-pats (alt-pat a))) alts)
    alts))


(define (match-pattern-list exps list-of-pats alts)
  (let ((block-name  (gensym "PMATCH")))
    (**case-block
      block-name
      (map (lambda (a p) (match-pattern exps p a block-name))
	   alts
	   list-of-pats))))


;;; Produce an exp that matches the given alt against the exps.
;;; If the match succeeds, it will return-from the given block-name.

(define (match-pattern exps pats alt block-name)
  (if (null pats)
      ;; No more patterns to match.
      ;; Return an exp that handles the guards and where-decls.
      (cfn-ast-1
        (rewrite-guards-and-where-decls
	  (alt-where-decls alt) (alt-rhs-list alt) block-name))
      ;; Otherwise dispatch on type of first pattern.
      (let ((pat  (pop pats))
	    (exp  (pop exps)))
	(funcall
	  (typecase pat
	    (wildcard-pat (function match-wildcard-pat))
	    (var-pat      (function match-var-pat))
	    (pcon         (function match-pcon))
	    (as-pat       (function match-as-pat))
	    (irr-pat      (function match-irr-pat))
	    (const-pat    (function match-const-pat))
	    (plus-pat     (function match-plus-pat))
	    (list-pat     (function match-list-pat))
	    (else         (error "Unrecognized pattern ~s." pat)))
	  pat
	  exp
	  pats
	  exps
	  alt
	  block-name))))




;;; Wildcard patterns add no pattern matching test.
;;; Just recurse on the next pattern to be matched.

(define (match-wildcard-pat pat exp pats exps alt block-name)
  (declare (ignore pat exp))
  (match-pattern exps pats alt block-name))


;;; A variable pattern likewise does not add any test.  However,
;;; a binding of the variable to the corresponding exp must be added.

(define (match-var-pat pat exp pats exps alt block-name)
  (push (**valdef/pat pat exp)
	(alt-where-decls alt))
  (match-pattern exps pats alt block-name))


;;; Pcons are the hairy case because they may have subpatterns that need
;;; to be matched.
;;; If there are subpats and the exp is not a var-ref, make a let binding.
;;; If the con is a tuple type, there is no need to generate a test
;;; since the test would always succeed anyway.
;;; Do not generate let bindings here for subexpressions; do this lazily
;;; if and when necessary.

(define (match-pcon pat exp pats exps alt block-name)
  (let* ((var?    (is-type? 'var-ref exp))
	 (var     (if var?
		      (var-ref-var exp)
		      (create-temp-var 'conexp)))
	 (con     (pcon-con pat))
	 (arity   (con-arity con))
	 (alg     (con-alg con))
	 (tuple?  (algdata-tuple? alg))
	 (subpats (pcon-pats pat))
	 (subexps '()))
    (dotimes (i arity)
      (push (**sel con (**var/def var) i) subexps))
    (setf exps (nconc (nreverse subexps) exps))
    (setf pats (append subpats pats))
    (let ((tail  (match-pattern exps pats alt block-name)))
      (when (not tuple?)
	(setf tail
	      (**and-exp (**is-constructor (**var/def var) con) tail)))
      (when (not var?)
	(setf tail
	      (**let (list (**valdef/def var (cfn-ast-1 exp))) tail)))
      tail)))


;;; For as-pat, add a variable binding.
;;; If the expression being matched is not already a variable reference,
;;; take this opportunity to make the let binding.  Otherwise push the
;;; let-binding onto the where-decls.

(define (match-as-pat pat exp pats exps alt block-name)
  (let ((var    (var-ref-var (as-pat-var pat)))
	(subpat (as-pat-pattern pat)))
    (if (is-type? 'var-ref exp)
	(begin
	  (push (**valdef/def var (**var/def (var-ref-var exp)))
		(alt-where-decls alt))
	  (match-pattern
	    (cons exp exps)
	    (cons subpat pats)
	    alt
	    block-name))
	(**let (list (**valdef/def var (cfn-ast-1 exp)))
	       (match-pattern
		 (cons (**var/def var) exps)
		 (cons subpat pats)
		 alt
		 block-name)))))


;;; An irrefutable pattern adds no test to the pattern matching,
;;; but adds a pattern binding to the where-decls.

(define (match-irr-pat pat exp pats exps alt block-name)
  (let ((subpat  (irr-pat-pattern pat)))
    (push (**valdef/pat subpat exp) (alt-where-decls alt))
    (match-pattern exps pats alt block-name)))


;;; A const pat has a little piece of code inserted by the typechecker
;;; to do the test.
;;; For matches against string constants, generate an inline test to match 
;;; on each character of the string.

(define (match-const-pat pat exp pats exps alt block-name)
  (let ((const  (const-pat-value pat)))
    (**and-exp 
      (if (is-type? 'string-const const)
	  (let ((string  (string-const-value const)))
	    (if (string=? string "")
		(**is-constructor exp (core-symbol "Nil"))
		(**app (**var/def (core-symbol "primStringEq")) const exp)))
	  (cfn-ast-1 (**app (const-pat-match-fn pat) exp)))
      (match-pattern exps pats alt block-name))
    ))


;;; Plus pats have both a magic test and a piece of code to
;;; make a binding in the where-decls.  Make a variable binding
;;; for the exp if it's not already a variable.

(define (match-plus-pat pat exp pats exps alt block-name)
  (let* ((var?  (is-type? 'var-ref exp))
	 (var   (if var? (var-ref-var exp) (create-temp-var 'plusexp))))
    (push (**valdef/pat (plus-pat-pattern pat)
			(**app (plus-pat-bind-fn pat) (**var/def var)))
	  (alt-where-decls alt))
    (let ((tail  (match-pattern exps pats alt block-name)))
      (setf tail
	    (**and-exp
	      (cfn-ast-1 (**app (plus-pat-match-fn pat) (**var/def var)))
	      tail))
      (if var?
	  tail
	  (**let (list (**valdef/def var exp)) tail)))))


;;; Rewrite list pats as pcons, then process recursively.

(define (match-list-pat pat exp pats exps alt block-name)
  (let ((newpat  (rewrite-list-pat (list-pat-pats pat))))
    (match-pattern
      (cons exp exps)
      (cons newpat pats)
      alt
      block-name)))

(define (rewrite-list-pat subpats)
  (if (null? subpats)
      (**pcon/def (core-symbol "Nil") '())
      (**pcon/def (core-symbol ":")
		  (list (car subpats)
			(rewrite-list-pat (cdr subpats))))))




;;;=====================================================================
;;; Pattern definitions
;;;=====================================================================


(define (do-cfn-pattern-def-top object)
  (typecase (valdef-lhs object)
    (var-pat
      ;; If the pattern definition is a simple variable assignment, it
      ;; may have dictionary parameters that need to be messed with.
      ;; Complicated pattern bindings can't be overloaded in this way.
      (list (add-dict-params object (do-cfn-pattern-def-simple object))))
    (irr-pat
      ;; Irrefutable patterns are redundant here.
      (setf (valdef-lhs object) (irr-pat-pattern (valdef-lhs object)))
      (do-cfn-pattern-def-top object))
    (wildcard-pat
     ;; Wildcards are no-ops.
     '())
    (pcon
     ;; Special-case because it's frequent and general case creates
     ;; such lousy code
     (do-cfn-pattern-def-pcon object))
    (else
      (do-cfn-pattern-def-general object))))


;;; Do a "simple" pattern definition, e.g. one that already has a
;;; var-pat on the lhs.

(define (do-cfn-pattern-def-simple object)
  (let* ((pat  (valdef-lhs object))
	 (sfd  (car (valdef-definitions object)))
	 (exp  (rewrite-guards-and-where-decls
		 (single-fun-def-where-decls sfd)
		 (single-fun-def-rhs-list sfd)
		 '#f)))
  (**valdef/pat pat (cfn-ast-1 exp))))


;;; Destructure a pcon.
;;; Note that the simplified expansion is only valid if none of
;;; the subpatterns introduce tests.  Otherwise we must defer to
;;; the general case.

(define (do-cfn-pattern-def-pcon object)
  (let* ((pat     (valdef-lhs object))
	 (subpats (pcon-pats pat)))
    (if (every (function irrefutable-pat?) subpats)
	(let* ((con     (pcon-con pat))
	       (arity   (con-arity con))
	       (alg     (con-alg con))
	       (tuple?  (algdata-tuple? alg))
	       (temp    (create-temp-var 'pbind))
	       (result  '()))
	  (dotimes (i arity)
	    (setf result
		  (nconc result
			 (do-cfn-pattern-def-top 
			   (**valdef/pat (pop subpats)
					 (**sel con (**var/def temp) i))))))
	  (if (null? result)
	      '()
	      (let* ((sfd   (car (valdef-definitions object)))
		     (exp   (cfn-ast-1
			      (rewrite-guards-and-where-decls
			        (single-fun-def-where-decls sfd)
				(single-fun-def-rhs-list sfd)
				'#f))))
		(when (not tuple?)
		  (let ((temp1  (create-temp-var 'cfn)))
		    (setf exp
			  (**let (list (**valdef/def temp1 exp))
				 (**if (**is-constructor (**var/def temp1) con)
				       (**var/def temp1)
				       (make-failure-exp))))))
		(cons (**valdef/def temp exp) result))))
	(do-cfn-pattern-def-general object))))



;;; Turn a complicated pattern definition into a list of simple ones.
;;; The idea is to use case to match the pattern and build a tuple of
;;; all the values which are being destructured into the pattern
;;; variables.

(define (do-cfn-pattern-def-general object)
  (multiple-value-bind (new-pat vars new-vars)
      (copy-pattern-variables (valdef-lhs object))
    (if (not (null? vars))
	(let* ((sfd      (car (valdef-definitions object)))
	       (exp      (rewrite-guards-and-where-decls
			   (single-fun-def-where-decls sfd)
			   (single-fun-def-rhs-list sfd)
			   '#f))
	       (arity    (length vars)))
	  (if (eqv? arity 1)
	      (list (**valdef/def
		      (var-ref-var (car vars))
		      (do-cfn-case
		        exp
			(list (**alt/simple new-pat (car new-vars))))))
	      (let ((temp     (create-temp-var 'pbind))
		    (bindings '()))
		(dotimes (i arity)
		  (push (**valdef/def (var-ref-var (pop vars))
				      (**tuple-sel arity i (**var/def temp)))
			bindings))
		(cons (**valdef/def
		        temp
			(do-cfn-case
			  exp
			  (list (**alt/simple new-pat (**tuple/l new-vars)))))
		      bindings))))
	'())))



;;; Helper function for above.
;;; All the variables in the pattern must be replaced with temporary
;;; variables.  

(define (copy-pattern-variables pat)
  (typecase pat
    (wildcard-pat
      (values pat '() '()))
    (var-pat
      (let ((new  (create-temp-var (var-ref-name (var-pat-var pat)))))
	(values (**var-pat/def new)
		(list (var-pat-var pat))
		(list (**var/def new)))))
    (pcon
      (multiple-value-bind (new-pats vars new-vars)
	  (copy-pattern-variables-list (pcon-pats pat))
	(values (**pcon/def (pcon-con pat) new-pats)
		vars
		new-vars)))
    (as-pat
      (let ((new  (create-temp-var (var-ref-name (as-pat-var pat)))))
	(multiple-value-bind (new-pat vars new-vars)
	    (copy-pattern-variables (as-pat-pattern pat))
	  (values
	    (make as-pat
		  (var (**var/def new))
		  (pattern new-pat))
	    (cons (as-pat-var pat) vars)
	    (cons (**var/def new) new-vars)))))
    (irr-pat
      (multiple-value-bind (new-pat vars new-vars)
	  (copy-pattern-variables (irr-pat-pattern pat))
	(values
	  (make irr-pat (pattern new-pat))
	  vars
	  new-vars)))
    (const-pat
      (values pat '() '()))
    (plus-pat
      (multiple-value-bind (new-pat vars new-vars)
	  (copy-pattern-variables (plus-pat-pattern pat))
	(values
	  (make plus-pat
		(pattern new-pat)
		(k (plus-pat-k pat))
		(match-fn (plus-pat-match-fn pat))
		(bind-fn (plus-pat-bind-fn pat)))
	  vars
	  new-vars)))
    (list-pat
      (multiple-value-bind (new-pats vars new-vars)
	  (copy-pattern-variables-list (list-pat-pats pat))
	(values (make list-pat (pats new-pats))
		vars
		new-vars)))
    (else
      (error "Unrecognized pattern ~s." pat))))

(define (copy-pattern-variables-list pats)
  (let ((new-pats  '())
	(vars      '())
	(new-vars  '()))
    (dolist (p pats)
      (multiple-value-bind (p v n) (copy-pattern-variables p)
	(push p new-pats)
	(setf vars (nconc vars v))
	(setf new-vars (nconc new-vars n))))
    (values (nreverse new-pats)
	    vars
	    new-vars)))



;;;=====================================================================
;;; Helper functions for processing guards and where-decls
;;;=====================================================================

;;; Process guards and where-decls into a single expression.
;;; If block-name is non-nil, wrap the exp with a return-from.
;;; If block-name is nil, add a failure exp if necessary.
;;; Note that this does NOT do the CFN traversal on the result or
;;; any part of it.

(define (rewrite-guards-and-where-decls where-decls rhs-list block-name)
  (if (null? where-decls)
      (rewrite-guards rhs-list block-name)
      (**let where-decls
	     (rewrite-guards rhs-list block-name))))

(define (rewrite-guards rhs-list block-name)
  (if (null? rhs-list)
      (if block-name
	  (**con/def (core-symbol "False"))
	  (make-failure-exp))
      (let* ((rhs     (car rhs-list))
	     (guard   (guarded-rhs-guard rhs))
	     (exp     (guarded-rhs-rhs rhs)))
	(when block-name
	  (setf exp (**return-from block-name exp)))
	(cond ((is-type? 'omitted-guard (guarded-rhs-guard (car rhs-list)))
	       exp)
	      ((and block-name (null? (cdr rhs-list)))
	       (**and-exp guard exp))
	      (else
	       (**if guard
		     exp
		     (rewrite-guards (cdr rhs-list) block-name)))
	      ))))


(define (make-failure-exp)
  (let ((c  (dynamic *context*)))
    (**abort
      (if (not c)
	  "Pattern match failed."
	  (let* ((stuff  (ast-node-line-number c))
		 (line   (source-pointer-line stuff))
		 (file   (source-pointer-file stuff)))
	    (if (and (is-type? 'valdef c)
		     (is-type? 'var-pat (valdef-lhs c)))
		(format
		  '#f
		  "Pattern match failed in function ~a at line ~s in file ~a."
		  (valdef-lhs c) line file)
		(format
		  '#f
		  "Pattern match failed at line ~s in file ~a."
		  line file)))))))




