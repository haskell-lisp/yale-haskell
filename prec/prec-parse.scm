;;; prec-parse.scm -- do precedence parsing of expressions and patterns
;;;
;;; author :  John & Sandra
;;; date   :  04 Feb 1992
;;;
;;;


;;; ==================================================================
;;; Handling for pp-exp-list
;;; ==================================================================

;;; This function is called during the scope phase after all of the
;;; exps in a pp-exp-list have already been walked.  Basically, the
;;; purpose is to turn the original pp-exp-list into something else.
;;; Look for the section cases first and treat them specially.

;;; Sections are handled by inserting a magic cookie (void) into the
;;; list where the `missing' operand of the section would be and then
;;; making sure the cookie stays at the top.

;;; Unary minus needs checking to avoid things like a*-a.

(define (massage-pp-exp-list exps)
 (let* ((first-term (car exps))
        (last-term (car (last exps)))
        (type (cond ((infix-var-or-con? first-term) 'section-l)
		    ((infix-var-or-con? last-term) 'section-r)
		    (else 'exp)))
	(exps1 (cond ((eq? type 'section-l)
		      (cons (make void) exps))
		     ((eq? type 'section-r)
		      (append exps (list (make void))))
		     (else exps)))
	(parsed-exp (parse-pp-list '#f exps1)))
   (if (eq? type 'exp)
       parsed-exp
       (if (or (not (app? parsed-exp))
	       (not (app? (app-fn parsed-exp))))
	   (begin
	     (signal-section-precedence-conflict
	      (if (eq? type 'section-l) first-term last-term))
	     (make void))
	   (let ((rhs (app-arg parsed-exp))
		 (op (app-fn (app-fn parsed-exp)))
		 (lhs (app-arg (app-fn parsed-exp))))
	     (if (eq? type 'section-l)
		 (if (void? lhs)
		     (make section-l (op op) (exp rhs))
		     (begin
		       (signal-section-precedence-conflict first-term)
		       (make void)))
		 (if (void? rhs)
		     (make section-r (op op) (exp lhs))
		     (begin
		       (signal-section-precedence-conflict last-term)
		       (make void)))))))))


;;; ==================================================================
;;; Handling for pp-pat-list
;;; ==================================================================

;;; In this case, we have to do an explicit walk of the pattern looking
;;; at all of its subpatterns.
;;;  ** This is a crock - the scope walker needs fixing.

(define (massage-pattern pat)
  (cond ((is-type? 'as-pat pat)
	 (setf (as-pat-pattern pat) (massage-pattern (as-pat-pattern pat)))
	 pat)
	((is-type? 'irr-pat pat)
	 (setf (irr-pat-pattern pat) (massage-pattern (irr-pat-pattern pat)))
	 pat)
	((is-type? 'plus-pat pat)
	 (setf (plus-pat-pattern pat) (massage-pattern (plus-pat-pattern pat)))
	 pat)
	((is-type? 'pcon pat)
	 (when (eq? (pcon-con pat) *undefined-def*)
	   (setf (pcon-con pat) (lookup-toplevel-name (pcon-name pat))))
	 (setf (pcon-pats pat) (massage-pattern-list (pcon-pats pat)))
	 pat)
	((is-type? 'list-pat pat)
	 (setf (list-pat-pats pat) (massage-pattern-list (list-pat-pats pat)))
	 pat)
	((is-type? 'pp-pat-list pat)
	 (parse-pp-list '#t (massage-pattern-list (pp-pat-list-pats pat))))
	(else
	 pat)))

(define (massage-pattern-list pats)
  (map (function massage-pattern) pats))


;;; ==================================================================
;;; Shared support
;;; ==================================================================

;;; This is the main routine.

(define (parse-pp-list pattern? l)
  (mlet (((stack terms) (push-pp-stack '() l)))
    (pp-parse-next-term pattern? stack terms)))

(define (pp-parse-next-term pattern? stack terms)
  (if (null? terms)
      (reduce-complete-stack pattern? stack)
      (let ((stack (reduce-stronger-ops pattern? stack (car terms))))
	(mlet (((stack terms)
		(push-pp-stack (cons (car terms) stack) (cdr terms))))
	   (pp-parse-next-term pattern? stack terms)))))

(define (reduce-complete-stack pattern? stack)
  (if (pp-stack-op-empty? stack)
      (car stack)
      (reduce-complete-stack pattern? (reduce-pp-stack pattern? stack))))

(define (reduce-pp-stack pattern? stack)
  (let ((term (car stack))
	(op (cadr stack)))
    (if pattern?
	(cond ((pp-pat-plus? op)
	       (let ((lhs (caddr stack)))
		 (cond ((or (not (const-pat? term))
			    (and (not (var-pat? lhs))
				 (not (wildcard-pat? lhs))))
			(signal-plus-precedence-conflict term)
			(cddr stack))
		       (else
			(cons (make plus-pat (pattern lhs)
				             (k (integer-const-value
						 (const-pat-value term))))
			      (cdddr stack))))))
	      ((pp-pat-negated? op)
	       (cond ((const-pat? term)
		      (let ((v (const-pat-value term)))
			(if (integer-const? v)
			    (setf (integer-const-value v)
				  (- (integer-const-value v)))
			    (setf (float-const-numerator v)
				  (- (float-const-numerator v)))))
		      (cons term (cddr stack)))
		     (else
		      (signal-minus-precedence-conflict term)
		      (cons term (cddr stack)))))
	      (else
	       (setf (pcon-pats op) (list (caddr stack) term))
	       (cons op (cdddr stack))))
	(cond ((negate? op)
	       (cons (**app (**var/def (core-symbol "negate")) term)
		     (cddr stack)))
	      (else
	       (cons (**app op (caddr stack) term) (cdddr stack)))))))

(define (pp-stack-op-empty? stack)
  (null? (cdr stack)))

(define (top-stack-op stack)
  (cadr stack))

(define (push-pp-stack stack terms)
  (let ((term (car terms)))
    (if (or (negate? term) (pp-pat-negated? term))
	(begin
	  (when (and stack (stronger-op? (car stack) term))
	      (unary-minus-prec-conflict term))
	  (push-pp-stack (cons term stack) (cdr terms)))
	(values (cons term stack) (cdr terms)))))

(define (reduce-stronger-ops pattern? stack op)
  (cond ((pp-stack-op-empty? stack) stack)
	((stronger-op? (top-stack-op stack) op)
	 (reduce-stronger-ops pattern? (reduce-pp-stack pattern? stack) op))
	(else stack)))

(define (stronger-op? op1 op2)
  (let ((fixity1 (get-op-fixity op1))
	(fixity2 (get-op-fixity op2)))
    (cond ((> (fixity-precedence fixity1) (fixity-precedence fixity2))
	   '#t)
	  ((< (fixity-precedence fixity1) (fixity-precedence fixity2))
	   '#f)
	  (else
	   (let ((a1 (fixity-associativity fixity1))
		 (a2 (fixity-associativity fixity2)))
	     (if (eq? a1 a2)
		 (cond ((eq? a1 'l)
			'#t)
		       ((eq? a1 'r)
			'#f)
		       (else
			(signal-precedence-conflict op1 op2)
			'#t))
		 (begin
		   (signal-precedence-conflict op1 op2)
		   '#t))))
	  )))
	     
(define (get-op-fixity op)
  (cond ((var-ref? op)
	 (pp-get-var-fixity (var-ref-var op)))
	((con-ref? op)
	 (pp-get-con-fixity (con-ref-con op)))
	((pcon? op)
	 (pp-get-con-fixity (pcon-con op)))
	((or (negate? op) (pp-pat-negated? op))
	 (pp-get-var-fixity (core-symbol "-")))
	((pp-pat-plus? op)
	 (pp-get-var-fixity (core-symbol "+")))
	(else
	 (error "Bad op ~s in pp-parse." op))))

(define (pp-get-var-fixity def)
  (if (eq? (var-fixity def) '#f)
      default-fixity
      (var-fixity def)))

(define (pp-get-con-fixity def)
  (if (eq? (con-fixity def) '#f)
      default-fixity
      (con-fixity def)))

;;; Error handlers

(define (signal-section-precedence-conflict op)
  (phase-error 'section-precedence-conflict
    "Operators in section body have lower precedence than section operator ~A."
   op))

(define (signal-precedence-conflict op1 op2)
  (phase-error 'precedence-conflict
    "The operators ~s and ~s appear consecutively, but they have the same~%~
     precedence and are not either both left or both right associative.~%
     You must add parentheses to avoid a precedence conflict."
    op1 op2))

(define (signal-plus-precedence-conflict term)
  (phase-error 'plus-precedence-conflict
    "You need to put parentheses around the plus-pattern ~a~%~
     to avoid a precedence conflict."
    term))

(define (signal-minus-precedence-conflict arg)
  (phase-error 'minus-precedence-conflict
    "You need to put parentheses around the negative literal ~a~%~
     to avoid a precedence conflict."
    arg))

(define (unary-minus-prec-conflict arg)
  (recoverable-error 'minus-precedence-conflict
     "Operator ~A too strong for unary minus - add parens please!~%"
     arg))

