;;; prec-util.scm -- utilities for precedence parsing and printing of
;;;                  expressions
;;;
;;; author :  Sandra Loosemore
;;; date   :  15 Feb 1992
;;;
;;; The functions in this file are used by the expression printers
;;; and by precedence parsing.


;;; Uncurry the function application, looking for a con-ref as the
;;; actual function being applied.  Return the con-ref-con and a list
;;; of the arguments.

(define (extract-constructor fn args)
  (cond ((is-type? 'con-ref fn)
	 (values (con-ref-con fn) args))
	((is-type? 'app fn)
	 (extract-constructor (app-fn fn) (cons (app-arg fn) args)))
	(else
	 (values '#f '()))))


;;; If this is an infix operator application, there are really two nested
;;; applications that we handle at once.  The "fn" on the outer app
;;; points to a nested app which is a var-ref or con-ref with the infix?
;;; slot set to T.
;;; Returns three values: the fixity info, the operator, and the first
;;; argument (the arg to the outer application is the second argument).

(define (extract-infix-operator fn)
  (if (is-type? 'app fn)
      (let* ((new-fn  (app-fn  fn))
	     (arg     (app-arg fn))
	     (fixity  (operator-fixity new-fn)))
	(if fixity
	    (values fixity new-fn arg)
	    (values '#f '#f '#f)))
      (values '#f '#f '#f)))


;;; Return the fixity info for a reference to a var or con.
;;; If it doesn't have an explicit fixity, use the default of
;;; left associativity and precedence 9.

(define default-fixity
  (make fixity (associativity 'l) (precedence 9)))

(define (operator-fixity fn)
  (if (is-type? 'save-old-exp fn)
      (operator-fixity (save-old-exp-old-exp fn))
      (or (and (is-type? 'var-ref fn)
	       (var-ref-infix? fn)
	       (or (and (var-ref-var fn)
			(not (eq? (var-ref-var fn) *undefined-def*))
			(var-fixity (var-ref-var fn)))
		   default-fixity))
	  (and (is-type? 'con-ref fn)
	       (con-ref-infix? fn)
	       (or (and (con-ref-con fn)
			(not (eq? (con-ref-con fn) *undefined-def*))
			(con-fixity (con-ref-con fn)))
		   default-fixity))
	  (and (is-type? 'pcon fn)
	       (pcon-infix? fn)
	       (or (and (pcon-con fn)
			(not (eq? (pcon-con fn) *undefined-def*))
			(con-fixity (pcon-con fn)))
		   default-fixity))
	  '#f)))
  


;;; Determine the precedence of an expression.
;;; *** What about unary -?

(define (precedence-of-exp exp associativity)
  (cond ((is-type? 'save-old-exp exp)
	 (precedence-of-exp (save-old-exp-old-exp exp) associativity))
	((is-type? 'aexp exp) 10)
	((is-type? 'app exp)
	 (multiple-value-bind (fixity op arg1)
	     (extract-infix-operator (app-fn exp))
	   (declare (ignore op arg1))
	   (if fixity
	       (if (eq? associativity (fixity-associativity fixity))
		   (1+ (fixity-precedence fixity))
		   (fixity-precedence fixity))
	       10)))
	((is-type? 'lambda exp) 10)
	((is-type? 'let exp) 10)
	((is-type? 'if exp) 10)
	((is-type? 'case exp) 10)
	((pp-exp-list-section? exp) 10)
	((is-type? 'negate exp) 10)  ; hack, hack
	(else
	 0)))


;;; Determine whether a pp-exp-list is really a section -- the
;;; first or last exp in the list is really an infix op.

(define (pp-exp-list-section? object)
  (if (is-type? 'pp-exp-list object)
      (let ((exps  (pp-exp-list-exps object)))
	(or (infix-var-or-con? (car exps))
	    (infix-var-or-con? (list-ref exps (1- (length exps))))))
      '#f))

(define (infix-var-or-con? object)
  (or (and (is-type? 'var-ref object)
	   (var-ref-infix? object))
      (and (is-type? 'con-ref object)
	   (con-ref-infix? object))))

