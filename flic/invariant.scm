;;; invariant.scm -- look for invariant expressions
;;;
;;; author :  Sandra Loosemore
;;; date   :  12 Mar 1993
;;;
;;;
;;; The function flic-invariant? returns true if the expression is
;;; invariant with respect to a set of local variable bindings.

(define-flic-walker flic-invariant? (object local-bindings))

(define (flic-invariant-list? objects local-bindings)
  (if (null objects)
      '#t
      (and (flic-invariant? (car objects) local-bindings)
	   (flic-invariant-list? (cdr objects) local-bindings))))

(define-flic-invariant? flic-lambda (object local-bindings)
  (flic-invariant? (flic-lambda-body object)
		   (cons (flic-lambda-vars object) local-bindings)))

(define-flic-invariant? flic-let (object local-bindings)
  (let* ((bindings      (flic-let-bindings object))
	 (body          (flic-let-body object))
	 (recursive?    (flic-let-recursive? object))
	 (inner-stuff   (cons bindings local-bindings)))
    (and (flic-invariant-list? (map (function var-value) bindings)
			       (if recursive? inner-stuff local-bindings))
	 (flic-invariant? body inner-stuff))))

(define-flic-invariant? flic-app (object local-bindings)
  (and (flic-invariant? (flic-app-fn object) local-bindings)
       (flic-invariant-list? (flic-app-args object) local-bindings)))

(define-flic-invariant? flic-ref (object local-bindings)
  (let ((var  (flic-ref-var object)))
    (or (var-toplevel? var)
	(flic-local-var? var local-bindings))))

(define (flic-local-var? var local-bindings)
  (cond ((null? local-bindings)
	 '#f)
	((memq var (car local-bindings))
	 '#t)
	(else
	 (flic-local-var? var (cdr local-bindings)))))

(define-flic-invariant? flic-const (object local-bindings)
  (declare (ignore object local-bindings))
  '#t)

(define-flic-invariant? flic-pack (object local-bindings)
  (declare (ignore object local-bindings))
  '#t)

(define-flic-invariant? flic-case-block (object local-bindings)
  (flic-invariant-list? (flic-case-block-exps object) local-bindings))

(define-flic-invariant? flic-return-from (object local-bindings)
  (flic-invariant? (flic-return-from-exp object) local-bindings))

(define-flic-invariant? flic-and (object local-bindings)
  (flic-invariant-list? (flic-and-exps object) local-bindings))

(define-flic-invariant? flic-if (object local-bindings)
  (and (flic-invariant? (flic-if-test-exp object) local-bindings)
       (flic-invariant? (flic-if-then-exp object) local-bindings)
       (flic-invariant? (flic-if-else-exp object) local-bindings)))

(define-flic-invariant? flic-sel (object local-bindings)
  (flic-invariant? (flic-sel-exp object) local-bindings))

(define-flic-invariant? flic-is-constructor (object local-bindings)
  (flic-invariant? (flic-is-constructor-exp object) local-bindings))

(define-flic-invariant? flic-con-number (object local-bindings)
  (flic-invariant? (flic-con-number-exp object) local-bindings))

(define-flic-invariant? flic-void (object local-bindings)
  (declare (ignore object local-bindings))
  '#t)




    


