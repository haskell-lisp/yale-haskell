;;; File: expr-parser           Author: John

(define (parse-exp)
 (trace-parser exp
   (parse-exp-0)))

(define (parse-exp-0)  ;; This picks up expr type signatures
  (let ((exp (parse-exp-i)))
    (token-case
     (\:\: (let ((signature (parse-signature)))
	     (make exp-sign (exp exp) (signature signature))))
   (else exp))))

(define (parse-exp-i)  ;; This collects a list of exps for later prec parsing
  (let ((exps (parse-infix-exps)))
    (if (null? (cdr exps))
	(car exps)
	(make pp-exp-list (exps exps)))))

(define (parse-infix-exps)
  (token-case
     (- (cons (make negate) (parse-infix-exps)))
     (\\ (list (parse-lambda)))
     (|let| (list (parse-let)))
     (|if| (list (parse-if)))
     (|case| (parse-possible-app (parse-case)))
     (else (let ((aexp (parse-aexp)))
	     (parse-possible-app aexp)))))

(define (parse-possible-app exp)
  (token-case
    (aexp-start
     (let ((exp2 (parse-aexp)))
      (parse-possible-app (make app (fn exp) (arg exp2)))))
    (varop
     (let ((varop (varop->ast)))
       (if (eq-token? '\))
	   (list exp varop)
	   `(,exp ,varop ,@(parse-infix-exps)))))
    (conop
     (let ((conop (conop->ast)))
       (if (eq-token? '\))
	   (list exp conop)
	   `(,exp ,conop ,@(parse-infix-exps)))))
    (else (list exp))))

(define (parse-lambda)
  (trace-parser lambda
   (save-parser-context
    (let ((pats (parse-apat-list)))
      (require-token -> (signal-missing-token "`->'" "lambda expression"))
      (let ((exp (parse-exp)))
	(make lambda (pats pats) (body exp)))))))

(define (parse-let)
  (trace-parser let
   (save-parser-context
    (let ((decls (parse-decl-list)))
      (require-token |in| (signal-missing-token "`in'" "let expression"))
      (let ((exp (parse-exp)))
	(make let (decls decls) (body exp)))))))

(define (parse-if)
  (trace-parser if
   (save-parser-context
    (let ((test-exp (parse-exp)))
      (require-token |then| (signal-missing-token "`then'" "if expression"))
      (let ((then-exp (parse-exp)))
	(require-token |else| (signal-missing-token "`else'" "if expression"))
	(let ((else-exp (parse-exp)))
	  (make if (test-exp test-exp)
		   (then-exp then-exp)
		   (else-exp else-exp))))))))

(define (parse-case)
  (trace-parser case
   (save-parser-context
    (let ((exp (parse-exp)))
      (require-token |of| (signal-missing-token "`of'" "case expression"))
      (let ((alts (start-layout (function parse-alts))))
	(make case (exp exp) (alts alts)))))))

(define (parse-alts in-layout?)
  (token-case
    (pat-start
     (let ((alt (parse-alt)))
       (token-case
	(\; (cons alt (parse-alts in-layout?)))
	(else (close-layout in-layout?)
	      (list alt)))))
    (else
     (close-layout in-layout?)
     '())))

(define (parse-alt)
 (trace-parser alt
  (let* ((pat (parse-pat))
	 (rhs-list (token-case
		    (-> (let ((exp (parse-exp)))
			  (list (make guarded-rhs (guard (make omitted-guard))
				                  (rhs exp)))))
		    (\| (parse-guarded-alt-rhs))
		    (else (signal-missing-token "`->' or `|'" "rhs of alt"))))
	 (decls (parse-where-decls)))
    (make alt (pat pat) (rhs-list rhs-list) (where-decls decls)))))

(define (parse-guarded-alt-rhs)
  (let ((guard (parse-exp)))
    (require-token -> (signal-missing-token "`->'" "alt"))
    (let* ((exp (parse-exp))
	   (res (make guarded-rhs (guard guard) (rhs exp))))
      (token-case
       (\| (cons res (parse-guarded-alt-rhs)))
       (else (list res))))))

(define (parse-aexp)
 (trace-parser aexp
  (token-case
    (var (save-parser-context (var->ast)))
    (con (save-parser-context (con->ast)))
    (literal (literal->ast))
    (\(
     (token-case
       (\) (**con/def (core-symbol "UnitConstructor")))
       ((no-advance -) (parse-exp-or-tuple))
       (varop
	(let ((varop (varop->ast)))
	  (make-right-section varop)))
       (conop
	(let ((conop (conop->ast)))
	  (make-right-section conop)))
       (else
	(parse-exp-or-tuple))))
    (\[
     (token-case
      (\] (make list-exp (exps '())))
      (else
       (let ((exp (parse-exp)))
        (token-case
         (\, (let ((exp2 (parse-exp)))
	       (token-case
		 (\] (make list-exp (exps (list exp exp2))))
		 (\.\. (token-case
			 (\] (make sequence-then (from exp) (then exp2)))
			 (else
			   (let ((exp3 (parse-exp)))
			     (require-token
			       \]
			       (signal-missing-token
				 "`]'" "sequence expression"))
			     (make sequence-then-to (from exp) (then exp2)
				                    (to exp3))))))
		 (else
		  (make list-exp
			(exps `(,exp ,exp2 ,@(parse-exp-list))))))))
	 (\.\. (token-case
		 (\] (make sequence (from exp)))
		 (else
		  (let ((exp2 (parse-exp)))
		    (require-token
		      \]
		      (signal-missing-token "`]'" "sequence expression"))
		    (make sequence-to (from exp) (to exp2))))))
	 (\] (make list-exp (exps (list exp))))
	 (\| (parse-list-comp exp))
	 (else
	  (signal-invalid-syntax
	    "a list, sequence, or list comprehension")))))))
    (else
     (signal-invalid-syntax "an aexp")))))

(define (make-right-section op)
  (let ((exps (parse-infix-exps)))
    (token-case
     (\) (make pp-exp-list (exps (cons op exps))))
     (else (signal-missing-token "`)'" "right section expression")))))

(define (parse-exp-list)
  (token-case
   (\] '())
   (\, (let ((exp (parse-exp))) (cons exp (parse-exp-list))))
   (else (signal-missing-token "`]' or `,'" "list expression"))))

(define (parse-exp-or-tuple)
  (let ((exp (parse-exp)))
    (token-case
     (\) exp)  ; Note - sections ending in an op are parsed elsewhere
     (else (make-tuple-cons (cons exp (parse-tuple-exp)))))))

(define (parse-tuple-exp)
  (token-case
   (\) '())
   (\, (let ((exp (parse-exp))) (cons exp (parse-tuple-exp))))
   (else (signal-missing-token
	  "`)' or `,'" "tuple or parenthesized expression"))))

;;; List comprehensions

;;; Assume | has been consumed

(define (parse-list-comp exp)
 (save-parser-context 
  (let ((quals (parse-qual-list)))
    (make list-comp (exp exp) (quals quals)))))

(define (parse-qual-list)
  (let ((qual (parse-qual)))
    (token-case
      (\, (cons qual (parse-qual-list)))
      (\] (list qual))
      (else (signal-missing-token "`]' or `,'" "list comprehension")))))

(define (parse-qual)
 (trace-parser qual
  (save-parser-context 
   (let* ((saved-excursion (save-scanner-state))
	  (is-gen? (and (scan-pat) (eq-token? '<-))))
    (restore-excursion saved-excursion)
    (cond (is-gen?
	   (let ((pat (parse-pat)))
	     (advance-token) ; past the <-
	     (let ((exp (parse-exp)))
	       (make qual-generator (pat pat) (exp exp)))))
	  (else
	   (let ((exp (parse-exp)))
	     (make qual-filter (exp exp)))))))))

(define (make-tuple-cons args)
  (let ((tuple-con (**con/def (tuple-constructor (length args)))))
    (**app/l tuple-con args)))
