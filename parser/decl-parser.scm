;;; File: decl-parser           Author: John

(define (parse-decl)
  (let ((decl-type (find-decl-type)))
    (cond ((eq? decl-type 'signdecl)
	   (parse-signdecl))
	  ((eq? decl-type 'pat-or-op)
	   (parse-pat-or-op))
	  ((eq? decl-type 'fundef)
	   (parse-fundef))
	  ((eq? decl-type 'plus-def)
	   (parse-plus-def))
	  ((eq? decl-type 'annotation)
	   (make annotation-decls (annotations (parse-annotations)))))))

;;; This looks at the first tokens in a definition to determine it's type.
;;;   var (:: | ,)      - signdecl
;;;   var apat-start    - function definition
;;;   (var | _) +       - definition of infix +
;;;   anything alse     - pattern binding or infix definition

(define (find-decl-type)
  (let* ((saved-excursion (save-scanner-state))
	 (decl-type
	  (token-case
	   (var (scan-var)
		(token-case
		 ((\, \:\:) 'signdecl)
		 (apat-start 'fundef)
		 (+ 'plus-def)
		 (else 'pat-or-op)))
	   (_ (token-case
	       (+ 'plus-def)
	       (else 'pat-or-op)))
	   (begin-annotation 'annotation)
	   (else 'pat-or-op))))
    (restore-excursion saved-excursion)
    decl-type))

;;; These are the different flavors of decl parsers

(define (parse-signdecl)
 (save-parser-context
  (trace-parser signdecl
    (let ((vars (parse-signdecl-vars)))
      (require-token \:\:
		     (signal-missing-token "`::'" "signature declaration"))
      (let ((signature (parse-signature)))
	(make signdecl (vars vars) (signature signature)))))))
 
(define (parse-signdecl-vars)
  (token-case
   (var (let ((var (var->ast)))
	  (token-case (\, (cons var (parse-signdecl-vars)))
		      (else (list var)))))
   (else (signal-missing-token "<var>" "signature declaration"))))

(define (parse-pat-or-op)
  (trace-parser patdef
    (let* ((line-number (capture-current-line))
	   (pat (parse-pat)))
      (token-case
       (varop (parse-infix-def pat line-number))
       (else (add-rhs pat '() '#f line-number))))))

(define (parse-infix-def pat1 line-number)
  (let* ((op (make var-pat (var (varop->ast))))
	 (pat2 (parse-pat)))
	(add-rhs op (list pat1 pat2) '#t line-number)))

(define (parse-fundef)
 (trace-parser fundef
  (let* ((start-line (capture-current-line))
	 (fn (parse-apat))  ; must be a single variable
	 (args (parse-apat-list)))
    (add-rhs fn args '#f start-line))))

(define (parse-plus-def)
  (trace-parser plus-def
    (let* ((start-line (capture-current-line))
	   (var (parse-apat)))
      (parse-infix-def var start-line))))

(define (add-rhs pat args infix? start-line)
  (let* ((rhs (parse-rhs))
	 (decls (parse-where-decls))
	 (single (make single-fun-def
		       (args args)
		       (rhs-list rhs)
		       (where-decls decls)
		       (infix? infix?)))
	 (valdef (make valdef (lhs pat) (definitions (list single)))))
    (setf (ast-node-line-number single) start-line)
    (setf (ast-node-line-number valdef) start-line)
    valdef))

(define (parse-rhs)
  (token-case
   (= (let ((rhs (parse-exp)))
	(list (make guarded-rhs (guard (make omitted-guard)) (rhs rhs)))))
   (\| (parse-guarded-rhs))
   (else
    (signal-missing-token "`=' or `|'" "rhs of valdef"))))

(define (parse-guarded-rhs) ; assume just past |
 (trace-parser guard
  (let ((guard (parse-exp-i)))  ; 1.2 change
    (require-token = (signal-missing-token "`='" "guarded rhs"))
    (let* ((exp (parse-exp))
	   (res (make guarded-rhs (guard guard) (rhs exp))))
      (token-case
       (\| (cons res (parse-guarded-rhs)))
       (else (list res)))))))

(define (parse-where-decls)
  (token-case
   (|where|
    (parse-decl-list))
   (else '())))

(define (parse-decl-list)
  (start-layout (function parse-decl-list-1)))

(define (parse-decl-list-1 in-layout?)
  (token-case
   ((apat-start begin-annotation)
    (let ((decl (parse-decl)))
      (token-case
       (\; (decl-cons decl (parse-decl-list-1 in-layout?)))
       (else (close-layout in-layout?)
	     (list decl)))))
   (else
    (close-layout in-layout?)
    '())))

;;; This adds a new decl to a decl list.  Successive decls for the same fn
;;; are combined.

(define (decl-cons decl decl-list)
  (cond ((null? decl-list)
	 (list decl))
	(else (nconc (combine-decls decl (car decl-list)) (cdr decl-list)))))

(define (decl-push decl decl-stack)
  (cond ((null? decl-stack)
	 (list decl))
	(else (nconc (nreverse (combine-decls (car decl-stack) decl))
		     (cdr decl-stack)))))

(define (combine-decls decl1 decl2)
  (if (and (is-type? 'valdef decl1)
	   (is-type? 'valdef decl2)
	   (same-decl-var? (valdef-lhs decl1) (valdef-lhs decl2)))
      (if (eqv? (length (single-fun-def-args (car (valdef-definitions decl1))))
		(length (single-fun-def-args (car (valdef-definitions decl2)))))
	  (begin
	    (setf (valdef-definitions decl1)
		  (nconc (valdef-definitions decl1)
			 (valdef-definitions decl2)))
	    (list decl1))
	  (signal-multiple-definitions-arity-mismatch (valdef-lhs decl1)))
      (list decl1 decl2)))

(define (same-decl-var? pat1 pat2)
  (and (is-type? 'var-pat pat1)
       (is-type? 'var-pat pat2)
       (eq? (var-ref-name (var-pat-var pat1))
	    (var-ref-name (var-pat-var pat2)))))

(define (signal-multiple-definitions-arity-mismatch pat)
  (parser-error 'multiple-definitions-arity-mismatch
		"Definition of ~a does not match arity of previous definition."
		pat))
		   
	 
