;;; File: type-parser        Author: John

(define (parse-type)
  (let ((type (parse-btype)))
    (token-case
      (->
       (**tycon/def (core-symbol "Arrow") (list type (parse-type))))
      (else type))))

(define (parse-btype)
  (token-case
   (tycon (let* ((tycon (tycon->ast))
		 (tycon-args (parse-atype-list)))
	    (setf (tycon-args tycon) tycon-args)
	    tycon))
   (else
    (parse-atype))))

(define (parse-atype-list)
  (token-case
    (atype-start
     (let ((atype (parse-atype)))
       (cons atype (parse-atype-list))))
    (else '())))

(define (parse-atype)
  (token-case
    (tyvar (tyvar->ast))
    (tycon (tycon->ast))
    (\( (token-case
	  (\) (**tycon/def (core-symbol "UnitType") '()))
	  (else
	    (let ((type (parse-type)))
	      (token-case
	       (\) type)
	       (\, (let ((types  (cons type (parse-type-list))))
		     (**tycon/def (tuple-tycon (length types)) types)))
	       (else
		(signal-missing-token "`)' or `,'" "type expression")))))))
    (\[ (let ((type (parse-type)))
	  (require-token \] (signal-missing-token "`]'" "type expression"))
	  (**tycon/def (core-symbol "List") (list type))))
    (else
     (signal-invalid-syntax "an atype"))))

(define (parse-type-list)
  (let ((type (parse-type)))
    (token-case (\, (cons type (parse-type-list)))
		(\) (list type))
		(else (signal-missing-token "`)' or `,'" "type expression")))))

;;; This is used to determine whether a type is preceded by a context

(define (has-optional-context?)
  (let* ((saved-excursion (save-scanner-state))
	 (res (token-case
		(conid	
		 (token-case
		  (varid (eq-token? '=>))
		  (else '#f)))
		(\( (scan-context))
		(else '#f))))
    (restore-excursion saved-excursion)
    res))

(define (scan-context)
  (token-case
    (conid
     (token-case
       (varid
	 (token-case
	   (\) (eq-token? '=>))
	   (\, (scan-context))
	   (else '#f)))
       (else '#f)))
    (else '#f)))

(define (parse-context)
 (let ((contexts (token-case
	           (tycon
		    (list (parse-single-context)))
		   (\( (parse-contexts))
		   (else
		    (signal-invalid-syntax "a context")))))
   (require-token => (signal-missing-token "`=>'" "context"))
   contexts))

(define (parse-single-context)
  (let ((class (class->ast)))
    (token-case
      (tyvar
       (let ((tyvar (token->symbol)))
	 (make context (class class) (tyvar tyvar))))
      (else (signal-missing-token "<tyvar>" "class assertion")))))

(define (parse-contexts)
  (token-case
    (tycon (let ((context (parse-single-context)))
	      (token-case
		(\, (cons context (parse-contexts)))
		(\) (list context))
		(else (signal-missing-token "`)' or `,'" "context")))))
    (else (signal-missing-token "<tycon>" "class assertion"))))

(define (parse-optional-context)
  (if (has-optional-context?)
      (parse-context)
      '()))

(define (parse-signature)
  (let* ((contexts (parse-optional-context))
	 (type (parse-type)))
    (make signature (context contexts) (type type))))


		 