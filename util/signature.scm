;;; This file handles the scoping and error checking of signatures.

;;; Possible errors:
;;;  Wrong arity in a tycon
;;;  Ambiguous context

;;; Other errors may be present; these are detected at a higher level.
;;; The list of variables used in the signature is returned.

(define (resolve-signature signature)
  (with-slots signature (context type) signature
    (let ((tyvars (resolve-type type)))
      (resolve-signature-aux tyvars context)
      tyvars)))

(define (resolve-signature-aux tyvars context)
  (dolist (ctxt context)
    (with-slots context (class tyvar) ctxt
      (when (not (memq tyvar tyvars))
	(signal-ambiguous-context tyvar))
      (resolve-class class))))

(define (resolve-type type)
  (resolve-type-1 type '()))

(define (resolve-type-1 type vars)
  (cond ((tyvar? type)
	 (cons (tyvar-name type) vars))
	(else
	 (resolve-tycon type)
	 (with-slots tycon (name def args) type
	   (when (not (eq? def *undefined-def*))
	     (if (eqv? (tycon-def-arity def) -1)
		 (setf (tycon-def-arity def) (length args))
		 (when (not (eqv? (length args) (tycon-def-arity def)))
		     (signal-tycon-arity name type))))
	   (resolve-type/list args vars)))))

(define (resolve-type/list args vars)
  (if (null? args)
      vars
      (resolve-type/list (cdr args) (resolve-type-1 (car args) vars))))

;;; This returns the names of the tyvars in a simple tycon

(define (simple-tyvar-list simple)
  (remember-context simple
    (let* ((res (map (lambda (x) (tyvar-name x)) (tycon-args simple)))
	   (dups (find-duplicates res)))
      (when (not (null? dups))
	(signal-non-linear-type-vars simple))
      res)))

;;; This is used to build the class dictionary signature.

(define (substitute-tyvar type tyvar new)
  (cond ((tyvar? type)
	 (if (eq? (tyvar-name type) tyvar)
	     new
	     (**tyvar (tyvar-name type))))
	((tycon? type)
	 (with-slots tycon (name def args) type
	   (make tycon (name name) (def def)
		       (args (map (lambda (x) (substitute-tyvar x tyvar new))
				  args)))))
	(else
	 (**signature (signature-context type)
		      (substitute-tyvar (signature-type type) tyvar new)))))



;;; Error signalling routines

(define (signal-ambiguous-context tyvar)
  (phase-error 'ambiguous-context
    "~a is referenced in a context, but is not bound as a type variable."
    tyvar))

(define (signal-tycon-arity name type)
  (phase-error 'tycon-arity
    "The wrong number of arguments are supplied to the constructor ~a~%~
     in the type ~a."
    name type))


(define (signal-non-linear-type-vars simple)
  (phase-error 'non-linear-type-vars
    "There are duplicate type variables in ~s."
    simple))

