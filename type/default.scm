;;; This handles the default rule.

(define (maybe-default-ambiguous-tyvar type def module)
  (let ((classes (ntyvar-context type)))
   (and (not (null? classes)) ; this happens only during cleanup after an error
    (let ((non-standard? '#f)
	  (numeric? '#f))
      (dolist (class classes)
	(cond ((eq? (class-kind class) 'numeric)
	       (setf numeric? '#t))
	      ((not (eq? (class-kind class) 'standard))
	       (setf non-standard? '#t))))
      (cond ((or non-standard? (not numeric?))
	     (remember-context def
 	       (phase-error 'Non-defaultable-ambiguous-context
"An ambiguous context, ~A, cannot be defaulted.~%Ambiguity in call to ~A~%"
                  classes def))
	     '#f)
	    (else
	     (find-default-type type classes classes
			(tuple-2-2 (assq module *default-decls*)))))))))

(define (find-default-type tyvar classes all-classes defaults)
  (cond ((null? defaults)
	 (phase-error 'no-default-applies
	    "Ambiguous context: ~A~%No default applies.~%"
	    all-classes)
	 '#f)
	((null? classes)
	 (instantiate-tyvar tyvar (car defaults))
	 '#t)
	((type-in-class? (car defaults) (car classes))
	 (find-default-type tyvar (cdr classes) all-classes defaults))
	(else
	 (find-default-type tyvar all-classes all-classes (cdr defaults)))))
	 
(define (type-in-class? ntype class)
  (let* ((ntype (expand-ntype-synonym ntype))
	 (alg (ntycon-tycon ntype))
	 (inst (lookup-instance alg class)))
    (if (eq? inst '#f)
	'#f
	(let ((res '#t))
	  (do-contexts (c (instance-context inst)) (ty (ntycon-args ntype))
	    (when (not (type-in-class? ty c))
	      (setf res '#f)))
	  res))))
