;;; ----------------------------------------------------------------
;;;  Text
;;; ----------------------------------------------------------------

(define (text-fns algdata suppress-reader?)
  (let ((print+read
	 (cond ((algdata-enum? algdata)
		(text-enum-fns algdata))
	       (else
		(text-general-fns algdata)))))
    (when suppress-reader?
      (setf print+read (list (car print+read))))
    print+read))

(define (text-enum-fns algdata)
  (list
   (**define '|showsPrec| '(|d| |x|)
      (**case/con algdata (**var '|x|)
		  (lambda (con vars)
		     (declare (ignore vars))
		     (**showString (**string (con-string con))))))
   (**define '|readsPrec| '(|d| |str|)
     (**listcomp
      (**var '|s|)
      (list
       (**gen '(tuple |tok| |rest|) (**lex (**var '|str|)))
       (**gen '|s|
	      (**case (**var '|tok|)
		      `(,@(map (lambda (con)
				 (**alt/simple
				  (**pat (con-string con))
				  (**list (**tuple2 (**con/def con)
						    (**var '|rest|)))))
			       (algdata-constrs algdata))
			,(**alt/simple (**pat '_) (**null))))))))))

;;; This has been hacked to split up the read function for large
;;; data types to avoid choking the lisp compiler.

(define (text-general-fns algdata)
 (let ((split-fn-def? (> (algdata-n-constr algdata) 6)))  ;; pretty arbitrary!
  (list
   (**define '|showsPrec| '(|d| |x|)
       (**case/con algdata (**var '|x|)
	  (lambda (con vars)
	    (if (con-infix? con)
		(show-infix con vars)
		(show-prefix con vars)))))
   (**define '|readsPrec| '(|d| |str|)
     (**append/l
      (map (lambda (con)
	     (cond ((con-infix? con)
		    (read-infix con))
		   (else
		    (read-prefix con split-fn-def?))))
		 (algdata-constrs algdata)))))))

(define (show-infix con vars)
  (multiple-value-bind (p lp rp) (get-con-fixity con)
    (**showParen
     (**< (**Int p) (**var '|d|))
     (**dot (**showsPrec (**int lp) (**var (car vars)))
	    (**showString
	      (**string (string-append " " (con-string con) " ")))
	    (**showsPrec (**int rp) (**var (cadr vars)))))))

(define (show-prefix con vars)
  (**showParen
   (**<= (**int 10) (**var '|d|))
   (**dot/l (**showString (**string (con-string con)))
	    (show-fields vars))))

(define (show-fields vars)
  (if (null? vars)
      '()
      `(,(**space) ,(**showsPrec (**int 10) (**var (car vars)))
	,@(show-fields (cdr vars)))))

(define (read-infix con)
  (multiple-value-bind (p lp rp) (get-con-fixity con)
    (**let
     (list
      (**define '|readVal| '(|r|) 
	 (**listcomp
	  (**tuple2 (**app (**con/def con) (**var '|u|) (**var '|v|))
		    (**var '|s2|))
	  (list
	   (**gen '(tuple |u| |s0|)
		  (**readsPrec (**int lp) (**var '|r|)))
	   (**gen `(tuple ,(con-string con) |s1|)
		  (**lex (**var '|s0|)))
	   (**gen '(tuple |v| |s2|)
		  (**readsprec (**int rp) (**var '|s1|)))))))
     (**readParen (**< (**int p) (**var '|d|))
		  (**var '|readVal|) (**var '|str|)))))

(define (read-prefix con split?)
  (let ((res (read-prefix-1 con)))
    (if (not split?)
	res
	(dynamic-let ((*module-name* (def-module con)))
	 (dynamic-let ((*module* (table-entry *modules* *module-name*)))
  	  (let* ((alg (con-alg con))
		 (fn (make-new-var
		      (string-append (symbol->string (def-name alg))
				     "/read-"
				     (remove-con-prefix
				      (symbol->string (def-name con))))))
		 (new-code (**app (**var/def fn) (**var '|str|) (**var '|d|)))
		 (def (**define fn '(|str| |d|) res)))
	  (setf (module-decls *module*) (cons def (module-decls *module*)))
	  new-code))))))

(define (read-prefix-1 con)
  (let* ((arity (con-arity con))
	 (vars (temp-vars "x" arity))
	 (svars (cons '|rest| (temp-vars "s" arity))))
    (**let
     (list
      (**define '|readVal| '(|r|) 
        (**listcomp
	 (**tuple2 (**app/l (**con/def con) (map (function **var) vars))
		   (**var (car (reverse svars))))
	 (cons
	  (**gen `(tuple ,(con-string con) |rest|)
		 (**lex (**var '|r|)))
	  (read-fields vars svars (cdr svars))))))
     (**readParen (**< (**int 9) (**var '|d|))
		  (**var '|readVal|) (**var '|str|)))))

(define (read-fields vars s0 s1)
  (if (null? vars)
      '()
      (cons
       (**gen `(tuple ,(car vars) ,(car s1))
	      (**readsprec (**int 10) (**var (car s0))))
       (read-fields (cdr vars) (cdr s0) (cdr s1)))))


;;; ----------------------------------------------------------------
;;;  Binary
;;; ----------------------------------------------------------------

(define (binary-fns algdata)
 (let ((res
  (cond ((algdata-enum? algdata)
	 (binary-enum-fns algdata))
	((algdata-tuple? algdata)
	 (binary-tuple-fns algdata))
	(else
	 (binary-general-fns algdata)))))
;   (dolist (x res)
;       (fresh-line)
;       (pprint x))
   res))


(define (binary-enum-fns algdata)
  (list
    (**define '|showBin| '(|x| |b|)
	(**showBinInt (**con-number (**var '|x|) algdata) (**var '|b|)))
    (**define '|readBin| '(|b|)
      (**let
       (list
	(**define '(tuple |n| |b1|) '()
	   (**readBinSmallInt
	    (**var '|b|)
	    (**int (1- (algdata-n-constr algdata))))))
        (**tuple2
	 (**case/int algdata (**var '|n|)
	       (lambda (con)
		 (**con/def con)))
	 (**var '|b1|))))))

(define (binary-tuple-fns algdata)
  (let* ((con (tuple-con algdata))
	 (arity (con-arity con))
	 (vars (temp-vars "v" arity)))
    (list
      (**define '|showBin| `((,con ,@vars) |b|)
	  (show-binary-body vars '|b|))
      (**define '|readBin| '(|b|)
	  (read-binary-body con)))))

(define (show-binary-body vars b)
  (**foldr (lambda (new-term prev-terms)
	       (**showBin new-term prev-terms))
	   (map (function **var) vars)
	   (**var b)))

(define (read-binary-body con)
  (let* ((arity (con-arity con))
	 (vars (temp-vars "v" arity))
	 (bvars (cons '|b| (temp-vars "b" arity))))
    (**let
     (map (lambda (v b nb)
	    (**define `(tuple ,v ,nb) '()
		      (**readBin (**var b))))
	  vars bvars (cdr bvars))
     (**tuple2
      (**app/l (**con/def con)
	       (map (function **var) vars))
      (**var (car (reverse bvars)))))))

(define (binary-general-fns algdata)
  (list
    (**define '|showBin| '(|x| |b|)
      (**showBinInt
       (**con-number (**var '|x|) algdata)
       (**case/con algdata (**var '|x|)
	  (lambda (con vars)
	    (declare (ignore con))
	    (show-binary-body vars '|b|)))))
    (**define '|readBin| '(|bin|)
      (**let
       (list
	(**define '(tuple |i| |b|) '()
	 (**readBinSmallInt (**var '|bin|)
			    (**int (1- (algdata-n-constr algdata))))))
       (**case/int algdata (**var '|i|) (function read-binary-body))))))

(define (get-con-fixity con)
  (let ((fixity (con-fixity con)))
    (if (not (eq? fixity '#f))
	(let ((p (fixity-precedence fixity))
	      (a (fixity-associativity fixity)))
	  (values p (if (eq? a 'L) p (1+ p)) (if (eq? a 'R) p (1+ p))))
	(values 9 10 9))))
