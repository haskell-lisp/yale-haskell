;;; ----------------------------------------------------------------
;;;  Eq
;;; ----------------------------------------------------------------

(define (Eq-fns algdata)
  (list
   (cond ((algdata-enum? algdata)
	  (**define '== '(|x| |y|)
		    (**== (**con-number (**var '|x|) algdata)
			  (**con-number (**var '|y|) algdata))))
	 (else
	  (**multi-define '== algdata
			  ;; For nullary constructors
			  (function **true)
			  ;; For unary constructors
			  (lambda (v1 v2)
			    (**== (funcall v1) (funcall v2)))
			  ;; For n-ary constructors
			  (lambda (v1 v2 bool)
			    (**and (**== (funcall v1) (funcall v2)) bool))
			  ;; The else clause in case the constructors do
			  ;; not match.
			  (if (algdata-tuple? algdata)
			      '#f
			      (function **false)))))))

;;; ----------------------------------------------------------------
;;;  Ord
;;; ----------------------------------------------------------------

(define (Ord-fns algdata)
  (list (ord-fn1 algdata '< (function **<))
	(ord-fn1 algdata '<= (function **<=))))

(define (Ord-fn1 algdata fn prim)
  (cond ((algdata-enum? algdata)
	 (**define fn '(|x| |y|)
		       (funcall prim (**con-number (**var '|x|) algdata)
				     (**con-number (**var '|y|) algdata))))
	((algdata-tuple? algdata)
	 (**multi-define fn algdata
		         (function **false)
			 (lambda (x y) (funcall prim (funcall x) (funcall y)))
			 (function combine-eq-<)
			 '#f))
	(else
	 (**define fn '(|x| |y|)
	   (**let
	    (list 
	     (**multi-define '|inner| algdata
			       (if (eq? fn '<) (function **false)
				               (function **true))
			       (lambda (x y)
				 (funcall prim (funcall x) (funcall y)))
			       (function combine-eq-<)
			       '#f)
	     (**define '|cx| '() (**con-number (**var '|x|) algdata))
	     (**define '|cy| '() (**con-number (**var '|y|) algdata)))
	    (**or (**< (**var '|cx|) (**var '|cy|))
		  (**and (**== (**var `|cx|) (**var '|cy|))
			 (**app (**var '|inner|)
				(**var '|x|)
				(**var '|y|)))))))))

(define (combine-eq-< v1 v2 rest)
  (**or (**< (funcall v1) (funcall v2))
	(**and (**== (funcall v1) (funcall v2))
	       rest)))

