;;; ----------------------------------------------------------------
;;;  Ix
;;; ----------------------------------------------------------------

(define (ix-fns algdata)
  (if (algdata-enum? algdata)
      (ix-fns/enum algdata)
      (ix-fns/tuple algdata)))

(define (ix-fns/enum algdata)
 (list 
   (**define '|range| '((tuple |l| |u|))
     (**let
      (list
       (**define '|cl| '() (**con-number (**var '|l|) algdata))
       (**define '|cu| '() (**con-number (**var '|u|) algdata)))
      (**if (**< (**var '|cu|) (**var '|cl|))
	    (**null)
	    (**take (**+ (**- (**var '|cu|) (**var '|cl|)) (**int 1))
		    (**drop (**var '|cl|)
		      (**list/l
		       (map (function **con/def)
			    (algdata-constrs algdata))))))))
   (**define '|index| '((tuple |l| |u|) |x|)
      (**- (**con-number (**var '|x|) algdata)
	   (**con-number (**var '|l|) algdata)))
   (**define '|inRange| '((tuple |l| |u|) |x|)
      (**and (**<= (**con-number (**var '|l|) algdata)
		   (**con-number (**var '|x|) algdata))
	     (**<= (**con-number (**var '|x|) algdata)
		   (**con-number (**var '|u|) algdata))))))

(define (ix-fns/tuple algdata)
  (let* ((con (tuple-con algdata))
	 (arity (con-arity con))
	 (llist (temp-vars "l" arity))
	 (ulist (temp-vars "u" arity))
	 (ilist (temp-vars "i" arity)))
   (list
    (**define '|range| `((tuple (,con ,@llist) (,con ,@ulist)))
      (**listcomp (**app/l (**con/def con) (map (function **var) ilist))
		  (map (lambda (iv lv uv)
			  (**gen iv
			       (**app (**var '|range|)
				      (**tuple2 (**var lv)
						(**var uv)))))
			ilist llist ulist)))
    (**define '|index| `((tuple (,con ,@llist) (,con ,@ulist))
			 (,con ,@ilist))
	  (index-body (reverse ilist) (reverse llist) (reverse ulist)))
    (**define '|inRange| `((tuple (,con ,@llist) (,con ,@ulist))
			   (,con ,@ilist))
		(inrange-body ilist llist ulist)))))

(define (index-body is ls us)
  (let ((i1 (**app (**var '|index|)
		   (**tuple2 (**var (car ls)) (**var (car us)))
		   (**var (car is)))))
    (if (null? (cdr is))
	i1
	(**app (**var '|+|)
	       i1 (**app (**var '|*|)
			 (**1+ (**app (**var '|index|)
				      (**tuple2 (**var (car ls))
						(**var (car us)))
				      (**var (car us))))
			 (index-body (cdr is) (cdr ls) (cdr us)))))))

(define (inrange-body is ls us)
  (let ((i1 (**app (**var '|inRange|)
		   (**tuple2 (**var (car ls)) (**var (car us)))
		   (**var (car is)))))
    (if (null? (cdr is))
	i1
	(**app (**var/def (core-symbol "&&"))
	       i1
	       (inrange-body (cdr is) (cdr ls) (cdr us))))))

;;; ----------------------------------------------------------------
;;;  Enum
;;; ----------------------------------------------------------------

; Enum uses the Int methods since Enums are represented as Ints.

(define (enum-fns algdata)
  (list
   (**define '|enumFrom| '(|x|)
       (**let
	 (list
	  (**define '|from'| '(|x'|)
	      (**if (**> (**var '|x'|)
			 (**con-number (**con/def (last-con algdata)) algdata))
		    (**null)
		    (**cons (**var '|x'|)
			    (**app (**var '|from'|) (**1+ (**var '|x'|)))))))
	 (**cast (**app (**var '|from'|)
			(**con-number (**var '|x|) algdata)))))
   (**define '|enumFromThen| '(|x| |y|)
     (**let
      (list
       (**define '|step| '()
	 (**- (**con-number (**var '|y|) algdata)
	      (**con-number (**var '|x|) algdata)))
       (**define '|from'| '(|x'|)
	(**if (**or (**> (**var '|x'|)
			 (**con-number (**con/def (last-con algdata)) algdata))
		    (**< (**var '|x'|) (**int 0)))
	      (**null)
	      (**cons (**var '|x'|)
		      (**app (**var '|from'|)
			     (**+ (**var '|x'|) (**var '|step|)))))))
      (**cast (**app (**var '|from'|) (**con-number (**var '|x|) algdata)))))))

(define (last-con algdata)
  (car (reverse (algdata-constrs algdata))))

