;;; General macros for the Haskell compiler

(define-syntax (remember-context exp . body)
  (let ((temp  (gensym)))
    `(let ((,temp  ,exp))
       (dynamic-let ((*context* (if (ast-node-line-number ,temp)
				    ,temp 
				    (dynamic *context*))))
         ,@body))))

(define-syntax (maybe-remember-context exp . body)
  (let ((temp  (gensym)))
    `(let ((,temp  ,exp))
       (if (ast-node-line-number ,temp)
	   (dynamic-let ((*context* ,temp)) ,@body)
	   (begin ,@body)))))

(define-syntax (recover-errors error-value . body)
  (let ((local-handler (gensym)))
    `(let/cc ,local-handler
       (dynamic-let ((*recoverable-error-handler*
		       (lambda () (funcall ,local-handler ,error-value))))
         ,@body))))

;;; This is for iterating a list of contexts over a list of types.

(define-syntax (do-contexts cbinder tbinder . body)
  (let ((cvar (car cbinder))
	(cinit (cadr cbinder))
	(tvar (car tbinder))
	(tinit (cadr tbinder))
	(cv (gensym))
	(tv (gensym)))
    `(do ((,cv ,cinit (cdr ,cv))
	  (,tv ,tinit (cdr ,tv)))
	 ((null? ,cv))
       (let ((,tvar (car ,tv)))
	 (dolist (,cvar (car ,cv))
	   ,@body)))))

;; dolist for 2 lists at once.

(define-syntax (dolist2 a1 a2 . body)
  (let ((a1var (car a1))
	(a1init (cadr a1))
	(a2var (car a2))
	(a2init (cadr a2))
	(a1l (gensym))
	(a2l (gensym)))
    `(do ((,a1l ,a1init (cdr ,a1l))
	  (,a2l ,a2init (cdr ,a2l)))
	 ((null? ,a1l))
       (let ((,a1var (car ,a1l))
	     (,a2var (car ,a2l)))
	 ,@body))))

  