;;; runtime-utils.scm -- basic runtime support
;;;
;;; author :  Sandra Loosemore
;;; date   :  9 Jun 1992
;;;
;;; This file contains definitions (beyond the normal mumble stuff)
;;; that is referenced directly in code built by the code generator.
;;; See backend/codegen.scm.
;;;



;;; (delay form)
;;;   returns a delay object with unevaluated "form".

(define-syntax (delay form)
  `(cons '#f (lambda () ,form)))


;;; (box form)
;;;   returns a delay object with evaluated "form".

(define-syntax (box form)
  (cond ((number? form)
	 `(quote ,(cons '#t form)))
	((and (pair? form) (eq? (car form) 'quote))
	 `(quote ,(cons '#t (cadr form))))
	(else
	 `(cons '#t ,form))))

(define-syntax (unbox form)
  `(cdr ,form))

(define-syntax (forced? form)
  `(car ,form))


;;; (force delay)
;;;   return the value of the delay object.

(define (force delay-object)
  (declare (type pair delay-object))
  (if (car delay-object)
      (cdr delay-object)
      (begin
        (let ((result  (funcall (cdr delay-object))))
	  (setf (car delay-object) '#t)
	  (setf (cdr delay-object) result)))))

;;; Inline version of the above.  Not good to use everywhere because
;;; of code bloat problems, but handy for helper functions.

(define-syntax (force-inline delay-object)
  (let ((temp1  (gensym))
	(temp2  (gensym)))
    `(let ((,temp1  ,delay-object))
       (declare (type pair ,temp1))
       (if (car ,temp1)
	   (cdr ,temp1)
	   (let ((,temp2  (funcall (cdr ,temp1))))
	     (setf (car ,temp1) '#t)
	     (setf (cdr ,temp1) ,temp2))))))


;;; (make-curried-fn opt-fn strictness)
;;; The basic idea is to compare the number of arguments received against
;;; the number expected.
;;; If the same, call the optimized entry point opt-fn.
;;; If more, apply the result of calling the optimized entry to the
;;;   leftover arguments.
;;; If less, make a closure that accepts the additional arguments.

(define (make-curried-fn opt-fn strictness)
  (lambda args
    (curried-fn-body '() args opt-fn strictness)))

(define (curried-fn-body previous-args args opt-fn strictness)
  (multiple-value-bind
      (saturated? actual-args leftover-args leftover-strictness)
      (process-curried-fn-args strictness args '())
    (setf actual-args (append previous-args actual-args))
    (if saturated?
	(if (null? leftover-args)
	    (apply opt-fn actual-args)
	    (apply (apply opt-fn actual-args) leftover-args))
	(lambda more-args
	  (curried-fn-body actual-args more-args opt-fn leftover-strictness)))
    ))

(define (process-curried-fn-args strictness args actual-args)
  (cond ((null? strictness)
	 ;; At least as many arguments as expected.
	 (values '#t (nreverse actual-args) args strictness))
	((null? args)
	 ;; Not enough arguments supplied.
  	 (values '#f (nreverse actual-args) args strictness))
	(else
	 ;; Process the next argument.
	 (if (car strictness)
	     (push (force-inline (car args)) actual-args)
	     (push (car args) actual-args))
 	 (process-curried-fn-args (cdr strictness) (cdr args) actual-args))
	))


;;; Special cases of the above.

(define (make-curried-fn-1-strict opt-fn)
  (lambda (arg1 . moreargs)
    (setf arg1 (force-inline arg1))
    (if (null? moreargs)
	(funcall opt-fn arg1)
	(apply (funcall opt-fn arg1) moreargs))))

(define (make-curried-fn-1-nonstrict opt-fn)
  (lambda (arg1 . moreargs)
    (if (null? moreargs)
	(funcall opt-fn arg1)
	(apply (funcall opt-fn arg1) moreargs))))


;;; Here's a similar helper function used for making data constructors.

(define (constructor-body previous-args args arity fn)
  (declare (type fixnum arity))
  (let ((n  (length args)))
    (declare (type fixnum n))
    (setf args (append previous-args args))
    (cond ((eqv? n arity)
	   (apply fn args))
	  ((< n arity)
	   (lambda more-args
	     (constructor-body args more-args (- arity n) fn)))
	  (else
	   (error "Too many arguments supplied to constructor.")))))


;;; Special case for cons constructor

(define (make-cons-constructor . args)
  (constructor-body '() args 2 (function cons)))


;;; (make-tuple-constructor arity)
;;;   return a function that makes an untagged data structure with "arity" 
;;;   slots.  "arity" is a constant.

(define-integrable *max-predefined-tuple-arity* 10)

(define (make-tuple-constructor-aux arity)
  (cond ((eqv? arity 0)
	 ;; Actually, should never happen -- this is the unit constructor
	 0)
	((eqv? arity 1)
	 (lambda args
	   (constructor-body '() args 2 (lambda (x) x))))
	((eqv? arity 2)
	 (lambda args
	   (constructor-body '() args 2 (function cons))))
	(else
	 (lambda args
	   (constructor-body '() args arity (function vector))))))

(define *predefined-tuple-constructors*
  (let ((result  '()))
    (dotimes (i *max-predefined-tuple-arity*)
      (push (make-tuple-constructor-aux i) result))
    (list->vector (nreverse result))))

(define-syntax (make-tuple-constructor arity)
  (declare (type fixnum arity))
  (if (< arity *max-predefined-tuple-arity*)
      `(vector-ref *predefined-tuple-constructors* ,arity)
      `(make-tuple-constructor-aux ,arity)))


;;; (make-tuple . args)
;;;   uncurried version of the above

(define-syntax (make-tuple . args)
  (let ((arity  (length args)))
    (cond ((eqv? arity 0)
	   ;; Actually, should never happen -- this is the unit constructor
	   0)
	  ((eqv? arity 1)
	   (car args))
	  ((eqv? arity 2)
	   `(cons ,@args))
	  (else
	   `(vector ,@args)))))


;;; (make-tagged-data-constructor n arity)
;;;   return a function that makes a data structure with tag "n" and
;;;   "arity" slots.

(define-integrable *max-predefined-tagged-data-tag* 10)
(define-integrable *max-predefined-tagged-data-arity* 10)

(define (make-tagged-data-constructor-aux n arity)
  (if (eqv? arity 0)
      (vector n)
      (lambda args
	(constructor-body (list n) args arity (function vector)))))

(define *predefined-tagged-data-constructors*
  (let ((result  '()))
    (dotimes (i *max-predefined-tagged-data-arity*)
      (let ((inner-result  '()))
	(dotimes (j *max-predefined-tagged-data-tag*)
	  (push (make-tagged-data-constructor-aux j i) inner-result))
	(push (list->vector (nreverse inner-result)) result)))
    (list->vector (nreverse result))))

(define-syntax (make-tagged-data-constructor n arity)
  (declare (type fixnum arity n))
  (if (and (< arity *max-predefined-tagged-data-arity*)
	   (< n *max-predefined-tagged-data-tag*))
      `(vector-ref (vector-ref *predefined-tagged-data-constructors* ,arity)
		   ,n)
      `(make-tagged-data-constructor-aux ,n ,arity)))


;;; (make-tagged-data n . args)
;;;   uncurried version of the above

(define-syntax (make-tagged-data n . args)
  `(vector ,n ,@args))


;;; (tuple-select arity i object)
;;;   extract component "i" from untagged "object"

(define-syntax (tuple-select arity i object)
  (cond ((eqv? arity 1)
	 object)
	((eqv? arity 2)
	 (if (eqv? i 0)
	     `(car ,object)
	     `(cdr ,object)))
	(else
	 `(vector-ref (the vector ,object) (the fixnum ,i)))))


;;; (tagged-data-select arity i object)
;;;   extract component "i" from tagged "object"

(define-syntax (tagged-data-select arity i object)
  (declare (ignore arity))
  `(vector-ref (the vector ,object) (the fixnum ,(1+ i))))


;;; (constructor-number object)
;;;   return the tag from "object"

(define-syntax (constructor-number object)
  `(vector-ref (the vector ,object) 0))

(define-syntax (funcall-force fn . args)
  (let* ((n    (length args))
	 (junk (assv n '((1 . funcall-force-1)
			 (2 . funcall-force-2)
			 (3 . funcall-force-3)
			 (4 . funcall-force-4)))))
    `(,(if junk (cdr junk) 'funcall-force-n) ,fn ,@args)))

(define (funcall-force-1 fn a1)
  (funcall (force-inline fn) a1))
(define (funcall-force-2 fn a1 a2)
  (funcall (force-inline fn) a1 a2))
(define (funcall-force-3 fn a1 a2 a3)
  (funcall (force-inline fn) a1 a2 a3))
(define (funcall-force-4 fn a1 a2 a3 a4)
  (funcall (force-inline fn) a1 a2 a3 a4))
(define-syntax (funcall-force-n fn . args)
  `(funcall (force ,fn) ,@args))


;;; (make-haskell-string string)
;;;   Converts a Lisp string lazily to a boxed haskell string (makes
;;;   a delay with a magic function).  Returns an unboxed result.

(define (make-haskell-string string)
  (declare (type string string))
  (let ((index   1)
	(size    (string-length string)))
    (declare (type fixnum index size))
    (cond ((eqv? size 0)
	   '())
	  ((eqv? size 1)
	   (cons (box (char->integer (string-ref string 0)))
		 (box '())))
	  (else
	   (letrec ((next-fn
		      (lambda ()
			(let ((ch  (char->integer (string-ref string index))))
			  (incf index)
			  (cons (box ch)
				(if (eqv? index size)
				    (box '())
				    (cons '#f next-fn)))))))
	     (cons (box (char->integer (string-ref string 0)))
		   (cons '#f next-fn))))
	  )))


;;; Similar, but accepts an arbitrary tail (which must be a delay object)

(define (make-haskell-string-tail string tail-delay)
  (declare (type string string))
  (let ((index   1)
	(size    (string-length string)))
    (declare (type fixnum index size))
    (cond ((eqv? size 0)
	   (force-inline tail-delay))
	  ((eqv? size 1)
	   (cons (box (char->integer (string-ref string 0)))
		 tail-delay))
	  (else
	   (letrec ((next-fn
		      (lambda ()
			(let ((ch  (char->integer (string-ref string index))))
			  (incf index)
			  (cons (box ch)
				(if (eqv? index size)
				    tail-delay
				    (cons '#f next-fn)))))))
	     (cons (box (char->integer (string-ref string 0)))
		   (cons '#f next-fn))))
	  )))


(define (haskell-string->string s)
  (let ((length  0))
    (declare (type fixnum length))
    (do ((s s (force (cdr s))))
	((null? s))
	(setf length (+ length 1)))
    (let ((result  (make-string length)))
      (declare (type string result))
      (do ((s s (unbox (cdr s)))
	   (i 0 (+ i 1)))
	  ((null? s))
	  (declare (type fixnum i))
	  (setf (string-ref result i) (integer->char (force (car s)))))
      result)))


(define (print-haskell-string s port)
   (do ((s1 s (force (cdr s1))))
       ((null? s1))
     (write-char (integer->char (force (car s1))) port)))

;;; This explicates the value returned by a proc (the IO () type).

(define (insert-unit-value x)
  (declare (ignore x))
  0)

;;; These handle list conversions

(define (haskell-list->list fn l)
  (if (null? l)
      '()
      (cons (funcall fn (force (car l))) 
	    (haskell-list->list fn (force (cdr l))))))

(define (list->haskell-list fn l)
  (if (null? l)
      '()
      (cons (box (funcall fn (car l)))
	    (box (list->haskell-list fn (cdr l))))))

(define (haskell-list->list/identity l)
  (if (null? l)
      '()
      (cons (force (car l))
	    (haskell-list->list/identity (force (cdr l))))))

(define (list->haskell-list/identity l)
  (if (null? l)
      '()
      (cons (box (car l))
	    (box (list->haskell-list/identity (cdr l))))))
