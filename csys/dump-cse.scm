;;; This file handles common subexpressions in the interface file.
;;; Common subexpressions are detected in two places: gtypes and strictness
;;; properties.

;;; Compressing strictness signatures

;;; A strictness is represented by a list of booleans.  We do two things to
;;; compress strictnesses: all lists less than *pre-defined-strictness-size*
;;; are pre-computed in a vector and the first *pre-defined-strictness-vars*
;;; vector elements are cached in global vars.  The strictness will dump as
;;; as either a global or as a vector reference into the vector.

(define (initialize-strictness-table)
  (setf (dynamic *pre-defined-strictness-table*)
	(make-vector (expt 2 (1+ (dynamic *pre-defined-strictness-size*)))))
  (setf (vector-ref *pre-defined-strictness-table* 1) '())
  (do ((i 1 (1+ i))
       (j 1 (* j 2))
       (k 2 (* k 2)))
      ((> i *pre-defined-strictness-size*))
    (do ((l 0 (1+ l)))
	((>= l j))
      (setf (vector-ref *pre-defined-strictness-table* (+ k l))
	    (cons '#f (vector-ref *pre-defined-strictness-table* (+ j l))))
      (setf (vector-ref *pre-defined-strictness-table* (+ k j l))
	    (cons '#t (vector-ref *pre-defined-strictness-table* (+ j l))))))
  (set-strictness-vars))

(define (strictness-table-ref x)
  (vector-ref (dynamic *pre-defined-strictness-table*) x))

(define (dump-strictness s)
  (if (null? s)
      ''()
      (dump-strictness-1 s s 0 0)))

(define (dump-strictness-1 s s1 n size)
  (if (null? s1)
      (if (> size *pre-defined-strictness-size*)
	  (dump-big-strictness (- size *pre-defined-strictness-size*) s)
	  (let ((k (+ n (expt 2 size))))
	    (if (< k *pre-defined-strictness-vars*)
		`(dynamic ,(vector-ref *pre-defined-strictness-names* k))
		`(strictness-table-ref ,k))))
      (dump-strictness-1 s (cdr s1) (+ (* 2 n) (if (car s1) 1 0)) (1+ size))))

(define (dump-big-strictness k s)
  (if (= k 0)
      (dump-strictness s)
      `(cons ',(car s)
	     ,(dump-big-strictness (1- k) (cdr s)))))

;;; This routine handles saving type signatures (gtypes).  
;;; common subexpressions are detected in two places: the type body
;;; and the the contexts.

(define (init-predefined-gtyvars)
  (setf *saved-gtyvars* (make-vector *num-saved-gtyvars*))
  (dotimes (i *num-saved-gtyvars*)
     (setf (vector-ref *saved-gtyvars* i) (**gtyvar i)))
  (setup-gtyvar-vars))

(define (init-cse-structs)
  (initialize-strictness-table)
  (init-predefined-gtyvars))

(define (save-cse-value v)
  (setf (vector-ref (dynamic *saved-cse-values*) (dynamic *cse-value-num*)) v)
  (incf (dynamic *cse-value-num*)))

(define (cse-init-code)
  (let* ((n (length *cse-objects*))
	 (init-code '()))
    (do ((i (1- n) (1- i))
	 (init *cse-objects* (cdr init)))
	((null? init))
      (push `(save-cse-value ,(car init)) init-code))
    `((setf *saved-cse-values* (make-vector ,n))
      (setf *cse-value-num* 0)
      ,@init-code)))

(define (remember-dumped-object init-code)
  (push init-code *cse-objects*)
  (incf *cse-object-num*)
  *cse-object-num*)

(define (cse-value-ref x)
  (vector-ref (dynamic *saved-cse-values*) x))

(define (cse-ref-code n)
  (cond ((eqv? n 0)
	 ''())
	((<= n *num-saved-gtyvars*)
	 `(dynamic ,(vector-ref *saved-gtyvar-varnames* (1- n))))
	(else
	 `(cse-value-ref ,(- n *num-saved-gtyvars* 1)))))

(define (dump-gtyvar g)
  (let ((n (gtyvar-varnum g)))
    (if (< n *num-saved-gtyvars*)
	(1+ n)
	(remember-dumped-object `(**gtyvar ,n)))))

(define (dump-context-list contexts)
  (if (null? contexts)
      0
      (let* ((rest (dump-context-list (cdr contexts)))
	     (classes (dump-class-list (car contexts)))
	     (t1 (assq/insert-l classes *gtype-class-index*))
	     (res (assq/insert rest (cdr t1))))
	  (if (eq? (cdr res) '#f)
	      (let ((z (remember-dumped-object
			`(cons ,(cse-ref-code classes) ,(cse-ref-code rest)))))
		(setf (cdr res) z)
		z)
	      (cdr res)))))

(define (dump-class-list classes)
  (if (null? classes)
      0
      (let* ((rest (dump-class-list (cdr classes)))
	     (class (dump-class/n (car classes)))
	     (t1 (assq/insert-l class *context-class-index*))
	     (res (assq/insert rest (cdr t1))))
	  (if (eq? (cdr res) '#f)
	      (let ((z (remember-dumped-object
			`(cons ,class ,(cse-ref-code rest)))))
		(setf (cdr res) z)
		z)
	      (cdr res)))))
	
(define (dump-gtype-1 g)
  (cond ((gtyvar? g)
	 (dump-gtyvar g))
	((ntyvar? g)
	 (dump-gtype-1 (prune g)))
	(else
	 (dump-gtycon g))))

(define (dump-gtycon g)
  (let* ((ty (ntycon-tycon g))
	 (tycon (if (algdata? ty) (dump-algdata/n ty) (dump-synonym/n ty)))
	 (l (dump-gtype-list (ntycon-args g)))
	 (t1 (assq/insert-l tycon *gtype-tycon-index*))
	 (res (assq/insert l (cdr t1))))
    (if (eq? (cdr res) '#f)
	(let ((z (remember-dumped-object
		  `(**ntycon ,tycon ,(cse-ref-code l)))))
	  (setf (cdr res) z)
	  z)
	(cdr res))))

(define (dump-gtype-list l)
  (if (null? l)
      0
      (let* ((g (dump-gtype-1 (car l)))
	     (rest (dump-gtype-list (cdr l)))
	     (t1 (assq/insert-l g *gtype-list-index*))
	     (res (assq/insert rest (cdr t1))))
	(if (eq? (cdr res) '#f)
	    (let ((z (remember-dumped-object
		      `(cons ,(cse-ref-code g)
			     ,(cse-ref-code rest)))))
	      (setf (cdr res) z)
	      z)
	    (cdr res)))))

(define (dump-gtype/cse g)
 (cse-ref-code
  (let* ((context (dump-context-list (gtype-context g)))
	 (type (dump-gtype-1 (gtype-type g)))
	 (t1 (assq/insert-l type *gtype-index*))
	 (res (assq/insert context (cdr t1))))
    (if (eq? (cdr res) '#f)
	(let ((z (remember-dumped-object
		      `(**gtype ,(cse-ref-code context)
				,(cse-ref-code type)))))
	      (setf (cdr res) z)
	      z)
	(cdr res)))))


