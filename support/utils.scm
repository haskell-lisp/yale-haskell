;;; utils.scm -- utility functions
;;;
;;; author :  Sandra Loosemore
;;; date   :  18 Nov 1991
;;;
;;; This file contains miscellaneous functions that are generally useful.
;;; If you find some missing feature from the base language, this is
;;; a good place to put it.  Common Lisp-style sequence functions are 
;;; an example of the sort of thing found here.


;;;=====================================================================
;;; Sequence functions
;;;=====================================================================

(define (vector-replace to-vec from-vec to start end)
  (declare (type fixnum to start end)
	   (type vector to-vec from-vec))
  (if (and (eq? to-vec from-vec)
	   (> to start))
      ;; Right shift in place
      (do ((from  (1- end) (1- from))
	   (to    (1- (+ to (- end start)))))
	  ((< from start) to-vec)
	  (declare (type fixnum from to))
	  (setf (vector-ref to-vec to) (vector-ref from-vec from))
	  (decf to))
      ;; Normal case, left-to-right
      (do ((from  start (1+ from)))
	  ((= from end) to-vec)
	  (declare (type fixnum from))
	  (setf (vector-ref to-vec to) (vector-ref from-vec from))
	  (incf to))))

(define (string-replace to-vec from-vec to start end)
  (declare (type fixnum to start end)
	   (type string to-vec from-vec))
  (if (and (eq? to-vec from-vec)
	   (> to start))
      ;; Right shift in place
      (do ((from  (1- end) (1- from))
	   (to    (1- (+ to (- end start)))))
	  ((< from start) to-vec)
	  (declare (type fixnum from to))
	  (setf (string-ref to-vec to) (string-ref from-vec from))
	  (decf to))
      ;; Normal case, left-to-right
      (do ((from  start (1+ from)))
	  ((= from end) to-vec)
	  (declare (type fixnum from))
	  (setf (string-ref to-vec to) (string-ref from-vec from))
	  (incf to))))

(define (string-fill string c start end)
  (declare (type fixnum start end)
	   (type string string)
	   (type char c))
  (do ((i start (1+ i)))
      ((= i end) string)
      (declare (type fixnum i))
      (setf (string-ref string i) c)))

(define (string-position c string start end)
  (declare (type fixnum start end)
	   (type string string)
	   (type char c))
  (cond ((= start end) '#f)
	((char=? (string-ref string start) c) start)
	(else
	 (string-position c string (1+ start) end))))

(define (string-position-not-from-end c string start end)
  (declare (type fixnum start end)
	   (type string string)
	   (type char c))
  (cond ((= start end) '#f)
	((not (char=? (string-ref string (setf end (1- end))) c))
	 end)
	(else
	 (string-position-not-from-end c string start end))))

(define (string-nreverse string start end)
  (declare (type fixnum start end)
	   (type string string))
  (do ((i start (1+ i))
       (j (1- end) (1- j)))
      ((not (< i j)) string)
      (declare (type fixnum i j))
    (let ((c (string-ref string i)))
      (setf (string-ref string i) (string-ref string j))
      (setf (string-ref string j) c))))


(define (string-starts? s1 s2)  ; true is s1 begins s2
  (and (>= (string-length s2) (string-length s1))
       (string=? s1 (substring s2 0 (string-length s1)))))


;;;=====================================================================
;;; Table utilities
;;;=====================================================================


(define (table->list table)
  (let ((l '()))
       (table-for-each
	(lambda (key val) (push (cons key val) l)) table)
       l))

(define (list->table l)
  (let ((table (make-table)))
     (dolist (p l)
	(setf (table-entry table (car p)) (cdr p)))
     table))



;;;=====================================================================
;;; Tuple utilities
;;;=====================================================================

;;; For future compatibility with a typed language, define 2 tuples with
;;; a few functions:  (maybe add 3 tuples someday!)

(define-integrable (tuple x y)
  (cons x y))

(define-integrable (tuple-2-1 x) (car x))  ; Flic-like notation
(define-integrable (tuple-2-2 x) (cdr x))

(define (map-tuple-2-1 f l)
  (map (lambda (x) (tuple (funcall f (tuple-2-1 x)) (tuple-2-2 x))) l))

(define (map-tuple-2-2 f l)
  (map (lambda (x) (tuple (tuple-2-1 x) (funcall f (tuple-2-2 x)))) l))


;;;=====================================================================
;;; List utilities
;;;=====================================================================

;;; This does an assq using the second half of the tuple as the key.

(define (rassq x l)
  (if (null? l)
      '#f
      (if (eq? x (tuple-2-2 (car l)))
	  (car l)
	  (rassq x (cdr l)))))

;;; This is an assoc with an explicit test

(define (assoc/test test-fn x l)
  (if (null? l)
      '#f
      (if (funcall test-fn x (tuple-2-1 (car l)))
	  (car l)
	  (assoc/test test-fn x (cdr l)))))




;;; Stupid position function works only on lists, uses eqv?

(define (position item list)
  (position-aux item list 0))

(define (position-aux item list index)
  (declare (type fixnum index))
  (cond ((null? list)
	 '#f)
	((eqv? item (car list))
	 index)
	(else
	 (position-aux item (cdr list) (1+ index)))
	))


;;; Destructive delete-if function

(define (list-delete-if f l)
  (list-delete-if-aux f l l '#f))

(define (list-delete-if-aux f head next last)
  (cond ((null? next)
	 ;; No more elements.
	 head)
	((not (funcall f (car next)))
	 ;; Leave this element and do the next.
	 (list-delete-if-aux f head (cdr next) next))
	(last
	 ;; Delete element from middle of list.
	 (setf (cdr last) (cdr next))
	 (list-delete-if-aux f head (cdr next) last))
	(else
	 ;; Delete element from head of list.
	 (list-delete-if-aux f (cdr next) (cdr next) last))))


;;; Same as the haskell function

(define (concat lists)
  (if (null? lists)
      '()
      (append (car lists) (concat (cdr lists)))))


;;; This is a quick & dirty list sort function.

(define (sort-list l compare-fn)
  (if (or (null? l) (null? (cdr l)))
      l
      (insert-sorted compare-fn (car l) (sort-list (cdr l) compare-fn))))

(define (insert-sorted compare-fn e l)
  (if (null? l)
      (list e)
      (if (funcall compare-fn e (car l))
	  (cons e l)
	  (cons (car l) (insert-sorted compare-fn e (cdr l))))))

(define (find-duplicates l)
  (cond ((null? l)
	 '())
	((memq (car l) (cdr l))
	 (cons (car l)
	       (find-duplicates (cdr l))))
	(else (find-duplicates (cdr l)))))

;;;  A simple & slow topsort routine.
;;;  Input:  A list of lists.  Each list is a object consed onto the
;;;          list of objects it preceeds.
;;;  Output: Two values: SORTED / CYCLIC & a list of either sorted objects
;;;                      or a set of components containing the cycle.

(define (topsort l)
  (let ((changed? '#t)
	(sorted '())
	(next '()))
    (do () ((not changed?) 
	    (if (null? next)
		(values 'sorted (nreverse sorted))
		(values 'cyclic (map (function car) next))))
      (setf changed? '#f)
      (setf next '())
      (dolist (x l)
        (cond ((topsort-aux (cdr x) sorted)
	       (push (car x) sorted)
	       (setf changed? '#t))
	      (else
	       (push x next))))
      (setf l next))))


;;; Returns true if x doesn't contain any elements that aren't in sorted.
;;; equivalent to (null? (set-intersection x sorted)), but doesn't cons
;;; and doesn't traverse the whole list in the failure case.

(define (topsort-aux x sorted)
  (cond ((null? x)
	 '#t)
	((memq (car x) sorted)
	 (topsort-aux (cdr x) sorted))
	(else
	 '#f)))

(define (set-intersection s1 s2)
  (if (null? s1)
      '()
      (let ((rest (set-intersection (cdr s1) s2)))
	(if (memq (car s1) s2)
	    (cons (car s1) rest)
	    rest))))

;;; remove s2 elements from s1

(define (set-difference s1 s2)
  (if (null? s1)
      '()
      (let ((rest (set-difference (cdr s1) s2)))
	(if (memq (car s1) s2)
	    rest
	    (cons (car s1) rest)))))


(define (set-union s1 s2)
  (if (null? s2)
      s1
      (if (memq (car s2) s1)
	  (set-union s1 (cdr s2))
	  (cons (car s2) (set-union s1 (cdr s2))))))


;;; Destructive list splitter

(define (split-list list n)
  (declare (type fixnum n))
  (let ((tail1  (list-tail list (1- n))))
    (if (null? tail1)
	(values list '())
	(let ((tail2  (cdr tail1)))
	  (setf (cdr tail1) '())
	  (values list tail2)))))


;;; Some string utils

(define (mem-string s l)
  (and (not (null? l)) (or (string=? s (car l))
			   (mem-string s (cdr l)))))

(define (ass-string k l)
  (cond ((null? l)
	 '#f)
	((string=? k (caar l))
	 (car l))
	(else
	 (ass-string k (cdr l)))))


;;;=====================================================================
;;; Syntax extensions
;;;=====================================================================

;;; The mlet macro combines let* and multiple-value-bind into a single
;;; syntax.

(define-syntax (mlet binders . body)
  (mlet-body binders body))

(define (mlet-body binders body)
  (if (null? binders)
      `(begin ,@body)
      (let* ((b (car binders))
	     (var (car b))
	     (init (cadr b))
	     (inner-body (mlet-body (cdr binders) body)))
	(if (pair? var)
	    (multiple-value-bind (new-vars ignore-decl)
				 (remove-underlines var)
	       `(multiple-value-bind ,new-vars
				     ,init ,@ignore-decl ,inner-body))
	    `(let ((,var ,init)) ,inner-body)))))

(define (remove-underlines vars)
  (if (null? vars)
      (values '() '())
      (multiple-value-bind (rest ignore-decl) (remove-underlines (cdr vars))
	(if (not (eq? (car vars) '_))
	    (values (cons (car vars) rest) ignore-decl)
	    (let ((var (gensym)))
	      (values (cons var rest)
		      `((declare (ignore ,var)) ,@ignore-decl)))))))




;;;=====================================================================
;;; Other utilities
;;;=====================================================================

(define (add-extension name ext)
  (assemble-filename (filename-place name) (filename-name name) ext))

(define (time-execution thunk)
  (let* ((start-time (get-run-time))
	 (res (funcall thunk))
	 (end-time (get-run-time)))
    (values res (- end-time start-time))))

(define (pprint-flatten code . maybe-port)
  (pprint-flatten-aux
    code
    (if (null? maybe-port) (current-output-port) (car maybe-port))))

(define (pprint-flatten-aux code port)
  (if (and (pair? code)
	   (eq? (car code) 'begin))
      (dolist (c (cdr code))
	(pprint-flatten-aux c port))
      (pprint*-aux code port)))

(define (print-flatten code port)
  (if (and (pair? code)
	   (eq? (car code) 'begin))
      (dolist (c (cdr code))
	(print-flatten c port))
      (begin
	(internal-write code port)
	(internal-newline port))))


;;; Like pprint, but print newline after instead of before.

(define (pprint* object . maybe-port)
  (pprint*-aux
    object
    (if (null? maybe-port) (current-output-port) (car maybe-port))))

(define (pprint*-aux object port)
  (dynamic-let ((*print-pretty*  '#t))
    (prin1 object port))
  (terpri port))

;;; This reads stuff from a string.  (Better error checks needed!)

(define (read-lisp-object str)
  (call-with-input-string str (lambda (port) (read port))))
