
;;; Some general utilities for dealing with annotations

;;; Lookup an annotation on a var

(define (lookup-annotation var aname)
  (lookup-annotation-1 (var-annotations var) aname))

(define (lookup-annotation-1 a aname)
  (if (null? a)
      '#f
      (if (eq? aname (annotation-value-name (car a)))
	  (car a)
	  (lookup-annotation-1 (cdr a) aname))))

;;; This parses a string denoting a strictness property into a list
;;; of booleans.   "S,N,S" -> (#t #f #t)

(define (parse-strictness str)
  (parse-strictness-1 str 0))

(define (parse-strictness-1 str i)
  (if (>= i (string-length str))
      (signal-bad-strictness-annotation str)
      (let* ((ch (char-downcase (string-ref str i)))
	     (s (cond ((char=? ch '#\s)
		       '#t)
		      ((char=? ch '#\n)
		       '#f)
		      (else
		       (signal-bad-strictness-annotation str)))))
	(cond ((eqv? (1+ i) (string-length str))
	       (list s))
	      ((char=? (string-ref str (1+ i)) '#\,)
	       (cons s (parse-strictness-1 str (+ i 2))))
	      (else
	       (signal-bad-strictness-annotation str))))))

(define (signal-bad-strictness-annotation str)
  (fatal-error 'bad-strictness "Bad strictness annotation: ~A~%" str))

