
;;; This has some diagnostic stuff

;;; This forces all delays in a structure

(define (force-all x)
  (cond ((delay? x)
	 (force-all (force x)))
	((pair? x)
	 (force-all (car x))
	 (force-all (cdr x)))
	((vector? x)
	 (dotimes (i (vector-length x))
            (force-all (vector-ref x i)))))
  x)

;;; This forces & removes all delays in a structure.

(define (remove-delays x)
  (cond ((delay? x)
	 (remove-delays (force x)))
	((pair? x)
	 (cons (remove-delays (car x))
	       (remove-delays (cdr x))))
	((vector? x)
	 (list->vector (map (function remove-delays) (vector->list x))))
	(else x)))

(define (delay? x)
  (and (pair? x)
       (or (eq? (car x) '#t)
	   (eq? (car x) '#f))))

