;; these primitives support arbitrary sized tuples.

(define (prim.tupleSize x)
  (vector-length x))

(define (prim.tupleSel tuple i n)
 (force
  (if (eqv? n 2)
      (if (eqv? i 0)
	  (car tuple)
	  (cdr tuple))
      (vector-ref tuple i))))

(define (prim.list->tuple l)
  (let ((l (haskell-list->list/non-strict l)))
    (if (null? (cddr l))
	(cons (car l) (cadr l))
	(list->vector l))))

(define (haskell-list->list/non-strict l)
  (if (null? l)
      '()
      (cons (car l)
	    (haskell-list->list/non-strict (force (cdr l))))))

(define (prim.dict-sel dicts i)
  (force (vector-ref dicts i)))

;;; These generate dictionaries.

(define-local-syntax (create-dict dicts vars other-dicts)
  `(let ((dict-vector (box (list->vector ,dicts))))
     (make-tuple
       ,@(map (lambda (v)
		`(delay (funcall (dynamic ,v) dict-vector)))
	   vars)
       ,@(map (lambda (sd)
		`(delay (,(car sd)
			 (map (lambda (d)
			       (tuple-select ,(cadr sd) ,(caddr sd) (force d)))
			      ,dicts))))
	      other-dicts))))

(define prim.tupleEqdict
  (lambda dicts
    (tupleEqDict/l dicts)))

(define (tupleEqDict/l dicts)
  (create-dict dicts
     (|PreludeTuple:tupleEq| |PreludeTuple:tupleNeq|)
     ()))

(define prim.tupleOrdDict
 (lambda dicts
   (tupleOrdDict/l dicts)))

(define (tupleOrdDict/l d)
  (create-dict d
   (|PreludeTuple:tupleLe| |PreludeTuple:tupleLeq|
    |PreludeTuple:tupleGe| |PreludeTuple:tupleGeq|
    |PreludeTuple:tupleMax| |PreludeTuple:tupleMin|)
   ((tupleEqDict/l 7 6))))

(define prim.tupleIxDict
 (lambda dicts
   (create-dict dicts
      (|PreludeTuple:tupleRange| |PreludeTuple:tupleIndex|
       |PreludeTuple:tupleInRange|)
      ((tupleEqDict/l 6 3) (tupleTextDict/l 6 4) (tupleOrdDict/l 6 5)))))

(define prim.tupleTextDict
 (lambda dicts
   (tupleTextDict/l dicts)))

(define (tupleTextDict/l d)
  (create-dict d
     (|PreludeTuple:tupleReadsPrec| |PreludeTuple:tupleShowsPrec|
      |PreludeTuple:tupleReadList| |PreludeTuple:tupleShowList|)
     ()))

(define prim.tupleBinaryDict
 (lambda dicts
   (create-dict dicts
    (|PreludeTuple:tupleReadBin| |PreludeTuple:tupleShowBin|)
    ())))

