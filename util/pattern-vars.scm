;;; This collects the vars bound in a pattern.

(define-walker collect-pattern-vars ast-td-collect-pattern-vars-walker)

(define (collect-pattern-vars x)
  (collect-pattern-vars-1 x '()))

(define (collect-pattern-vars-1 x vars-so-far)
  (call-walker collect-pattern-vars x vars-so-far))

(define (collect-pattern-vars/list l vars-so-far)
  (if (null? l)
      vars-so-far
      (collect-pattern-vars/list (cdr l)
		        (collect-pattern-vars-1 (car l) vars-so-far))))

(define-local-syntax (collect-pattern-vars-processor
		        slot type object-form accum-form)
  (let ((stype  (sd-type slot))
	(sname  (sd-name slot)))
    (cond ((eq? stype 'var-ref)
	   `(cons (struct-slot ',type ',sname ,object-form) ,accum-form))
	  ((eq? stype 'pattern)
	   `(collect-pattern-vars-1
	       (struct-slot ',type ',sname ,object-form)
	       ,accum-form))
	  ((equal? stype '(list pattern))
	   `(collect-pattern-vars/list
	       (struct-slot ',type ',sname ,object-form) ,accum-form))
	  (else
;	   (format '#t "Collect-pattern-vars: skipping slot ~A in ~A~%"
;		   sname
;		   type)
	   accum-form)
	  )))

(define-collecting-walker-methods collect-pattern-vars
  (as-pat irr-pat var-pat wildcard-pat const-pat plus-pat pcon list-pat
	  pp-pat-list pp-pat-plus pp-pat-negated)
  collect-pattern-vars-processor)
