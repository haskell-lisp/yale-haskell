;;; dump-flic.scm -- general dump functions for flic structures
;;;
;;; author :  Sandra Loosemore
;;; date   :  24 Feb 1993
;;;
;;;
;;; This stuff is used to write inline expansions to the interface file.
;;; 


(define-flic-walker dump-flic (object var-renamings))

(define (dump-flic-list objects var-renamings)
  (let ((result  '()))
    (dolist (o objects)
      (push (dump-flic o var-renamings) result))
    `(list ,@(nreverse result))))

(define (dump-flic-top object)
  (dump-flic object '()))


(define (make-temp-bindings-for-dump oldvars var-renamings)
  (let ((vars      '())
	(bindings  '()))
    (dolist (v oldvars)
      (let ((var  (def-name v))
	    (temp (gensym)))
	(push temp vars)
	(push `(,temp (create-temp-var ',var)) bindings)
	(push (cons v temp) var-renamings)))
    (setf bindings (nreverse bindings))
    (setf vars (nreverse vars))
    (values vars bindings var-renamings)))

(define-dump-flic flic-lambda (object var-renamings)
  (multiple-value-bind (vars bindings var-renamings)
      (make-temp-bindings-for-dump (flic-lambda-vars object) var-renamings)
    `(let ,bindings
       (make-flic-lambda
	 (list ,@vars)
	 ,(dump-flic (flic-lambda-body object) var-renamings)))
    ))

(define-dump-flic flic-let (object var-renamings)
  (multiple-value-bind (vars bindings var-renamings)
      (make-temp-bindings-for-dump (flic-let-bindings object) var-renamings)
    `(let ,bindings
       ,@(map (lambda (temp v)
		`(setf (var-value ,temp)
		       ,(dump-flic (var-value v) var-renamings)))
	      vars
	      (flic-let-bindings object))
       (make-flic-let
	 (list ,@vars)
	 ,(dump-flic (flic-let-body object) var-renamings)
	 ',(flic-let-recursive? object)))
    ))

(define-dump-flic flic-app (object var-renamings)
  `(make-flic-app
     ,(dump-flic (flic-app-fn object) var-renamings)
     ,(dump-flic-list (flic-app-args object) var-renamings)
     ',(flic-app-saturated? object)))

(define-dump-flic flic-ref (object var-renamings)
  (let* ((var    (flic-ref-var object))
	 (entry  (assq var var-renamings)))
    (if entry
	`(make-flic-ref ,(cdr entry))
	`(make-flic-ref ,(dump-object var)))))

(define-dump-flic flic-const (object var-renamings)
  (declare (ignore var-renamings))
  `(make-flic-const ',(flic-const-value object)))

(define-dump-flic flic-pack (object var-renamings)
  (declare (ignore var-renamings))
  `(make-flic-pack ,(dump-object (flic-pack-con object))))

(define-dump-flic flic-case-block (object var-renamings)
  `(make-flic-case-block
     ',(flic-case-block-block-name object)
     ,(dump-flic-list (flic-case-block-exps object) var-renamings)))

(define-dump-flic flic-return-from (object var-renamings)
  `(make-flic-return-from
     ',(flic-return-from-block-name object)
     ,(dump-flic (flic-return-from-exp object) var-renamings)))

(define-dump-flic flic-and (object var-renamings)
  `(make-flic-and
     ,(dump-flic-list (flic-and-exps object) var-renamings)))

(define-dump-flic flic-if (object var-renamings)
  `(make-flic-if
     ,(dump-flic (flic-if-test-exp object) var-renamings)
     ,(dump-flic (flic-if-then-exp object) var-renamings)
     ,(dump-flic (flic-if-else-exp object) var-renamings)))

(define-dump-flic flic-sel (object var-renamings)
  `(make-flic-sel
     ,(dump-object (flic-sel-con object))
     ,(flic-sel-i object)
     ,(dump-flic (flic-sel-exp object) var-renamings)))

(define-dump-flic flic-is-constructor (object var-renamings)
  `(make-flic-is-constructor
     ,(dump-object (flic-is-constructor-con object))
     ,(dump-flic (flic-is-constructor-exp object) var-renamings)))

(define-dump-flic flic-con-number (object var-renamings)
  `(make-flic-con-number
     ,(dump-object (flic-con-number-type object))
     ,(dump-flic (flic-con-number-exp object) var-renamings)))

(define-dump-flic flic-void (object var-renamings)
  (declare (ignore object var-renamings))
  `(make-flic-void))
    






	

    

