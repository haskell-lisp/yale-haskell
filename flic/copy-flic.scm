;;; copy-flic.scm -- general copy functions for flic structures
;;;
;;; author :  Sandra Loosemore
;;; date   :  23 Feb 1993
;;;
;;;


;;; The var-renamings argument is an a-list.  It's used to map local vars
;;; in the input expression to new, gensymed vars.

(define-flic-walker copy-flic (object var-renamings))

(define (copy-flic-list objects var-renamings)
  (let ((result  '()))
    (dolist (o objects)
      (push (copy-flic o var-renamings) result))
    (nreverse result)))


(define (copy-flic-top object)
  (copy-flic object '()))


(define-copy-flic flic-lambda (object var-renamings)
  (let ((new-vars  (map (lambda (v)
			  (let ((new  (copy-temp-var (def-name v))))
			    (push (cons v new) var-renamings)
			    (when (var-force-strict? v)
			      (setf (var-force-strict? new) '#t))
			    (init-flic-var new '#f '#f)))
			(flic-lambda-vars object))))
    (make-flic-lambda
      new-vars
      (copy-flic (flic-lambda-body object) var-renamings))))


;;; Hack to avoid concatenating multiple gensym suffixes.

(define (copy-temp-var sym)
  (if (gensym? sym)
      (let* ((string  (symbol->string sym))
	     (n       (string-length string))
	     (root    (find-string-prefix string 0 n)))
	(create-temp-var root))
      (create-temp-var sym)))

(define (find-string-prefix string i n)
  (declare (type string string) (type fixnum i n))
  (cond ((eqv? i n)
	 string)
	((char-numeric? (string-ref string i))
	 (substring string 0 i))
	(else
	 (find-string-prefix string (+ i 1) n))))


(define-copy-flic flic-let (object var-renamings)
  (let ((new-vars  (map (lambda (v)
			  (let ((new  (copy-temp-var (def-name v))))
			    (when (var-force-inline? v)
			      (setf (var-force-inline? new) '#t))
			    (push (cons v new) var-renamings)
			    new))
			(flic-let-bindings object))))
    (for-each
      (lambda (new old)
	(init-flic-var new (copy-flic (var-value old) var-renamings) '#f))
      new-vars
      (flic-let-bindings object))
    (make-flic-let
      new-vars
      (copy-flic (flic-let-body object) var-renamings)
      (flic-let-recursive? object))))

(define-copy-flic flic-app (object var-renamings)
  (make-flic-app
    (copy-flic (flic-app-fn object) var-renamings)
    (copy-flic-list (flic-app-args object) var-renamings)
    (flic-app-saturated? object)))

(define-copy-flic flic-ref (object var-renamings)
  (let* ((var   (flic-ref-var object))
	 (entry (assq var var-renamings)))
    (if entry
	(make-flic-ref (cdr entry))
	(make-flic-ref var))))   ; don't share structure


(define-copy-flic flic-const (object var-renamings)
  (declare (ignore var-renamings))
  (make-flic-const (flic-const-value object)))  ; don't share structure

(define-copy-flic flic-pack (object var-renamings)
  (declare (ignore var-renamings))
  (make-flic-pack (flic-pack-con object)))      ; don't share structure


;;; Don't have to gensym new block names; these constructs always
;;; happen in pairs.

(define-copy-flic flic-case-block (object var-renamings)
  (make-flic-case-block
    (flic-case-block-block-name object)
    (copy-flic-list (flic-case-block-exps object) var-renamings)))

(define-copy-flic flic-return-from (object var-renamings)
  (make-flic-return-from
    (flic-return-from-block-name object)
    (copy-flic (flic-return-from-exp object) var-renamings)))

(define-copy-flic flic-and (object var-renamings)
  (make-flic-and
    (copy-flic-list (flic-and-exps object) var-renamings)))

(define-copy-flic flic-if (object var-renamings)
  (make-flic-if
    (copy-flic (flic-if-test-exp object) var-renamings)
    (copy-flic (flic-if-then-exp object) var-renamings)
    (copy-flic (flic-if-else-exp object) var-renamings)))

(define-copy-flic flic-sel (object var-renamings)
  (make-flic-sel
    (flic-sel-con object)
    (flic-sel-i object)
    (copy-flic (flic-sel-exp object) var-renamings)))

(define-copy-flic flic-is-constructor (object var-renamings)
  (make-flic-is-constructor
    (flic-is-constructor-con object)
    (copy-flic (flic-is-constructor-exp object) var-renamings)))

(define-copy-flic flic-con-number (object var-renamings)
  (make-flic-con-number
    (flic-con-number-type object)
    (copy-flic (flic-con-number-exp object) var-renamings)))

(define-copy-flic flic-void (object var-renamings)
  (declare (ignore object var-renamings))
  (make-flic-void))   ; don't share structure
  

	
     

    
