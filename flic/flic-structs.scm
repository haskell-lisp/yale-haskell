;;; flic-structs.scm -- structures to define FLIC intermediate language
;;;
;;; author : Sandra Loosemore
;;; date   : 24 Mar 1992


    
(define-struct flic-exp
  (type-template flic-td)
  (slots
   (unboxed?  (type bool) (default '#f) (bit #t))
   (cheap?    (type bool) (default '#f) (bit #t))))


;;; Use a macro to define each subtype and a BOA constructor.
;;; Maybe eventually the constructors will need to do additional
;;; initialization and have to be defined by hand.

(define-local-syntax (define-flic name . slots)
  (let* ((maker  (symbol-append 'make- name))
	 (pred   (symbol-append name '?))
	 (args   (map (function car) slots))
	 (inits  (map (lambda (x) (list x x)) args)))
    `(begin
       (define-struct ,name
         (include flic-exp)
	 (predicate ,pred)
	 (slots ,@slots))
       (define (,maker ,@args) (make ,name ,@inits))
       ',name)))

(define-flic flic-lambda
  (vars (type  (list var)))
  (body (type flic-exp)))

(define-flic flic-let
  ;; value exp is stored in var-value slot
  (bindings (type (list var)))
  (body (type flic-exp))
  (recursive? (type bool) (bit #t)))

(define-flic flic-app
  (fn (type flic-exp))
  (args (type (list flic-exp)))
  ;; true if number of args exactly matches arity of fn
  (saturated? (type bool) (bit #t)))

(define-flic flic-ref
  (var (type var)))

(define-flic flic-const
  (value (type t)))

(define-flic flic-pack
  (con (type con)))

(define-flic flic-case-block
  (block-name (type symbol))
  (exps       (type (list flic-exp))))

(define-flic flic-return-from
  (block-name (type symbol))
  (exp        (type flic-exp)))

(define-flic flic-and
  (exps       (type (list flic-exp))))

(define-flic flic-if
  (test-exp   (type flic-exp))
  (then-exp   (type flic-exp))
  (else-exp   (type flic-exp)))

(define-flic flic-sel
  (con (type con))
  (i (type int))
  (exp (type flic-exp)))

(define-flic flic-is-constructor
  (con (type con))
  (exp (type flic-exp)))

(define-flic flic-con-number
  (type (type algdata))
  (exp (type flic-exp)))
	   
(define-flic flic-void
  )


