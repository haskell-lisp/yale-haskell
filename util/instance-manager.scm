
;;; This file has some random utilities dealing with instances.

;;; Right now, this is a linear search off the class.

(define (lookup-instance alg-def class-def)
  (let ((res (lookup-instance-1 alg-def (class-instances class-def))))
    (if (and (eq? res '#f) (algdata-real-tuple? alg-def))
	(lookup-possible-tuple-instances alg-def class-def)
	res)))

(define (lookup-instance-1 alg-def instances)
  (cond ((null? instances)
	 '#f)
	((eq? (instance-algdata (car instances)) alg-def)
	 (if (instance-ok? (car instances))
	     (car instances)
	     '#f))
	(else
	 (lookup-instance-1 alg-def (cdr instances)))))

(define (lookup-possible-tuple-instances alg-def class-def)
  (cond ((eq? class-def (core-symbol "Eq"))
	 (get-tuple-eq-instance alg-def))
	((eq? class-def (core-symbol "Ord"))
	 (get-tuple-ord-instance alg-def))
	((eq? class-def (core-symbol "Ix"))
	 (get-tuple-ix-instance alg-def))
	((eq? class-def (core-symbol "Text"))
	 (get-tuple-text-instance alg-def))
	((eq? class-def (core-symbol "Binary"))
	 (get-tuple-binary-instance alg-def))
	(else '#f)))

(define *saved-eq-instances* '())
(define *saved-ord-instances* '())
(define *saved-ix-instances* '())
(define *saved-text-instances* '())
(define *saved-binary-instances* '())

(define (get-tuple-eq-instance tpl)
  (let ((res (assq tpl *saved-eq-instances*)))
    (if (not (eq? res '#f))
	(tuple-2-2 res)
	(let ((inst (make-tuple-instance
		     tpl (core-symbol "Eq") (core-symbol "tupleEqDict"))))
	  (push (tuple tpl inst) *saved-eq-instances*)
	  inst))))

(define (get-tuple-ord-instance tpl)
  (let ((res (assq tpl *saved-ord-instances*)))
    (if (not (eq? res '#f))
	(tuple-2-2 res)
	(let ((inst (make-tuple-instance
		     tpl (core-symbol "Ord") (core-symbol "tupleOrdDict"))))
	  (push (tuple tpl inst) *saved-ord-instances*)
	  inst))))

(define (get-tuple-ix-instance tpl)
  (let ((res (assq tpl *saved-ix-instances*)))
    (if (not (eq? res '#f))
	(tuple-2-2 res)
	(let ((inst (make-tuple-instance
		     tpl (core-symbol "Ix") (core-symbol "tupleIxDict"))))
	  (push (tuple tpl inst) *saved-ix-instances*)
	  inst))))

(define (get-tuple-text-instance tpl)
  (let ((res (assq tpl *saved-text-instances*)))
    (if (not (eq? res '#f))
	(tuple-2-2 res)
	(let ((inst (make-tuple-instance
		     tpl (core-symbol "Text") (core-symbol "tupleTextDict"))))
	  (push (tuple tpl inst) *saved-text-instances*)
	  inst))))

(define (get-tuple-binary-instance tpl)
  (let ((res (assq tpl *saved-binary-instances*)))
    (if (not (eq? res '#f))
	(tuple-2-2 res)
	(let ((inst (make-tuple-instance
		     tpl (core-symbol "Binary")
		     (core-symbol "tupleBinaryDict"))))
	  (push (tuple tpl inst) *saved-binary-instances*)
	  inst))))

(define (make-tuple-instance algdata class dict)
  (let* ((size (tuple-size algdata))
	 (tyvars (gen-symbols size))
	 (context (map (lambda (tyvar)
			  (**context (**class/def class) tyvar))
			tyvars))
	 (sig (**tycon/def algdata (map (lambda (x) (**tyvar x)) tyvars)))
	 (gcontext (gtype-context (ast->gtype context sig))))
    (make instance 
	  (algdata algdata)
	  (tyvars tyvars)
	  (class class)
	  (context context)
	  (gcontext gcontext)
	  (methods '())
	  (dictionary dict)
	  (ok? '#t)
	  (special? '#t))))

;;; I know these are somewhere else too ...

(define (tuple-size alg)
  (con-arity (car (algdata-constrs alg))))

(define (gen-symbols n)
  (gen-symbols-1 n '(|a| |b| |c| |d| |e| |f| |g| |h| |i| |j| |k| |l| |m|
		     |n| |o| |p| |q| |r| |s| |t| |u| |v| |w| |x| |y| |z|)))

(define (gen-symbols-1 n vars)
  (if (eqv? n 0)
      '()
      (if (null? vars)
	  (cons (string->symbol (format '#f "x~A" n))
		(gen-symbols-1 (1- n) '()))
	  (cons (car vars) (gen-symbols-1 (1- n) (cdr vars))))))

;;; This handles the dynamic linking of instances into classes

(define (link-instances modules)
  (dolist (m modules)
    ;; clear out any instances sitting around from old compiles
    (dolist (class (module-class-defs m))
      (setf (class-instances class) '())))
  (dolist (m modules)
    (dolist (inst (module-instance-defs m))
       (link-instance inst)))
  )

(define (link-instance inst)  ; links an instance into the associated class
  (push inst (class-instances (instance-class inst))))

;;; This creates a new instance object and installs it.

(predefine (make-new-var name))  ; in tdecl/tdecl-utils.scm

(define (new-instance class algdata tyvars)
 (let* ((dict-name
	 (string-append "dict-"
			(symbol->string (print-name class)) "-"
			(symbol->string (print-name algdata))))
	(inst (make instance (algdata algdata)
			     (tyvars tyvars)
		             (class class)
			     (gcontext '())
			     (context '())
			     (dictionary (make-new-var dict-name)))))
   (link-instance inst)
   inst))







