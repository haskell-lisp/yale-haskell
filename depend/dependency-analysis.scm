;;; depend/depend.scm     Author: John

;;; This performs dependency analysis.  All module definitions are gathered
;;; into a single nested let/let*.

(define-walker depend ast-td-depend-walker)

;;; This extracts the declarations out of the top level of the modules and
;;; creates a single let defining all values from the modules.

(define (do-dependency-analysis modules)
  (let ((all-decls '()))
    (dolist (mod modules)
      (setf all-decls (append (module-decls mod) all-decls)))
    (analyze-dependency-top
      (**let all-decls (make void)))))


(define *depend-fn-table* (make-table))

(define-syntax (var-depend-fn var)
  `(table-entry *depend-fn-table* ,var))

(define (analyze-dependency-top x)
  (dynamic-let ((*depend-fn-table*  (make-table)))
    (analyze-dependency x)))


;;; This is the entry point to dependency analysis for an expression or decl

(define (analyze-dependency x)
  (call-walker depend x))

(define (analyze-dependency/list l)
  (dolist (x l)
    (analyze-dependency x)))

;;; This makes default walkers for dependency analysis.  Expressions are
;;; walked into; declaration lists must be sorted.

(define-local-syntax (make-depend-code slot type)
  (let ((stype  (sd-type slot))
        (sname  (sd-name slot))
	(depend-exp-types '(exp alt qual single-fun-def guarded-rhs)))
    (cond ((and (symbol? stype)
		(memq stype depend-exp-types))
	   `(analyze-dependency (struct-slot ',type ',sname object)))
          ((and (pair? stype)
                (eq? (car stype) 'list)
                (symbol? (cadr stype))
                (memq (cadr stype) depend-exp-types)
	   `(analyze-dependency/list
		(struct-slot ',type ',sname object))))
          ((equal? stype '(list decl))
	   `(setf (struct-slot ',type ',sname object)
		  (restructure-decl-list (struct-slot ',type ',sname object))))
          (else
;           (format '#t "Depend: skipping slot ~A in ~A~%"
;                  (sd-name slot)
;                  type)
           '#f))))

(define-modify-walker-methods depend
  (lambda let if case alt exp-sign app con-ref
   integer-const float-const char-const string-const
   list-exp sequence sequence-then sequence-to sequence-then-to
   list-comp section-l section-r qual-generator qual-filter omitted-guard
   con-number sel is-constructor cast void
   single-fun-def guarded-rhs
   case-block return-from and-exp
   )
  (object)
  make-depend-code)

;;; This sorts a list of decls.  Recursive groups are placed in
;;; special structures: recursive-decl-group

(define (restructure-decl-list decls)
  (let ((stack '())
	(now 0)
	(sorted-decls '())
	(edge-fn '()))
   (letrec ((visit (lambda (k)
		     (let ((minval 0)
			   (recursive? '#f)
			   (old-edge-fn edge-fn))
		       (incf now)
;		       (format '#t "Visiting ~A: id = ~A~%" (valdef-lhs k) now)
		       (setf (valdef-depend-val k) now)
		       (setf minval now)
		       (push k stack)
		       (setf edge-fn
			     (lambda (tv)
;			       (format '#t "Edge ~A -> ~A~%" (valdef-lhs k)
;				                             (valdef-lhs tv))
			       (let ((val (valdef-depend-val tv)))
                                (cond ((eq? tv k)
				       (setf recursive? '#t))
				      ((eqv? val 0)
				       (setf minval (min minval
							 (funcall visit tv))))
				      (else
				       (setf minval (min minval val))))
;				(format '#t "Min for ~A is ~A~%"
;					(valdef-lhs k) minval)
			       )))
		       (analyze-dependency/list (valdef-definitions k))
		       (setf edge-fn old-edge-fn)
		       (when (eqv? minval (valdef-depend-val k))
			 (let ((defs '()))
			   (do ((quit? '#f)) (quit?)
			     (push (car stack) defs)
			     (setf (valdef-depend-val (car stack)) 100000)
			     (setf quit? (eq? (car stack) k))
			     (setf stack (cdr stack)))
;			   (format '#t "Popping stack: ~A~%"
;				   (map (lambda (x) (valdef-lhs x)) defs))
			   (if (and (null? (cdr defs))
				    (not recursive?))
			       (push k sorted-decls)
			       (push (make recursive-decl-group (decls defs))
				     sorted-decls))))
		       minval))))
    ;; for now assume all decl lists have only valdefs
    (dolist (d decls)
      (let ((decl d))  ; to force new binding for each closure
	(setf (valdef-depend-val decl) 0)
	(dolist (var (collect-pattern-vars (valdef-lhs decl)))
	  (setf (var-depend-fn (var-ref-var var))
		(lambda () (funcall edge-fn decl))))))
    (dolist (decl decls)
      (when (eqv? (valdef-depend-val decl) 0)
	(funcall visit decl)))
    (dolist (decl decls)
      (dolist (var (collect-pattern-vars (valdef-lhs decl)))
	(setf (var-depend-fn (var-ref-var var)) '#f)))
    (nreverse sorted-decls))))

;;; This is the only non-default walker needed.  When a reference to a
;;; variable is encountered, the sort algorithm above is notified.

(define-walker-method depend var-ref (object)
  (let ((fn (var-depend-fn (var-ref-var object))))
    (when (not (eq? fn '#f))
       (funcall fn))))

(define-walker-method depend overloaded-var-ref (object)
  (let ((fn (var-depend-fn (overloaded-var-ref-var object))))
    (when (not (eq? fn '#f))
       (funcall fn))))

