;;; ast-to-flic.scm -- convert AST to flic structures.
;;;
;;; author :  Sandra Loosemore
;;; date   :  3 Apr 1992
;;;
;;;


;;; ====================================================================
;;; Support
;;; ====================================================================


(define-walker ast-to-flic ast-td-ast-to-flic-walker)

(define-local-syntax (define-ast-to-flic ast-type lambda-list . body)
  `(define-walker-method ast-to-flic ,ast-type ,lambda-list ,@body))

(define (ast-to-flic big-let)
  (ast-to-flic-let-aux (let-decls big-let) (make-flic-void) '#t))

(define (ast-to-flic-1 ast-node)
  (call-walker ast-to-flic ast-node))

(define (ast-to-flic/list l)
  (map (function ast-to-flic-1) l))

(define (init-flic-var var value toplevel?)
  (setf (var-value var) value)
  (setf (var-toplevel? var) toplevel?)
  (setf (var-simple? var)
	(and value
	     (or (is-type? 'flic-const value)
		 (is-type? 'flic-pack value))))
  (setf (var-strict? var) '#f)
  ;; Remember the strictness annotation.
  (let ((strictness-ann (lookup-annotation var '|Strictness|)))
    (setf (var-strictness var)
	  (if strictness-ann
	      (adjust-annotated-strictness var
		(parse-strictness (car (annotation-value-args strictness-ann))))
	      '#f)))
  ;; If the variable has an inline annotation, rewrite its value
  ;; from var = value
  ;; to   var = let temp = value in temp
  ;; (Necessary for inlining recursive definitions.)
  (let ((inline-ann (lookup-annotation var '|Inline|)))
    (when inline-ann
      (setf (var-force-inline? var) '#t)
      (setf (var-value var) (wrap-with-let var value))))
  var)

(define (wrap-with-let var value)
  (let ((temp  (copy-temp-var (def-name var))))
    (init-flic-var temp (copy-flic value (list (cons var temp))) '#f)
    (make-flic-let (list temp) (make-flic-ref temp) '#t)))


;;; ====================================================================
;;; ast expression structs
;;; ====================================================================


(define-ast-to-flic lambda (object)
  (make-flic-lambda
    (map (lambda (pat)
	   (init-flic-var 
	     (cond ((var-pat? pat)
		    (var-ref-var (var-pat-var pat)))
		   (else
		    (error "Bad lambda pattern: ~s." pat)))
	     '#f
	     '#f))
	 (lambda-pats object))
    (ast-to-flic-1 (lambda-body object))))


;;; For LET, the CFN has turned all of the definitions into
;;; simple assignments to a variable.  The dependency analyzer
;;; adds recursive-decl-groups for things which need to be bound
;;; with LETREC.

(define-ast-to-flic let (object)
  (ast-to-flic-let-aux
    (let-decls object)
    (ast-to-flic-1 (let-body object))
    '#f))

(define (ast-to-flic-let-aux decls body toplevel?)
  (multiple-value-bind (bindings newbody)
      (ast-to-flic-bindings decls body toplevel?)
    (if (null? bindings)
	newbody
	(make-flic-let bindings newbody toplevel?))))

(define (ast-to-flic-bindings decls body toplevel?)
  (if (null? decls)
      (values '() body)
      (multiple-value-bind (bindings newbody)
	  (ast-to-flic-bindings (cdr decls) body toplevel?)
	(cond ((is-type? 'valdef (car decls))
	       ;; Continue collecting bindings.
	       (let* ((decl  (car decls))
		      (pat   (valdef-lhs decl))
		      (exp   (single-definition-rhs decl)))
		 (values
		  (cond ((var-pat? pat)
			 (cons
			   (init-flic-var
			    (var-ref-var (var-pat-var pat))
			    (ast-to-flic-1 exp)
			    toplevel?)
			   bindings))
			(else
			 (error "Definition has invalid pattern: ~s." decl)))
		  newbody)))
	      ((not (is-type? 'recursive-decl-group (car decls)))
	       (error "Decl has weird value: ~s." (car decls)))
	      (toplevel?
	       ;; We don't do any of this mess with top level bindings.
	       ;; Turn it into one big letrec.
	       (multiple-value-bind (more-bindings newerbody)
		   (ast-to-flic-bindings
		     (recursive-decl-group-decls (car decls))
		     newbody
		     toplevel?)
		 (values (nconc more-bindings bindings)
			 newerbody)))
	      (else
	       ;; Otherwise, turn remaining bindings into a nested
	       ;; let or letrec, and put that in the body of a new
	       ;; letrec.
	       (multiple-value-bind (more-bindings newerbody)
		   (ast-to-flic-bindings
		     (recursive-decl-group-decls (car decls))
		     (if (null? bindings)
			 newbody
			 (make-flic-let bindings newbody '#f))
		     toplevel?)
		 (values
		   '()
		   (if (null? more-bindings)
		       newerbody
		       (make-flic-let more-bindings newerbody '#t)))))
	      ))))


(define (single-definition-rhs decl)
  (let* ((def-list  (valdef-definitions decl))
	 (def       (car def-list))
	 (rhs-list  (single-fun-def-rhs-list def))
	 (rhs       (car rhs-list)))
    ;; All of this error checking could be omitted for efficiency, since
    ;; none of these conditions are supposed to happen anyway.
    (cond ((not (null? (cdr def-list)))
	   (error "Decl has multiple definitions: ~s." decl))
	  ((not (null? (single-fun-def-where-decls def)))
	   (error "Definition has non-null where-decls list: ~s." decl))
	  ((not (null? (cdr rhs-list)))
	   (error "Definition has multiple right-hand-sides: ~s." decl))
	  ((not (is-type? 'omitted-guard (guarded-rhs-guard rhs)))
	   (error "Definition has a guard: ~s." decl)))
    (guarded-rhs-rhs rhs)))



;;; These are all straightforward translations.

(define-ast-to-flic if (object)
  (make-flic-if
    (ast-to-flic-1 (if-test-exp object))
    (ast-to-flic-1 (if-then-exp object))
    (ast-to-flic-1 (if-else-exp object))))

(define-ast-to-flic case-block (object)
  (make-flic-case-block
    (case-block-block-name object)
    (ast-to-flic/list (case-block-exps object))))

(define-ast-to-flic return-from (object)
  (make-flic-return-from
    (return-from-block-name object)
    (ast-to-flic-1 (return-from-exp object))))

(define-ast-to-flic and-exp (object)
  (make-flic-and (ast-to-flic/list (and-exp-exps object))))
  

;;; Applications.  Uncurry here.  It's more convenient to do the
;;; optimizer on fully uncurried applications.  After the optimizer
;;; has run, all applications are adjusted based on observed arity
;;; of the functions and the saturated? flag is set correctly.

(define-ast-to-flic app (object)
  (ast-to-flic-app-aux object '()))

(define (ast-to-flic-app-aux object args)
  (if (is-type? 'app object)
      (ast-to-flic-app-aux
        (app-fn object)
	(cons (ast-to-flic-1 (app-arg object)) args))
      (make-flic-app (ast-to-flic-1 object) args '#f)))


;;; References

(define-ast-to-flic var-ref (object)
  (make-flic-ref (var-ref-var object)))

(define-ast-to-flic con-ref (object)
  (make-flic-pack (con-ref-con object)))


;;; Constants

(define-ast-to-flic integer-const (object)
  (make-flic-const (integer-const-value object)))


;;; We should probably add a type field to flic-const but at the moment
;;; I'll force the value to be a list of numerator, denominator.

(define-ast-to-flic float-const (object)
  (let ((e (float-const-exponent object))
	(n (float-const-numerator object))
	(d (float-const-denominator object)))
    (make-flic-const
     (if (> e 0)
	 (list (* n (expt 10 e)) d)
	 (list n (* d (expt 10 (- e))))))))

(define-ast-to-flic char-const (object)
  (make-flic-const (char-const-value object)))


(define-ast-to-flic string-const (object)
  (let ((value  (string-const-value object)))
    (if (equal? value "")
	(make-flic-pack (core-symbol "Nil"))
	(make-flic-const value))))



;;; Random stuff

(define-ast-to-flic con-number (object)
  (make-flic-con-number
    (con-number-type object)
    (ast-to-flic-1 (con-number-value object))))

(define-ast-to-flic sel (object)
  (make-flic-sel
    (sel-constructor object)
    (sel-slot object)
    (ast-to-flic-1 (sel-value object))))

(define-ast-to-flic is-constructor (object)
  (make-flic-is-constructor
    (is-constructor-constructor object)
    (ast-to-flic-1 (is-constructor-value object))))

(define-ast-to-flic void (object)
  (declare (ignore object))
  (make-flic-void))


;;; This hack make strictness annotations work.  It adds #t's which correspond
;;; to the strictness of the dict params.

(define (adjust-annotated-strictness v s)
  (let* ((ty (var-type v))
	 (c (gtype-context ty)))
    (dolist (c1 c)
      (dolist (c2 c1)
        (declare (ignorable c2))
        (push '#t s)))
    s))
