
;;; The `prune' function removes instantiated type variables at the
;;; top level of a type.

;;; It returns an uninstantiated type variable or a type constructor.

(define-integrable (prune ntype)
  (if (ntyvar? ntype)
      (if (instantiated? ntype)
	  (prune-1 (ntyvar-value ntype))
	  ntype)
      ntype))

;;; This is because lucid can't hack inlining recursive fns.

(define (prune-1 x) (prune x))

(define-integrable (instantiated? ntyvar)
  (ntyvar-value ntyvar))
;  (not (eq? (ntyvar-value ntyvar) '#f)))  ;*** Lucid compiler bug?

(define (prune/l l)
  (map (function prune) l))


;;; These functions convert between AST types and gtypes.  Care is taken to
;;; ensure that the gtyvars are in the same order that they appear in the
;;; context.  This is needed to make dictionary conversion work right.

(define (ast->gtype context type)
  (mlet (((gcontext env) (context->gcontext context '() '()))
	 ((type env1) (type->gtype type env))
	 (gcontext-classes (arrange-gtype-classes env1 gcontext)))
    (**gtype gcontext-classes type)))

;;; This is similar except that the ordering of the tyvars is as defined in
;;; the data type.  This is used only for instance declarations and allows
;;; for simple context implication checks.  It also used by the signature
;;; of the dictionary variable.

(define (ast->gtype/inst context type)
  (mlet (((type env) (type->gtype type '()))
	 ((gcontext env1) (context->gcontext context '() env))
	 (gcontext-classes (arrange-gtype-classes env1 gcontext)))
    (**gtype gcontext-classes type)))

;;; This converts a context into gtype form [[class]]: a list of classes
;;; for each gtyvar.  This returns the context and the gtyvar environment.

(define (context->gcontext context gcontext env)
  (if (null? context)
      (values gcontext env)
      (mlet ((sym (context-tyvar (car context)))
	     (class (class-ref-class (context-class (car context))))
	     ((n new-env) (ast->gtyvar sym env))
	     (old-context (get-gtyvar-context n gcontext))
	     (new-context (merge-single-class class old-context))
	     (new-gcontext (cons (tuple n new-context) gcontext)))
	(context->gcontext (cdr context) new-gcontext new-env))))

;;; This assigns a gtyvar number to a tyvar name.

(define (ast->gtyvar sym env)
  (let ((res (assq sym env)))
    (if (eq? res '#f)
	(let ((n (length env)))
	  (values n (cons (tuple sym n) env)))
	(values (tuple-2-2 res) env))))

(define (get-gtyvar-context n gcontext)
  (cond ((null? gcontext)
	 '())
	((eqv? n (tuple-2-1 (car gcontext)))
	 (tuple-2-2 (car gcontext)))
	(else (get-gtyvar-context n (cdr gcontext)))))

(define (type->gtype type env)
  (if (tyvar? type)
      (mlet (((n env1) (ast->gtyvar (tyvar-name type) env)))
	(values (**gtyvar n) env1))
      (mlet (((types env1) (type->gtype/l (tycon-args type) env)))
	(values (**ntycon (tycon-def type) types) env1))))

(define (type->gtype/l types env)
  (if (null? types)
      (values '() env)
      (mlet (((type env1) (type->gtype (car types) env))
	     ((other-types env2) (type->gtype/l (cdr types) env1)))
	 (values (cons type other-types) env2))))

(define (arrange-gtype-classes env gcontext)
  (arrange-gtype-classes-1 0 (length env) env gcontext))

(define (arrange-gtype-classes-1 m n env gcontext)
  (if (equal? m n)
      '()
      (cons (get-gtyvar-context m gcontext)
	    (arrange-gtype-classes-1 (1+ m) n env gcontext))))

;;; These routines convert gtypes back to ordinary types.

(define (instantiate-gtype g)
 (mlet (((gtype _) (instantiate-gtype/newvars g)))
    gtype))

(define (instantiate-gtype/newvars g)
  (if (null? (gtype-context g))
      (values (gtype-type g) '())
      (let ((new-tyvars (create-new-tyvars (gtype-context g))))
	(values (copy-gtype (gtype-type g) new-tyvars) new-tyvars))))

(define (create-new-tyvars ctxts)
  (if (null? ctxts)
      '()
      (let ((tyvar (**ntyvar)))
	(setf (ntyvar-context tyvar) (car ctxts))
	(cons tyvar (create-new-tyvars (cdr ctxts))))))

(define (copy-gtype g env)
  (cond ((ntycon? g)
	 (**ntycon (ntycon-tycon g)
		   (map (lambda (g1) (copy-gtype g1 env))
			(ntycon-args g))))
	((ntyvar? g)
	 g)
	((gtyvar? g)
	 (list-ref env (gtyvar-varnum g)))
	((const-type? g)
	 (const-type-type g))))

;;; ntypes may contain synonyms.  These are expanded here.  Only the
;;; top level synonym is expanded.

(define (expand-ntype-synonym type)
  (if (and (ntycon? type)
	   (synonym? (ntycon-tycon type)))
      (let ((syn (ntycon-tycon type)))
	(expand-ntype-synonym
  	  (expand-ntype-synonym-1 (synonym-body syn)
				  (map (lambda (var val)
					 (tuple var val))
				       (synonym-args syn)
				       (ntycon-args type)))))
      type))

(define (expand-ntype-synonym-1 type env)
  (if (tyvar? type)
      (tuple-2-2 (assq (tyvar-name type) env))
      (**ntycon (tycon-def type)
		(map (lambda (ty) (expand-ntype-synonym-1 ty env))
		     (tycon-args type)))))

;;; This is used in generalization.  Note that ntyvars will remain when
;;; non-generic tyvars are encountered.

(define (ntype->gtype ntype)
  (mlet (((res _) (ntype->gtype/env ntype '())))
    res))

(define (ntype->gtype/env ntype required-vars)
  (mlet (((gtype env) (ntype->gtype-1 ntype required-vars)))
   (values 
    (make gtype (type gtype) (context (map (lambda (x) (ntyvar-context x))
					  env)))
    env)))

(define (ntype->gtype-1 ntype env)
 (let ((ntype (prune ntype)))
  (cond ((ntycon? ntype)
	 (mlet (((args env1) (ntype->gtype/l (ntycon-args ntype) env)))
	   (values (**ntycon (ntycon-tycon ntype) args) env1)))
	(else
	 (ntyvar->gtyvar ntype env)))))

(define (ntype->gtype/l types env)
  (if (null? types)
      (values '() env)
      (mlet (((type env1) (ntype->gtype-1 (car types) env))
	     ((types2 env2) (ntype->gtype/l (cdr types) env1)))
	(values (cons type types2) env2))))

(define (ntyvar->gtyvar ntyvar env)
  (if (non-generic? ntyvar)
      (values ntyvar env)
      (let ((l (list-pos ntyvar env)))
	(if (eq? l '#f)
	    (values (**gtyvar (length env)) (append env (list ntyvar)))
	    (values (**gtyvar l) env)))))
     
(define (list-pos x l)
  (list-pos-1 x l 0))

(define (list-pos-1 x l n)
  (cond ((null? l)
	 '#f)
	((eq? x (car l))
	 n)
	(else
	 (list-pos-1 x (cdr l) (1+ n)))))


;;; These utils are used in dictionary conversion.

(define (**dsel/method class method dict-code)
  (let ((pos (locate-in-list method (class-method-vars class) 0)))
    (**tuple-sel (class-dict-size class) pos dict-code)))

(define (**dsel/dict class dict-class dict-code)
  (let ((pos (locate-in-list
	      dict-class (class-super* class) (class-n-methods class))))
    (**tuple-sel (class-dict-size class) pos dict-code)))
  
(define (locate-in-list var l pos)
  (if (null? l)
      (error "Locate in list failed")
      (if (eq? var (car l))
	  pos
	  (locate-in-list var (cdr l) (1+ pos)))))

;;; These routines deal with contexts.  A context is a list classes.

;;; A context is normalized whenever class is a superclass of another.

(define (merge-contexts ctxt1 ctxt2)
  (if (null? ctxt1)
      ctxt2
      (merge-single-class (car ctxt1) (merge-contexts (cdr ctxt1) ctxt2))))

;;; This could perhaps avoid some consing but I don't imagine it would
;;; make much difference.

(define (merge-single-class class ctxt)
  (cond ((null? ctxt)
	 (list class))
	((eq? class (car ctxt))
	 ctxt)
	((memq class (class-super* (car ctxt)))
	 ctxt)
	((memq (car ctxt) (class-super* class))
	 (merge-single-class class (cdr ctxt)))
	(else
	 (cons (car ctxt) (merge-single-class class (cdr ctxt))))))

;;; This determines if ctxt2 is contained in ctxt1.

(define (context-implies? ctxt1 ctxt2)
  (or (null? ctxt2)
      (and (single-class-implies? ctxt1 (car ctxt2))
	   (context-implies? ctxt1 (cdr ctxt2)))))

(define (single-class-implies? ctxt class)
  (and (not (null? ctxt))
       (or (memq class ctxt)
	   (super-class-implies? ctxt class))))

(define (super-class-implies? ctxt class)
  (and (not (null? ctxt))
       (or (memq class (class-super* (car ctxt)))
	   (super-class-implies? (cdr ctxt) class))))

;;; This looks at the context of a full signature.

(define (full-context-implies? ctxt1 ctxt2)
  (or (null? ctxt1)
      (and (context-implies? (car ctxt1) (car ctxt2))
	   (full-context-implies? (cdr ctxt1) (cdr ctxt2)))))

;;; This is used to avoid type circularity on unification.

(define (occurs-in-type tyvar type) ; Cardelli algorithm
  (let ((type (prune type)))
    (if (ntyvar? type)
	(eq? type tyvar)
	(occurs-in-type/l tyvar (ntycon-args type)))))

; Does a tyvar occur in a list of types?
(define (occurs-in-type/l tyvar types)
  (if (null? types)
      '#f
      (or (occurs-in-type tyvar (car types))
	  (occurs-in-type/l tyvar (cdr types)))))

(define-integrable (non-generic? tyvar)
  (occurs-in-type/l tyvar (dynamic *non-generic-tyvars*)))

(define (collect-tyvars ntype)
  (collect-tyvars-1 ntype '()))

(define (collect-tyvars-1 ntype vars)
 (let ((ntype (prune ntype)))
  (if (ntyvar? ntype)
      (if (or (memq ntype vars) (non-generic? ntype))
	  vars
	  (cons ntype vars))
      (collect-tyvars/l-1 (ntycon-args ntype) vars))))

(define (collect-tyvars/l types)
  (collect-tyvars/l-1 types '()))

(define (collect-tyvars/l-1 types vars)
  (if (null? types)
      vars
      (collect-tyvars/l-1 (cdr types) (collect-tyvars-1 (car types) vars))))

;;; Random utilities

(define (decl-var decl)
  (var-ref-var (var-pat-var (valdef-lhs decl))))
