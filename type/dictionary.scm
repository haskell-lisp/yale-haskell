
;;; type/dictionary.scm

;;; This function supports dictionary conversion.  It creates lambda
;;; variables to bind to the dictionary args needed by the context.
;;; The actual conversion to lambda is done in the cfn.  Each tyvar in
;;; the context has an associated mapping from class to dictionary
;;; variable.  This mapping depends on the decl containing the placeholder
;;; since different recursive decls share common tyvars.  The mapping is
;;; two levels: decl -> class -> var.

;;; Due to language restrictions this valdef must be a simple variable
;;; definition.

(define (dictionary-conversion/definition valdef tyvars)
  (let* ((var (decl-var valdef))
	 (type (var-type var))
	 (context (gtype-context type))
	 (dict-param-vars '()))
    (dolist (c context)
      (let ((tyvar (car tyvars))
	    (dparams '()))
       (when (not (null? c))
	(dolist (class c)
          (let ((var (create-temp-var
		      (string-append "d_"
				     (symbol->string (def-name class))))))
	    (setf (var-force-strict? var) '#t)
	    (push (tuple class var) dparams)
	    (push var dict-param-vars)))
	(push (tuple valdef dparams) (ntyvar-dict-params tyvar)))
       (setf tyvars (cdr tyvars))))
    (setf (valdef-dictionary-args valdef) (nreverse dict-param-vars))))

;;; These routines deal with dict-var processing.

;;; This discharges the tyvars associated with dictionaries.  The dict-vars
;;; to be processed at the next level are returned.

(define (process-placeholders placeholders deferred decls)
  (if (null? placeholders)
      deferred
      (let ((d1 (process-placeholder (car placeholders) deferred decls)))
	(process-placeholders (cdr placeholders) d1 decls))))

;;; This processes a placeholder.  The following cases arise:
;;;  a) the variable has already been processed (no placeholders remain) -
;;;     ignore it.  placeholders may contain duplicates so this is likely.
;;;  b) the type variable is from an outer type environment (in ng-list)
;;;     and should just be passed up to the next level (added to old-placeholders)
;;;  c) the type variable is associated with a dictionary parameter
;;;  d) the type variable is instantiated to a type constructor
;;;  e) the type variable is ambiguous (none of the above)

(define (process-placeholder p deferred decls)
  (let* ((tyvar (placeholder-tyvar p))
	 (type (prune tyvar)))
    (cond ((ntycon? type)
	   (process-instantiated-tyvar
	    (expand-ntype-synonym type) p deferred decls))
	  ((non-generic? type)
	   (cons p deferred))
	  ((not (null? (ntyvar-dict-params type)))
	   (if (dict-placeholder? p)
	       (placeholder->dict-param p (ntyvar-dict-params type) decls)
	       (placeholder->method p (ntyvar-dict-params type) decls))
	   deferred)
	  (else
	   ;; Since default types are monotypes, no new vars will
	   ;; be added to old-placeholders
	   (when (maybe-default-ambiguous-tyvar
		  type (placeholder-overloaded-var p)
		  (valdef-module (car (placeholder-enclosing-decls p))))
	      (process-placeholder p deferred decls))
	   deferred))))
	       
;;; The type variable is associated with a dictionary parameter.  The only
;;; complication here is that the class needed may not be directly available -
;;; it may need to be obtained from the super classes of the parameter
;;; dictionaries.

(define (placeholder->dict-param p param-vars decls)
  (let ((class (dict-placeholder-class p))
	(edecls (dict-placeholder-enclosing-decls p)))
    (setf (placeholder-exp p)
	  (dict-reference-code class (locate-params param-vars edecls decls)))))

(define (dict-reference-code class param-vars)
  (let ((var (assq class param-vars)))
    (if (not (eq? var '#f))
	(**var/def (tuple-2-2 var))
	(search-superclasses class param-vars))))

(define (locate-params param-vars enclosing-decls decls)
  (if (null? (cdr param-vars))
      (tuple-2-2 (car param-vars))
      (let ((decl (search-enclosing-decls enclosing-decls decls)))
	(tuple-2-2 (assq decl param-vars)))))

;;; This finds the first dictionary containing the needed class in its
;;; super classes and generates a selector to get the needed dictionary.

(define (search-superclasses class param-vars)
  (let ((pclass (tuple-2-1 (car param-vars))))
    (if (memq class (class-super* pclass))
	(**dsel/dict pclass class (**var/def (tuple-2-2 (car param-vars))))
	(search-superclasses class (cdr param-vars)))))

(define (placeholder->method p param-vars decls)
  (let* ((method (method-placeholder-method p))
	 (class (method-var-class method))
	 (edecls (placeholder-enclosing-decls p))
	 (params (locate-params param-vars edecls decls)))
    (setf (placeholder-exp p)
	  (method-reference-code method class params))))

(define (method-reference-code m c param-vars)
 (let ((pclass (tuple-2-1 (car param-vars))))
  (if (or (eq? c pclass)
	  (memq c (class-super* pclass)))
      (let* ((msel (assq m (class-selectors pclass)))
	     (mvar (tuple-2-2 msel)))
	(**app (**var/def mvar) (**var/def (tuple-2-2 (car param-vars)))))
      (method-reference-code m c (cdr param-vars)))))

;;; This is for tyvars instantiated to a tycon.  A reference to the
;;; appropriate dictionary is generated.  This reference must be recursively
;;; dictionary converted since dictionaries may need subdictionaries
;;; when referenced.

(define (process-instantiated-tyvar tycon p deferred decls)
  (let* ((alg (ntycon-tycon tycon))
	 (edecls (placeholder-enclosing-decls p))
	 (var (placeholder-overloaded-var p))
	 (class (if (dict-placeholder? p)
		    (dict-placeholder-class p)
		    (method-var-class (method-placeholder-method p))))
	 (instance (lookup-instance alg class)))
    (if (dict-placeholder? p)
	(mlet (((code def1)
		(generate-dict-ref instance tycon deferred decls edecls var)))
	   (setf (placeholder-exp p) code)
	   (setf deferred def1))
	(let ((method (method-placeholder-method p)))
	  (if (every (function null?) (instance-gcontext instance))
	      (let ((mvar (tuple-2-2
			   (assq method (instance-methods instance)))))
		(setf (placeholder-exp p) (**var/def mvar)))
	      (mlet (((code def1)
		      (generate-dict-ref
		         instance tycon deferred decls edecls var))
		     (sel (tuple-2-2 (assq method (class-selectors class)))))
		(setf (method-placeholder-exp p) (**app (**var/def sel) code))
		(setf deferred def1)))))
    deferred))

;;; This generates a reference to a specific dictionary and binds
;;; needed subdictionaries.  Since subdictionaries may be part of the outer
;;; type environment new placeholders may be generated for later resolution.

(define (generate-dict-ref instance type deferred decls edecls var)
  (let* ((ctxt (instance-gcontext instance))
	 (dict (dict-ref-code instance)))
    (do-contexts (class ctxt) (ty (ntycon-args type))
      (let ((ntype (prune ty)))
	(cond
	 ((ntycon? ntype)
	  (mlet ((ntype (expand-ntype-synonym ntype))
		 (alg (ntycon-tycon ntype))
		 (instance (lookup-instance alg class))
		 ((code dv1)
		  (generate-dict-ref
		    instance ntype deferred decls edecls var)))
	      (setf dict (**app dict code))
	      (setf deferred dv1)))
	 ((non-generic? ntype)
	  (let ((p (**dict-placeholder
		    class ntype edecls var)))
	    (setf dict (**app dict p))
	    (push p deferred)))
	 ((null? (ntyvar-dict-params ntype))
	  (let ((ref-code (**dict-placeholder
			   class ntype edecls var)))
	     (when (maybe-default-ambiguous-tyvar
		    ntype var (valdef-module (car edecls)))
		(process-placeholder ref-code '() decls))
	     (setf dict (**app dict ref-code))))
	 (else
	  (let ((p (locate-params (ntyvar-dict-params ntype) edecls decls)))
	    (setf dict (**app dict (dict-reference-code class p))))))))
    (values dict deferred)))

;;; The following routines deal with recursive placeholders.  The basic
;;; strategy is to pass the entire context as a parameter with each
;;; recursive call (this could be optimized later to make use of an
;;; internal entry point).  The basic complication is that the context
;;; of each function in a letrec may be arranged differently.

;;; This generates a call inside decl 'from' to the var 'to'.  Vmap is an
;;; alist from vars to a list of vars corresponding to the gtyvars of
;;; the decl signature.

(define (recursive-call-code from to vmap)
  (let ((exp (**var/def to))
	(tyvars (tuple-2-2 (assq to vmap)))
	(contexts (gtype-context (var-type to))))
    (do-contexts (class contexts) (tyvar tyvars)
       (setf exp (**app exp (locate-param-var tyvar class from))))
    exp))

(define (locate-param-var tyvar class decl)
  (let ((vmap (tuple-2-2 (assq decl (ntyvar-dict-params tyvar)))))
    (**var/def (tuple-2-2 (assq class vmap)))))

;;; This is used to get the code for a specific dictionary reference.

(define (dict-ref-code instance)
  (**var/def (instance-dictionary instance)))

;;; This is used to locate the correct enclosing decl.

(define (search-enclosing-decls decl-list decls)
  (cond ((null? decl-list)
	 (error "Lost decl in search-enclosing-decls!"))
	((memq (car decl-list) decls)
	 (car decl-list))
	(else
	 (search-enclosing-decls (cdr decl-list) decls))))

