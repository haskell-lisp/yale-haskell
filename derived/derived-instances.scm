
;;; Basic DI structure:
;;;  a. Create the set of instances
;;;  b. Expand the context of each potential instance.
;;;  c. Once b. reaches a fixpoint, fill in the ast for the generated instances

(define *di-context-changed* '#f)

(define (add-derived-instances modules)
  (let ((insts '()))
    (walk-modules modules
     (lambda () (setf insts (append (find-derivable-instances) insts))))
    (compute-di-fixpoint insts)
    (dolist (inst insts)
      (when (instance-ok? inst)
        (create-instance-fns inst)
	(push inst (module-instance-defs
		    (table-entry *modules*
				 (def-module (instance-algdata inst)))))))))

(define (compute-di-fixpoint insts)
  (setf *di-context-changed* '#f)
  (dolist (inst insts)
    (propagate-di-context inst))
  (when *di-context-changed* (compute-di-fixpoint insts)))

;;; Create instance decls for all derived instances in a module.  Filter
;;; out underivable instances (Ix & Enum only)

(define (find-derivable-instances)
  (let ((algs (module-alg-defs *module*))
	(insts '()))
    (dolist (alg algs)
      (dolist (class (algdata-deriving alg))
	 (cond ((memq class (list (core-symbol "Eq")
				  (core-symbol "Ord")
				  (core-symbol "Text")
				  (core-symbol "Binary")))
		(setf insts (add-derivable-instance insts alg class '#f)))
	       ((eq? class *printer-class*)
		(setf insts (add-derivable-instance
			     insts alg (core-symbol "Text") '#t)))
	       ((eq? class (core-symbol "Ix"))
		(if (or (algdata-enum? alg)
			(algdata-tuple? alg))
		    (setf insts (add-derivable-instance insts alg class '#f))
		    (signal-cant-derive-ix alg)))
	       ((eq? class (core-symbol "Enum"))
		(if (algdata-enum? alg)
		    (setf insts (add-derivable-instance insts alg class '#f))
		    (signal-cant-derive-enum alg)))
	       (else
		(signal-not-derivable class)))))
    insts))


(define (signal-cant-derive-ix alg)
  (phase-error 'cant-derive-IX
    "An Ix instance for ~A cannot be derived.  It is not an enumeration~%~
     or single-constructor datatype."
    alg))

(define (signal-cant-derive-enum alg)
  (phase-error 'cant-derive-Enum
    "An Enum instance for ~A cannot be derived.  It is not an enumeration."
    alg))

(define (signal-not-derivable class)
  (recoverable-error 'not-derivable
    "Class ~A is not one of the classes that permits derived instances."
    class))


;; This adds a provisional instance template.  Of course, there may already
;;; be an instance (error!)

(define (add-derivable-instance insts alg cls sp)
  (let ((existing-inst (lookup-instance alg cls)))
    (cond ((eq? existing-inst '#f)
	   (let ((inst (new-instance cls alg (algdata-tyvars alg))))
	     (setf (instance-context inst) (algdata-context alg))
	     (setf (instance-decls inst) '())
	     (setf (instance-ok? inst) '#t)
	     (setf (instance-suppress-readers? inst) sp)
	     (cons inst insts)))
	  (else
	   (signal-instance-exists alg cls)
	   insts))))

(define (signal-instance-exists alg cls)
  (recoverable-error 'instance-exists
    "An instance for type ~A in class ~A already exists;~%~
     the deriving clause is being ignored."
    alg cls))

;;; This updates all instance contexts for an algdata.  Each derivable
;;; instance generates a recursive context for every field.  If a
;;; component cannot satisfy the desired context, the ok? field is set to
;;; #f to mark the instance as bogus.

(define (propagate-di-context inst)
  (when (instance-ok? inst)
    (propagate-constructor-contexts inst
			   (algdata-constrs (instance-algdata inst)))))

;;; These two functions propagate the context to ever field of every
;;; constructor

(define (propagate-constructor-contexts inst constrs)
  (or (null? constrs)
      (and (propagate-contexts inst (instance-class inst)
			       (con-types (car constrs)))
	   (propagate-constructor-contexts inst (cdr constrs)))))

(define (propagate-contexts inst class types)
  (or (null? types)
      (and (propagate-type-context inst class (car types))
	   (propagate-contexts inst class (cdr types)))))

;;; This propagates a context out to a given type.  The type can only contain
;;; the tyvars which are args to the algdata.

(define (propagate-type-context inst class type)
  (cond ((tyvar? type)
	 (cond ((single-ast-context-implies?
		   (instance-context inst) class (tyvar-name type))
		'#t)
	       (else
		(setf *di-context-changed* '#t)
		(setf (instance-context inst)
		      (augment-context (instance-context inst) class
				       (tyvar-name type)))
		'#t)))
	((synonym? (tycon-def type))
	 (propagate-type-context inst class (expand-synonym type)))
	(else
	 (let* ((algdata (tycon-def type))  ; must be a algdata
	        (args (tycon-args type))
		(new-inst (lookup-instance algdata class)))
	   (cond ((or (eq? new-inst '#f)
		      (not (instance-ok? new-inst)))
		  (signal-cannot-derive-instance
		    (instance-class inst) (instance-algdata inst))
		  (setf (instance-ok? inst) '#f)
		  (setf *di-context-changed* '#t)
		  '#f)
		 (else
		  (propagate-instance-contexts inst 
				      (instance-context new-inst)
				      (instance-tyvars new-inst)
				      args)))))))


(define (single-ast-context-implies? ast-context class tyvar)
  (cond ((null? ast-context)
	 '#f)
	((eq? tyvar (context-tyvar (car ast-context)))
	 (let ((class1 (class-ref-class (context-class (car ast-context)))))
	   (or (eq? class1 class)
	       (memq class (class-super* class1))
	       (single-ast-context-implies? (cdr ast-context) class tyvar))))
	(else
	 (single-ast-context-implies? (cdr ast-context) class tyvar))))

;;; *** This message makes no sense to me.  What is the problem that
;;; *** makes it impossible to derive the instance?

(define (signal-cannot-derive-instance class alg)
  (phase-error 'cannot-derive-instance
    "Instance ~A(~A) cannot be derived."
    class alg))


;;; This propagates contexts into structure components.  The context
;;; changes due to the context associated with the various instance
;;; decls encountered.

;;; Here's the plan for expanding Cls(Alg t1 t2 .. tn) using
;;; instance (Cls1(vx),Cls2(vy),...) => Cls(Alg(v1 v2 .. vn))
;;;   for each Clsx in the instance context, propagate Clsx to the
;;;   ti corresponding to vx, where vx must be in the set vi.

(define (propagate-instance-contexts inst contexts tyvars args)
  (or (null? contexts)
      (and (propagate-type-context inst
	      (class-ref-class (context-class (car contexts)))
	      (find-corresponding-tyvar
	       (context-tyvar (car contexts)) tyvars args))
	   (propagate-instance-contexts inst (cdr contexts) tyvars args))))

;;; Given the t(i) and the v(i), return the t corresponding to a v.

(define (find-corresponding-tyvar tyvar tyvars args)
  (if (eq? tyvar (car tyvars))
      (car args)
      (find-corresponding-tyvar tyvar (cdr tyvars) (cdr args))))

;;; 1 level type synonym expansion

(define (expand-synonym type)
  (let* ((synonym (tycon-def type))
	 (args (synonym-args synonym))
	 (body (synonym-body synonym)))
  (let ((alist (map (lambda (tyvar arg) (tuple tyvar arg))
		    args (tycon-args type))))
    (copy-synonym-body body alist))))

(define (copy-synonym-body type alist)
  (if (tyvar? type)
      (tuple-2-2 (assq (tyvar-name type) alist))
      (make tycon (def (tycon-def type))
	          (name (tycon-name type))
		  (args (map (lambda (ty)
			       (copy-synonym-body ty alist))
			     (tycon-args type))))))

;;; This fills in the body decls for an instance function.

(define (create-instance-fns inst)
  (let ((class (instance-class inst))
	(alg (instance-algdata inst)))
    (cond ((eq? class (core-symbol "Eq"))
	   (add-instance inst (eq-fns alg)))
	  ((eq? class (core-symbol "Ord"))
	   (add-instance inst (ord-fns alg)))
	  ((eq? class (core-symbol "Ix"))
	   (add-instance inst (ix-fns alg)))
	  ((eq? class (core-symbol "Enum"))
	   (add-instance inst (enum-fns alg)))
	  ((eq? class (core-symbol "Text"))
	   (add-instance inst (text-fns alg (instance-suppress-readers? inst))))
	  ((eq? class (core-symbol "Binary"))
	   (add-instance inst (binary-fns alg))))))

(define (add-instance inst decls)
  (setf (instance-decls inst) decls))

;;; Add class(var) to a context, removing any contexts made redundant by
;;; the new addition.  Example: adding Ord a to (Eq a, Eq b) would yield
;;; (Ord a,Eq b).

(define (augment-context contexts cl var)
  (cons (**context (**class/def cl) var)
	(remove-implied-contexts cl var contexts)))

(define (remove-implied-contexts class1 tyvar1 contexts)
  (if (null? contexts)
      '#f
      (with-slots context (class tyvar) (car contexts)
	(let ((rest (remove-implied-contexts class1 tyvar1 (cdr contexts)))
	      (class2 (class-ref-class class)))
	  (if (and (eq? tyvar1 tyvar)
		   (memq class2 (class-super* class1)))
	      rest
	      (cons (car contexts) rest))))))
