;;; This file contains ast construction functions.  These
;;; functions are supplied for commonly used ast structures to
;;; avoid the longer `make' normally required.

;;; Function names are the type names with a `**' prefix.  For reference
;;; nodes, the /def for builds the node from a definition instead of a name.

;;; Note: maybe these should be made automagicly someday.

;;; from exp-structs:

(define (**lambda args body)
  (**lambda/pat (map (function **pat) args) body))

(define (**lambda/pat pats body)
  (if (null? pats)
      body
      (make lambda (pats pats) (body body))))



;;; Make a case expression.

(define (**case exp alts)
  (make case (exp exp) (alts alts)))

(define (**alt/simple pat exp)
  (**alt pat 
	 (list (make guarded-rhs
		     (guard (make omitted-guard))
		     (rhs exp)))
	 '()))

(define (**alt pat rhs-list where-decls)
  (make alt (pat pat) (rhs-list rhs-list) (where-decls where-decls)))




(define (**let decls body)
  (if decls
      (make let (decls decls) (body body))
      body))

(define (**if test then-exp else-exp)
  (make if (test-exp test) (then-exp then-exp) (else-exp else-exp)))

(define (**app fn . args)  ; any number of args
  (**app/l fn args))

(define (**app/l fn args)  ; second args is a list
  (if (null? args)
      fn
      (**app/l (make app (fn fn) (arg (car args)))
	       (cdr args))))

(define (**var name)
  (make var-ref (name name) (var (dynamic *undefined-def*)) (infix? '#f)))

(define (**var/def def)  ; arg is an entry
  (make var-ref (var def) (name (def-name def)) (infix? '#f)))
	    
(define (**con/def def)
  (make con-ref (name (def-name def)) (con def) (infix? '#f)))

(define (**int x)
  (make integer-const (value x)))

(define (**char x)
  (make char-const (value x)))

(define (**string x)
  (make string-const (value x)))

(define (**listcomp exp quals)
  (make list-comp (exp exp) (quals quals)))

(define (**gen pat exp)
  (make qual-generator (pat (**pat pat)) (exp exp)))

(define (**omitted-guard)
  (make omitted-guard))

(define (**con-number exp algdata)
  (make con-number (type algdata) (value exp)))

(define (**sel con exp i)
  (make sel (constructor con) (value exp) (slot i)))

(define (**is-constructor exp con)
  (make is-constructor (value exp) (constructor con)))

;;; From valdef-structs

(define (**signdecl vars type)
  (make signdecl (vars (map (function **var) vars)) (signature type)))

(define (**signdecl/def vars type)
  (make signdecl (vars (map (function **var/def) vars)) (signature type)))

(define (**define name args val)
  (**valdef (**pat name) (map (function **pat) args) val))

(define (**valdef/def var exp)
  (**valdef/pat (**var-pat/def var) exp))

(define (**valdef/pat pat exp)
  (**valdef pat '() exp))

(define (**valdef lhs args rhs)
  (make valdef
	(lhs lhs)
	(definitions
	  (list (make single-fun-def
		      (args args)
		      (rhs-list
		        (list (make guarded-rhs
				    (guard (**omitted-guard))
				    (rhs rhs))))
		      (where-decls '())
		      (infix? '#f))))))


;;; Patterns (still in valdef-structs)

;;; The **pat function converts a very simple lisp-style pattern representation
;;; into corresponding ast structure.  The conversion:
;;;   a) _ => wildcard
;;;   b) a symbol => Var pattern
;;;   c) an integer / string => const pattern
;;;   d) a list of pats starting with 'tuple => Pcon
;;;   e) a list of pats starting with a con definition => Pcon

(define (**pat v)
  (cond ((eq? v '_) (**wildcard-pat))
	((symbol? v)
	 (make var-pat (var (**var v))))
	((var? v)
	 (make var-pat (var (**var/def v))))
	((integer? v)
	 (make const-pat (value (**int v))))
	((string? v)
	 (make const-pat (value (**string v))))
	((and (pair? v) (eq? (car v) 'tuple))
	 (**pcon/tuple (map (function **pat) (cdr v))))
	((and (pair? v) (con? (car v)))
	 (**pcon/def (car v) (map (function **pat) (cdr v))))
	(else
	 (error "Bad pattern in **pat: ~A~%" v))))

(define (**pcon name pats)
  (make pcon (name (add-con-prefix/symbol name))
	     (con (dynamic *undefined-def*)) (pats pats) (infix? '#f)))

(define (**pcon/def def pats)
  (make pcon (name (def-name def)) (con def) (pats pats) (infix? '#f)))

(define (**pcon/tuple pats)
  (**pcon/def (tuple-constructor (length pats)) pats))

;;; Make a variable pattern from the var

(define (**var-pat/def var)
  (make var-pat
	(var (**var/def var))))

(define (**wildcard-pat)
  (make wildcard-pat))


;;; Either make a tuple, or return the single element of a list.

(define (**tuple-pat pats)
  (cond ((null? pats)
	 (**pcon/def (core-symbol "UnitConstructor") '()))
	((null? (cdr pats))
	 (car pats))
	(else
	 (**pcon/tuple pats))))


;;; From type-structs.scm

(define (**tycon name args)
  (make tycon (name name) (args args) (def (dynamic *undefined-def*))))

(define (**tycon/def def args)
  (make tycon (name (def-name def)) (def def) (args args)))

(define (**tyvar name)
  (make tyvar (name name)))

(define (**signature context type)
  (make signature (context context) (type type)))

(define (**class/def def)
  (make class-ref (name (def-name def)) (class def)))

(define (**context tycls tyvar)
  (make context (class tycls) (tyvar tyvar)))

;;; From tc-structs

(define (**ntyvar)
  (make ntyvar (value '#f) (context '()) (dict-params '())))

(define (**ntycon tycon args)
  (make ntycon (tycon tycon) (args args)))

(define (**arrow . args) 
  (**arrow/l args))

(define (**arrow/l args)
  (if (null? (cdr args))
      (car args)
      (**ntycon (core-symbol "Arrow")
		(list (car args) (**arrow/l (cdr args))))))

(define (**arrow/l-2 args final-val)
  (if (null? args)
      final-val
      (**ntycon (core-symbol "Arrow")
		(list (car args) (**arrow/l-2 (cdr args) final-val)))))

(define (**list-of arg)
  (**ntycon (core-symbol "List") (list arg)))

(define (**recursive-placeholder var edecls)
  (make recursive-placeholder (var var) (exp '#f)
	(enclosing-decls edecls)))

(define (**dict-placeholder class tyvar edecls var)
  (make dict-placeholder
	(class class) (exp '#f) (overloaded-var var)
	(tyvar tyvar) (enclosing-decls edecls)))

(define (**method-placeholder method tyvar edecls var)
  (make method-placeholder
	(method method) (exp '#f) (overloaded-var var)
	(tyvar tyvar) (enclosing-decls edecls)))

;;; Some less primitive stuff

(define (**tuple-sel n i exp)  ;; 0 <= i < n
  (if (eqv? n 1)
      exp
      (**sel (tuple-constructor n) exp i)))

(define (**abort msg)
  (**app (**var/def (core-symbol "error"))
	 (**string msg)))

(define (**tuple/l args)
  (cond ((null? args)
	 (**con/def (core-symbol "UnitConstructor")))
	((null? (cdr args))
	 (car args))
	(else
	 (**app/l (**con/def (tuple-constructor (length args)))
		  args))))

(define (**tuple . args)
  (**tuple/l args))

(define (**tuple-type/l args)
  (cond ((null? args)
	 (**tycon/def (core-symbol "UnitConstructor") '()))
	((null? (cdr args))
	 (car args))
	(else
	 (**tycon/def (tuple-tycon (length args)) args))))

(define (**tuple-type . args)
  (**tuple-type/l args))

(define (**arrow-type . args)
  (**arrow-type/l args))

(define (**arrow-type/l args)
  (if (null? (cdr args))
      (car args)
      (**tycon/def (core-symbol "Arrow") (list (car args)
					       (**arrow-type/l (cdr args))))))

(define (**fromInteger x)
  (**app (**var/def (core-symbol "fromInteger")) x))

(define (**fromRational x)
  (**app (**var/def (core-symbol "fromRational")) x))

(define (**gtyvar n)
  (make gtyvar (varnum n)))

(define (**gtype context type)
  (make gtype (context context) (type type)))

(define (**fixity a p)
  (make fixity (associativity a) (precedence p)))

(define (**ntycon/tuple . args)
  (let ((arity  (length args)))
    (**ntycon (tuple-tycon arity) args)))

(define (**ntycon/arrow . args)
  (**ntycon/arrow-l args))

(define (**ntycon/arrow-l args)
  (let ((arg (if (integer? (car args))
		 (**gtyvar (car args))
		 (car args))))
    (if (null? (cdr args))
	arg
	(**arrow arg (**ntycon/arrow-l (cdr args))))))

(define (**save-old-exp old new)
  (make save-old-exp (old-exp old) (new-exp new)))



;;; These are used by the CFN.

(define (**case-block block-name exps)
  (make case-block
	(block-name block-name)
	(exps exps)))

(define (**return-from block-name exp)
  (make return-from
	(block-name block-name)
	(exp exp)))

(define (**and-exp . exps)
  (cond ((null? exps)
	 (**con/def (core-symbol "True")))
	((null? (cdr exps))
	 (car exps))
	(else
	 (make and-exp (exps exps)))))

