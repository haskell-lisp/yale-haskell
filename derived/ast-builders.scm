;;; These functions build non-trivial ast structure.

;;; Prelude functions: booleans

(define (**== e1 e2)
  (**app (**var/def (core-symbol "==")) e1 e2))

(define (**<= e1 e2)
  (**app (**var/def (core-symbol "<=")) e1 e2))

(define (**< e1 e2)
  (**app (**var/def (core-symbol "<")) e1 e2))

(define (**> e1 e2)
  (**app (**var/def (core-symbol ">")) e1 e2))

(define (**and e1 e2)
  (**app (**var/def (core-symbol "&&")) e1 e2))

(define (**or e1 e2)
  (**app (**var/def (core-symbol "||")) e1 e2))

(define (**true) (**con/def (core-symbol "True")))

(define (**false) (**con/def (core-symbol "False")))

;; Tuples

(define (**tuple2 x y)
  (**app (**con/def (tuple-constructor 2)) x y))

(define (**tupleN exps)
  (**app/l (**con/def (tuple-constructor (length exps))) exps))

;; Arithmetic

(define (**+ x y)
  (**app (**var/def (core-symbol "+")) x y))

(define (**+/Int x y)
  (**app (**var/def (core-symbol "primPlusInt")) x y))

(define (**- x y)
  (**app (**var/def (core-symbol "-")) x y))

(define (**1+ x)
  (**+ x (**int 1)))

;; Lists

(define (**cons x y)
  (**app (**con/def (core-symbol ":")) x y))

(define (**null)
  (**con/def (core-symbol "Nil")))

(define (**list . args)
  (**list/l args))

(define (**list/l args)
  (if (null? args)
      (**null)
      (**cons (car args)
	      (**list/l (cdr args)))))

(define (**list/pattern pats)
  (if (null? pats)
      (**pcon/def (core-symbol "Nil") '())
      (**pcon/def (core-symbol ":")
		  (list (car pats) (**list/pattern (cdr pats))))))

(define (**append . lists)
  (**append/l lists))

(define (**append/l lists)
  (if (null? (cdr lists))
      (car lists)
      (**app (**var/def (core-symbol "++"))
	     (car lists)
	     (**append/l (cdr lists)))))

(define (**take n l)
  (**app (**var/def (core-symbol "take")) n l))

(define (**drop n l)
  (**app (**var/def (core-symbol "drop")) n l))

;; Functionals

(define (**dot fn . args)
  (**dot/l fn args))

(define (**dot/l fn args)
 (if (null? args)
     fn
     (**app (**var/def (core-symbol ".")) fn (**dot/l (car args) (cdr args)))))

;; Printing

(define (**showChar x)
  (**app (**var/def (core-symbol "showChar")) x))

(define (**space)
  (**showChar (**char #\ )))

(define (**comma)
  (**showChar (**char #\,)))

(define (**showsPrec x y)
  (**app (**var/def (core-symbol "showsPrec")) x y))

(define (**shows x)
  (**app (**var/def (core-symbol "shows")) x))

(define (**showString x)
  (**app (**var/def (core-symbol "showString")) x))

(define (**showParen x y)
  (**app (**var/def (core-symbol "showParen")) x y))

;; Reading

(define (**readsPrec x y)
  (**app (**var/def (core-symbol "readsPrec")) x y))

(define (**lex x)
  (**app (**var/def (core-symbol "lex")) x))

(define (**readParen bool fn r)
  (**app (**var/def (core-symbol "readParen")) bool fn r))

(define (**reads s)
  (**app (**var/def (core-symbol "reads")) s))

;;; Binary

(define (**showBinInt i b)
  (**app (**var/def (core-symbol "primShowBinInt")) i b))

(define (**readBinSmallInt max b)
  (**app (**var/def (core-symbol "primReadBinSmallInt")) max b))

(define (**showBin x b)
  (**app (**var/def (core-symbol "showBin")) x b))

(define (**readBin b)
  (**app (**var/def (core-symbol "readBin")) b))

;;; Some higher level code generators

;;; foldr  (expanded inline)

(define (**foldr build-fn terms init)
  (if (null? terms)
      init
      (funcall build-fn (car terms) (**foldr build-fn (cdr terms) init))))

;;; Unlike foldr, this uses two sets of args to avoid tupling

(define (**foldr2 build-fn terms1 terms2 init-fn)
  (if (null? (cdr terms1))
      (funcall init-fn (car terms1) (car terms2))
      (funcall build-fn (car terms1) (car terms2)
	      (**foldr2 build-fn (cdr terms1) (cdr terms2) init-fn))))

;;; Enum

(define (**enumFrom x)
  (**app (**var/def (core-symbol "enumFrom")) x))

(define (**enumFromThen from then)
  (**app (**var/def (core-symbol "enumFromThen")) from then))

(define (**enumFromTo from to)
  (**app (**var/def (core-symbol "enumFromTo")) from to))

(define (**enumFromThenTo from then to)
  (**app (**var/def (core-symbol "enumFromThenTo")) from then to))

;;; Cast overrides the type system

(define (**cast x)
  (make cast (exp x)))

;;; Case.  This also generates the alts.  All variants of case generate
;;; an arm for each constructor in a datatype.  This arm can be selected
;;; by pattern matching a value of the type, with all fields bound to vars,
;;; or with numbered or named selections.

;;; The fn always generates the arms given the constructor.  In the /con case,
;;; the fn also gets the variable list of values bound in the fields.

(define (**case/con alg exp fn)
  (**case exp
	  (map (lambda (con)
		 (let* ((arity (con-arity con))
			(vars (temp-vars "x" arity)))
		   (**alt/simple (**pat (cons con vars))
				 (funcall fn con vars))))
	       (algdata-constrs alg))))

;;; Selectors are integers (used for Bin)

(define (**case/int alg exp fn)
  (**case exp
    (map (lambda (con)
	   (**alt/simple
	      (**pat (con-tag con))
	      (funcall fn con)))
	 (algdata-constrs alg))))

;;; Selectors are strings (Text)

(define (**case/strings alg exp fn)
  (**case exp
    (map (lambda (con)
	   (**alt/simple
	    (**pat (remove-con-prefix (symbol->string (def-name con))))
	    (funcall fn con)))
	 (algdata-constrs alg))))

;;; Definitions containing multi-body

(define (**multi-define fname alg nullary-fn single-fn
			          combine-fn else-val)
  (**define/multiple fname
    (append
      (map (lambda (con) (**define/2 con nullary-fn single-fn combine-fn))
	     (algdata-constrs alg))
      (if (not (eq? else-val '#f))
	  `(((_ _) ,(funcall else-val)))
	  '()))))

(define (**define/2 con nullary-fn single-fn combine-fn)
  (let* ((arity (con-arity con))
	 (vars1 (temp-vars "l" arity))
	 (vars2 (temp-vars "r" arity)))
    `(((,con ,@vars1) (,con ,@vars2))
      ,(if (eqv? arity 0)
	   (funcall nullary-fn)
	   (**foldr2 combine-fn (suspend-vars vars1) (suspend-vars vars2)
			   single-fn)))))

(define (**define/multiple fn args)
  (make valdef
	(lhs (**pat fn))
	(definitions
          (map (lambda (arg)
		 (make single-fun-def
		       (args (map (function **pat) (car arg)))
		       (rhs-list (list (make guarded-rhs
					     (guard (**omitted-guard))
					     (rhs (cadr arg)))))
		       (where-decls '())
		       (infix? '#f)))
	       args))))

(define (suspend-vars vars) (map (lambda (v) (lambda () (**var v))) vars))

(define (temp-vars root arity)
  (temp-vars1 root 1 arity))

(define (temp-vars1 root i arity)
  (if (> i arity)
      '()
      (cons (string->symbol (string-append root (number->string i)))
	    (temp-vars1 root (1+ i) arity))))
       
(define (tuple-con algdata)
  (car (algdata-constrs algdata)))

(define (con-string x)
  (remove-con-prefix (symbol->string (def-name x))))
