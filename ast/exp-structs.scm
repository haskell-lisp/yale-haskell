;;; File: ast/exp-structs     Author: John

;;; These ast structures define the expression syntax


;;; This is simplified; there are additional rules for associativity and
;;; precedence.
;;;
;;; <exp>  -> <lambda-exp>
;;;        -> <let-exp>
;;;        -> <if-exp>
;;;        -> <case-exp>
;;;        -> <signature-exp>
;;;        -> <exp> <op> <exp>           ; treated like <fn-app>
;;;        -> - <exp>
;;;        -> <fn-app>
;;;        -> <aexp>
;;;

(define-struct exp
  (include ast-node))


;;; <lambda-exp> -> \ <apat> ... <apat> -> <exp>

(define-struct lambda
  (include exp)
  (slots
   (pats (type (list pattern)))
   (body (type exp))))

;;; <let-exp> -> let { <decls> [;] } in <exp>

(define-struct let
  (include exp)
  (slots
   (decls (type (list decl)))
   (body (type exp))))

;;; <if-exp> -> if <exp> then <exp> else <exp>

(define-struct if
  (include exp)
  (slots
   (test-exp (type exp))
   (then-exp (type exp))
   (else-exp (type exp))))


;;; <case-exp> -> case <exp> of { <alts> [;] }
;;;
;;; <alts>     -> <alt> ; ... ; <alt>
;;;
;;; <alt>      -> <pat> -> exp  [where { <decls> [;] } ]
;;;            -> <pat> <gdpat> [where { <decls> [;] } ]

(define-struct case
  (include exp)
  (slots
   (exp (type exp))
   (alts (type (list alt)))))

(define-struct alt
  (include ast-node)
  (slots
   (pat (type pattern))
   ;; defined in valdef-structs
   (rhs-list (type (list guarded-rhs)))
   (where-decls (type (list decl)))
   ;; used internally by cfn
   (test (type (maybe exp)) (default '#f))
   ))

;;; <signature-exp> -> <exp> :: [<context> =>] <atype>

(define-struct exp-sign
  (include exp)
  (slots
   (exp (type exp))
   (signature (type signature))))


;;; <fn-app> -> <exp> <aexp>

(define-struct app
  (include exp)
  (predicate app?)
  (slots
   (fn (type exp))
   (arg (type exp))))

;;; <aexp> -> <var>                                var-ref
;;;        -> <con>                                con-ref
;;;        -> <literal>                            const
;;;        -> ()                                   constructor is Unit
;;;        -> ( <exp> )                            
;;;        -> ( <exp> , ... , <exp> )              constructor is a tuple
;;;        -> [ <exp> , ... , <exp> ]              list
;;;        -> <sequence>
;;;        -> [exp> | <qual> , ... , <qual>]       list-comp
;;;        -> ( <exp> <op> )                       section-r
;;;        -> ( <op> <exp> )                       section-l
;;;

(define-struct aexp
  (include exp))


(define-struct var-ref
  (include aexp)
  (predicate var-ref?)
  (slots
   (name (type symbol))
   (var (type def))
   (infix? (type bool) (bit #t))))

(define-struct con-ref
  (include aexp)
  (predicate con-ref?)
  (slots
   (name (type symbol))
   (con (type def))
   (infix? (type bool) (bit #t))))

(define-struct const
  (include aexp)
  (slots
   (overloaded? (type bool) (default '#t) (bit #t))))

(define-struct integer-const
  (include const)
  (predicate integer-const?)
  (slots
   (value (type integer))))

(define-struct float-const
  (include const)
  (predicate float-const?)
  (slots
   (numerator (type integer))
   (denominator (type integer))
   (exponent (type integer))))

(define-struct char-const
  (include const)
  (predicate char-const?)
  (slots
   (value (type char))))

(define-struct string-const
  (include const)
  (predicate string-const?)
  (slots
   (value (type string))))

(define-struct list-exp
  (include aexp)
  (slots
   (exps (type (list exp)))))


;;; <sequence> -> [ <exp> .. ]                  sequence
;;;            -> [ <exp>, <exp> .. ]           sequence-then
;;;            -> [ <exp> .. <exp> ]            sequence-to
;;;            -> [ <exp>, <exp> .. <exp> ]     sequence-then-to

(define-struct sequence
  (include aexp)
  (slots
   (from (type exp))))

(define-struct sequence-to
  (include aexp)
  (slots
   (from (type exp))
   (to (type exp))))


(define-struct sequence-then
  (include aexp)
  (slots
   (from (type exp))
   (then (type exp))))

(define-struct sequence-then-to
  (include aexp)
  (slots
   (from (type exp))
   (then (type exp))
   (to (type exp))))

(define-struct list-comp
  (include aexp)
  (slots
   (exp (type exp))
   (quals (type (list qual)))))

;;; Op on left
(define-struct section-l
  (include aexp)
  (slots
   (exp (type exp))
   (op (type exp))))  ; either con-ref or var-ref

(define-struct section-r
  (include aexp)
  (slots
   (exp (type exp))
   (op (type exp))))  ; either con-ref or var-ref

;;; <qual> -> <pat> <- <exp>
;;;        -> <exp>

(define-struct qual
  (include ast-node))

(define-struct qual-generator
  (include qual)
  (slots
   (pat (type pattern))
   (exp (type exp))))

(define-struct qual-filter
  (include qual)
  (slots
   (exp (type exp))))


;;; This is used as the guard slot in a guarded-rhs to represent lack of a
;;; guard.  This is the same as True.

(define-struct omitted-guard ; same as True; should print in the guardless form
  (include exp))


;;; These structures are used by the precedence parser.  

(define-struct pp-exp-list  ; list of expressions & ops for the prec parser
  (include exp)
  (slots
   (exps (type (list exp)))))

;; This is a place holder for unary negation in pp-exp expressions.  It is
;; changed to call the negate function by the prec parser

(define-struct negate
  (include exp)
  (predicate negate?))

;; Note: operators are var / con structures with infix? set to #t

;;; The following ast nodes do not directly correspond to Haskell syntax.
;;; They are generated during internal code transformations.

;;; This returns a number (an Int) associated with the constructor of a
;;; value.

(define-struct con-number
  (include exp)
  (slots
    (type (type algdata))
    (value (type exp))))

;;; This selects a value (denoted by the Int in slot) from a data object
;;; created by a specified constructor.

(define-struct sel
  (include exp)
  (slots
    (constructor (type con))
    (slot (type int))
    (value (type exp))))

;;; This returns True if the data value was built with the designated
;;; constructor

(define-struct is-constructor
  (include exp)
  (slots 
   (constructor (type con))
   (value (type exp))))

;;; this is for the type checker only.  It turns off
;;; type checking for the argument.

(define-struct cast
  (include exp)     
  (slots 
   (exp (type exp))))

;; this is used as the body of the let generated by
;; dependency analysis

(define-struct void  
  (include exp)
  (predicate void?))
  

;;; These structures are for the type checker.  They serve as a placeholder
;;; for values which will evaluate to methods or dictionaries.

(define-struct placeholder
  (include exp)
  (predicate placeholder?)
  (slots
   (exp (type (maybe exp)))
   (tyvar (type ntype))
   (overloaded-var (type exp))
   (enclosing-decls (type (list decl)))))

(define-struct method-placeholder
  (include placeholder)
  (predicate method-placeholder?)
  (slots
   ;; the method to be dispatched
   (method (type method-var))
   ))

(define-struct dict-placeholder
  (include placeholder)
  (predicate dict-placeholder?)
  (slots
   ;; the class of dictionary needed
   (class (type class))))

(define-struct recursive-placeholder
  (include exp)
  (slots
   (var (type var))
   (enclosing-decls (type (list decl)))
   ;; this holds the code associated with recursive
   ;; functions or variables.  This code instantiates
   ;; the recursive context if necessary.
   (exp (type (maybe exp)))
   ))

;;; This is used in primitive modules only.  It holds the definition of
;;; a lisp level primitive.

(define-struct prim-definition
  (include exp)
  (slots
   (lisp-name (type symbol))
   (atts (type (list (tuple symbol t))))))

;;; This is used by the type checker to hang on to the original
;;; version of a program for message printing.  This is removed by
;;; the cfn pass.

(define-struct save-old-exp
  (include exp)
  (slots
   (old-exp (type exp))
   (new-exp (type exp))))


;;; This is used for type checking overloaded methods.

(define-struct overloaded-var-ref
  (include exp)
  (slots
    (var (type var))
    (sig (type ntype))))



;;; These are used by the CFN.


(define-struct case-block
  (include exp)
  (slots
    (block-name (type symbol))
    (exps       (type (list exp)))))

(define-struct return-from
  (include exp)
  (slots
    (block-name (type symbol))
    (exp        (type exp))))

(define-struct and-exp
  (include exp)
  (slots
    (exps       (type (list exp)))))

