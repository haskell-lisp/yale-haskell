;;; File: ast/valdef-structs    Author: John

;;; Ast structure for local declarations

;;; <decl> -> <signdecl>
;;;        -> <valdef>

;;; decl contains value declarations and type signatures.(
;;; type related decls are topdecls and are separated from
;;; these decls.

(define-struct decl   
  (include ast-node))
                      


;;; <signdecl> -> <vars> :: [<context> =>] <type>
;;;
;;; <vars>     -> <var> , ... , <var>
;;;

(define-struct signdecl ; this affixes a signature to a list of variables
  (include decl)
  (predicate signdecl?)
  (slots
   (vars (type (list var-ref)))
   (signature (type signature))))

;;; This is introduced into decl lists by dependency analysis
(define-struct recursive-decl-group
  (include decl)
  (slots
   ;; none of these are recursive decl groups
   (decls (type (list decl)))
   ))

;;; <valdef>  -> <lhs> = <exp> [where { <decls> [;] }]
;;;           -> <lhs> <gdrhs> [where { <decls> [;] }]
;;;
;;; <lhs>     -> <apat>
;;;           -> <funlhs>
;;;
;;; <funlhs>  -> <afunlhs>
;;;           -> <pat> <varop> <pat>
;;;           -> <lpat> <varop> <pat>
;;;           -> <pat> <varop> <rpat>
;;;
;;; <afunlhs> -> <var> <apat>
;;;           -> ( <funlhs> ) <apat>    (infix operator with more than 2 args)
;;;           -> <afunlhs> <apat>       (multiple argument pattern)

(define-struct valdef  ; this defines values.
  (include decl)
  (predicate valdef?)
  (slots
   ;; this pattern contains all new variables defined.
   ;; For a function definition the pattern will always
   ;; be a simple variable.
   (lhs (type pattern))
   ;; this is a list of right hand sides.
   ;; for a pattern definition, this list is always a singleton.  For
   ;; a function definition, there is a member for every successive
   ;; alternative for the function.
   (definitions (type (list single-fun-def)))
   ;; this is used internally by dependency analysis
   (depend-val (type int) (uninitialized? #t))
   ;; this is filled in by the type phase
   (dictionary-args (type (list var)) (uninitialized? #t))
   ;; used for defaulting
   (module (type symbol) (default '|Prelude|))
   ))

(define-struct single-fun-def
  (include ast-node)
  (slots
   ;; this list is always empty for pattern definition
   ;; and always non-empty for function definition.
   ;; The length of this list is the arity of the function.
   ;; All single-fun-defs for a function have the same arity.
   (args (type (list pattern)))
   ;; <gdrhs>, this contains a list of guard , expression pairs
   (rhs-list (type (list guarded-rhs)))
   ;; this contains declarations local to the
   ;; single fun def.  It scopes over the args.  The
   ;; guarded-rhs may refer to these values.
   (where-decls (type (list decl)))
   ;; true when declared in infix style.  Used for printing
   ;; and to check precs in prec parsing.
   (infix? (type bool) (bit #t))
   ))



;;; <gdrhs>   -> <gd> = <exp> [<gdrhs>]
;;;
;;; <gd>      -> | <exp>

(define-struct guarded-rhs ; a single guarded expression.  A special expression
  (include ast-node)
  (slots
   ;; node - omitted-guard - is used when no guard given
   (guard (type exp))
   (rhs (type exp))))


;;; Some examples of the above:
;;; (a,b) | z>y = (z,y)
;;;       | otherwise = (1,2)
;;;   where z = x-2
;;;
;;;  valdef:
;;;    lhs = (a,b)
;;;    definitions =
;;;       [single-fun-def:
;;;         args = []
;;;         rhs-list = [guarded-rhs: guard = z>y
;;;                                  rhs = (z,y),
;;;                     guarded-rhs: guard = otherwise
;;;                                  rhs = (1,2)]
;;;         where-decls = [valdef: lhs = z
;;;                                definitions =
;;;                                   [single-fun-def:
;;;                                      args = []
;;;                                      rhs-list = [guarded-rhs:
;;;                                                    guard = omitted-guard
;;;                                                    exp = x-2]
;;;                                      where-decls = []]]]
;;;
;;;  fact 0 = 1
;;;  fact (n+1) = (n+1)*fact n
;;;
;;;  valdef:
;;;    lhs = fact
;;;    definitions =
;;;       [single-fun-def:
;;;         args = [0]
;;;         rhs-list = [guarded-rhs: guard = omitted-guard
;;;                                  rhs = 1]
;;;         where-decls = [],
;;;        single-fun-def:
;;;         args = [n+1]
;;;         rhs-list = [guarded-rhs: guard = omitted-guard
;;;                                  rhs = (n+1)*fact n]
;;;         where-decls = []]




;;; Definitions for patterns

;;; This is a simplification; the real syntax is complicated by
;;; rules for precedence and associativity.
;;;
;;; <pat>   -> <pat> <conop> <pat>           pcon
;;;         -> <pat> + <integer>             plus-pat
;;;         -> - <integer-or-float>          *** ???  const-pat?
;;;         -> <apat>
;;;         -> <con> <apat> .... <apat>      pcon
;;;
;;; <apat>  -> <var>                         var-pat
;;;         -> <var> @ <apat>                as-pat
;;;         -> <con>                         *** ??? var-pat?
;;;         -> <literal>                     const-pat
;;;         -> _                             wildcard-pat
;;;         -> ()                            pcon special case
;;;         -> ( <pat> )                     (grouping syntax)
;;;         -> ( <pat> , ... , <pat> )       pcon special case
;;;         -> [ <pat> , ... , <pat> ]       list-pat
;;;         -> ~ <apat>                      irr-pat

(define-struct pattern
  (include ast-node))

(define-struct apat
  (include pattern))

(define-struct as-pat  ;; var@pat
  (include apat)
  (slots
   (var (type var-ref))
   (pattern (type pattern))))

(define-struct irr-pat ;; ~pat
  (include apat)
  (slots
   (pattern (type pattern))))

(define-struct var-pat  ;; v
  (include apat)
  (predicate var-pat?)
  (slots
   (var (type var-ref))))

(define-struct wildcard-pat  ;; _
  (include apat)
  (predicate wildcard-pat?))

(define-struct const-pat  ;; literal
  (include apat)
  (predicate const-pat?)
  (slots
   (value (type const))
   ;; this is the code that actually performs the match.
   ;; it's filled in by type phase.
   (match-fn (type exp) (uninitialized? #t))))

(define-struct plus-pat  ;; p+k
  (include pattern)
  (slots
   (pattern (type pattern))
   (k (type integer))
   ;; code to check for match, filled in by type phase
   (match-fn (type exp) (uninitialized? #t))
   ;; code to bind result, filled in by type phase
   (bind-fn (type exp) (uninitialized? #t))
   ))

(define-struct pcon      ;; con pat1 pat2 ...
  (include pattern)      ;; pat1 con pat2
  (predicate pcon?)
  (slots
   (name (type symbol))
   (con (type def))
   (pats (type (list pattern)))
   (infix? (type bool) (bit #t))))

(define-struct list-pat   ;; [p1,p2,...]
  (include apat)
  (slots
   (pats (type (list pattern)))))

;;; The following structs deal with prec parsing of patterns.

(define-struct pp-pat-list
  (include pattern)
  (slots
   (pats (type (list pattern)))))

(define-struct pp-pat-plus
  (include pattern)
  (predicate pp-pat-plus?))

(define-struct pp-pat-negated
  (include pattern)
  (predicate pp-pat-negated?))



;;; Structs for annotations

(define-struct annotation
  (include decl)
  (predicate annotation?))

(define-struct annotation-decl
  (include annotation)
  (predicate annotation-decl?)
  (slots
   (names (type (list symbol)))
   (annotations (type (list annotation-value)))))

(define-struct annotation-value
  (include annotation)
  (predicate annotation-value?)
  (slots
   (name (type symbol))
   (args (type (list t)))))

;;; This is a list of annotations placed in where decls lists in the same
;;; manner a signdecls.

(define-struct annotation-decls
  (include annotation)
  (predicate annotation-decls?)
  (slots
    (annotations (type (list annotation)))))
