;;;  File: ast/type-structs   Author: John

;;; This contains AST structures for the type-related declarations,
;;; including `data', `class', `instance', and `type' decls.  Basic type
;;; syntax is also defined here.

;;; Structures declared here:
;;;  type  type-var  type-con  context  signature  synonym-decl
;;;  data-decl  class-decl  instance-decl


;;; <type>  -> <atype>
;;;         -> <type> -> <type>                              ***
;;;         -> <tycon> <atype> ... <atype>                   tycon
;;;
;;; <atype> -> <tyvar>                                       tyvar
;;;         -> <tycon>                                       tycon
;;;         -> ()                                            ***
;;;         -> ( <type> )                                    grouping syntax
;;;         -> ( <type> , ... , <type>)                      ***
;;;         -> [ <type> ]                                    ***
;;; *** Special <tycon> cases

;;; Type with no context - either a tyvar or a constructor
(define-struct type
  (include ast-node))

(define-struct tyvar
  (include type)
  (predicate tyvar?)
  (slots
   (name (type symbol))))

(define-struct tycon
  (include type)
  (predicate tycon?)
  (slots
   (name (type symbol))
   (def (type def))
   (args (type (list type)))))

;;; <signature> -> [<context> =>] <type>
;;;
;;; <context> -> <class>
;;;           -> (<class> , ... , <class>)

;;; A single class, variable pair
(define-struct context
  (include ast-node)
  (slots
   (class (type class-ref))
   (tyvar (type symbol))))


;;; Type + context
(define-struct signature
  (include type)
  (slots
   (context (type (list context)))
   (type (type type))))


;;; Major type declarations.  Note: no explicit structures for <simple>
;;; or <inst> are needed - these are just special cases of type.

;;; <synonym-decl> -> type <simple> = <type>
;;;
;;; <simple> -> <tycon> <tyvar> ... <tyvar>

(define-struct synonym-decl
  (include ast-node)
  (slots
   (simple (type type))
   (body (type type))))


;;; <aldata-decl> -> data [<context> => ] <simple> = <constrs> 
;;;                    [deriving <tycls> | ( <tycls> , ... <tycls>) ]
;;;
;;; <constrs>     -> <constr> | ... | <constr>
;;;

(define-struct data-decl
  (include ast-node)
  (slots
   (context (type (list context)))
   (simple (type type))
   (constrs (type (list constr)))  
   (deriving (type (list class-ref)))
   (annotations (type (list annotation-value)))))

;;; <constr>      -> <con> <atype> ... <atype>
;;;               -> <type> <conop> <type>

(define-struct constr
  (include ast-node)
  (slots
   (constructor (type con-ref))  ; this con-ref has an infix? flag.
   (types (type (list (tuple type (list annotation-value)))))))


;;; <class-decl> -> class [<context> => ] <class> [where { <cbody> [;] } ]
;;;
;;; <cbody> -> [<csigns> ; ] [ <valdefs> ]
;;;
;;; <csigns> -> <signdecl> ; ... ; <signdecl>

(define-struct class-decl
  (include ast-node)
  (slots
   (class (type class-ref))
   (super-classes (type (list context)))
   (class-var (type symbol))              ; name of type var for this class in decls
   (decls (type (list decl)))))           ; <cbody>


;;; <instance-decl> -> instance [<context> =>] <tycls> <inst>
;;;                      [where { <valdefs> [;] } ]
;;;
;;; <inst> -> <tycon>
;;;        -> ( <tycon> <tyvar> ... <tyvar> )
;;;        -> ( <tyvar> , ... , <tyvar>)
;;;        -> ()
;;;        -> [ <tyvar> ]
;;;        -> ( <tyvar> -> <tyvar>)
;;;

(define-struct instance-decl
  (include ast-node)
  (slots
   ;; <context>
   (context (type (list context)))
   ;; <tycls>
   (class (type class-ref))
   ;;
   (simple (type type))
   ;; <valdefs>
   (decls (type (list valdef)))
   ))



;;; <default-decl> -> default <type>
;;;                -> default ( <type> , ... , <type> )

(define-struct default-decl
  (include ast-node)
  (slots
   (types (type (list type)))))


;;; <tycls> -> <aconid>

(define-struct class-ref
  (include ast-node)
  (slots
   (name (type symbol))
   (class (type def))))

