;;;  File: ast/module-structs   Author: John

;;; This contains AST structures which define the basic module structure.
;;; This is just the skeleton module structure: module, imports, exports,
;;; fixity, and default decls.

;;; AST nodes defined in the file:
;;;  module  import-decl  entity  entity-module  entity-var  entity-con
;;;  entity-class  entity-abbreviated  entity-datatype  fixity-decl



;;; All AST structs inherit from ast-node.  Not instantiated directly.
;;; The line-number is a back pointer to the source code.

(define-struct ast-node
  (type-template ast-td)
  (slots
   (line-number (type (maybe source-pointer)) (default '#f))))

(define-struct source-pointer
  (slots
   (line (type int))
   (file (type string))))

;;; <module> -> module <modid> [<exports>] where <body>
;;;          -> <body>
;;;
;;; <exports> -> ( <export>, ... <export> )
;;;
;;; <body>   -> { [<impdecls>;] [[<fixdecls>;] <topdecls> [;]] }
;;;          -> { <impdecls> [;] }
;;;
;;; <impdecls> -> <impdecl> ; ... ; <impdecl>
;;;
;;; <fixdecls> -> <fix> ; ... ; <fix>
;;;
;;; <topdecls> -> <topdecl> ; ... ; <topdecl>
;;;
;;; <topdecl> -> <synonym-decl>
;;;           -> <algdata-decl>
;;;           -> <class-decl>
;;;           -> <instance-decl>
;;;           -> <default-decl>
;;;           -> <sign-decl>
;;;           -> <valdef>

;;; The module struct is used to represent the program internally.  Binary
;;; files containing interface information contain these structures.
;;; Most compiler passes operate on this structure.  A table maps module
;;; names to this structure.  Within the module structure, local names are
;;; mapped to definitions.

;;; Modules are also used to represent interfaces & primitives.
;;; Some of the module fields may be blank for non-standard modules.

(define-struct module
  (include ast-node)
  (slots

    ;; These slots are required.

    (name (type symbol))
    (type (type (enum standard interface extension)))
    (prelude? (type bool) (default '#f))  ; True when symbols define the core
    (interface-module (type (maybe module)) (default '#f))
        ; link to previously compiled interface

    ;; The unit is filled in by the compilation system

    (unit (type symbol) (default '*undefined*))

    ;; The following slots are defined at parse time.
    ;; After a module is dumped, these are all empty.

    ;; <exports>, list of exported names
    (exports (type (list entity)) (default '()))
    ;; <impdecls>, local import decls
    (imports (type (list import-decl)) (default '()))
    ;; <fixdecls>, local fixity decls
    (fixities (type (list fixity-decl)) (default '()))
    ;; <synonym-decl>, local type synonym decls
    (synonyms (type (list synonym-decl)) (default '()))
    ;; <algdata-decl>, local data decls
    (algdatas (type (list data-decl)) (default '()))
    ;; <class-decl>, local class decls
    (classes (type (list class-decl)) (default '()))
    ;; <instance-decl>, local instance decls
    (instances (type (list instance-decl)) (default '()))
    ;; <default-decl>, default types
    (annotations (type (list annotation)) (default '()))
    (default (type (maybe default-decl)) (default '#f))
    ;; signatures, pattern, function bindings
    (decls (type (list decl)) (default '()))

    ;; These slots are filled in by the type-declaration-analysis phase
    ;; after conversion to definition form

    (synonym-defs (type (list synonym)) (default '()))
    (alg-defs (type (list algdata)) (default '()))
    (class-defs (type (list class)) (default '()))
    (instance-defs (type (list instance)) (default '()))


    ;; The import-export stage creates a set of tables which are used for
    ;; imports and exports and local name resolution.  All of these tables
    ;; are indexed by names.  These tables always deal with definitions.
    ;; Every variable, type, class, instance, and synonym is converted into
    ;; a definition.  Blank definitions are created early (in import/export)
    ;; and different aspects of the definitions are filled in as compilation
    ;; progresses.  The type-related definitions are filled in during
    ;; declaration analysis.  Only definitions are saved when a module is
    ;; written to a file; the ast information is not retained.

    ;; Used to avoid copy of Prelude symbols.
    (uses-standard-prelude? (type bool) (default '#f))
    ;; maps symbols in scope to definitions
    (symbol-table (type (table symbol def)) (default (make-table)))
    ;; maps names onto groups.
    (export-table (type (table symbol (list (tuple symbol def))))
		  (default (make-table)))
    ;; Note: symbol groups are found in classes and data decls.  An
    ;; entire group is denoted by the (..) abbreviation in an entity.
    ;; maps local names onto declared fixities
    (fixity-table (type (table symbol fixity)) (default (make-table)))
    ;; maps defs to local names
    (inverted-symbol-table (type (table symbol symbol)) (default (make-table)))
    ;; Used internally during import-export
    (fresh-exports (type (list (list (tuple symbol def)))) (default '()))
    (exported-modules (type (list module)) (default '()))

    ;; These slots are used to support incremental compilation.

    ;; vars defined in the module
    (vars (type (list var)) (default '()))
    ;; for incremental compilation
    (inherited-env (type (maybe module)) (default '#f))
    ;; The following slots are for interfaces only
    ;; These store renaming mappings defined in the import decls of
    ;; the interface.  Maps local name onto (module, original name).
    (interface-imports (type (list (tuple symbol (typle symbol symbol))))
		       (default '()))
    (interface-codefile (type (list string)) (default '()))
    ))


;;; <impdecl> -> import <modid> [<impspec>] [renaming <renamings>]
;;;
;;; <impspec> -> ( <import> , ... , <import> )
;;;           -> hiding ( <import> , ... , <import> )
;;;
;;; <import>  -> <entity>
;;;
;;; <renamings> -> ( <renaming>, ... , <renaming> )
;;;
;;; <renaming>  -> <varid> to <varid>
;;;             -> <conid> to <conid>

(define-struct import-decl
  (include ast-node)
  (slots
   ;; <modid>, module imported from
   (module-name (type symbol))
   ;; all: import Foo; by-name: import Foo(x) import Foo()
   (mode (type (enum all by-name)))
   ;; <impspec>, for mode = all this is the hiding list
   (specs (type (list entity)))
   ;; <renamings>, alist maps symbol -> symbol
   (renamings (type (list renaming)))
   ;; place to put corresponding module-ast; filled in by import/export.
   (module (type module) (uninitialized? #t))
   ))


;;; <entity> -> <modid> ..                              entity-module
;;           -> <varid>                                 entity-var
;;;          -> <tycon>                                 entity-con
;;;          -> <tycon> (..)                            entity-abbreviated
;;;          -> <tycon> ( <conid> , ... , <conid>)      entity-datatype
;;;          -> <tycls> (..)                            entity-abbreviated
;;;                note: this is indistinguishable from tycon (..)
;;;          -> <tycls> ( <varid> , ... , <varid>)      entity-class

(define-struct entity
  (include ast-node)
  (slots
    (name (type symbol))))

(define-struct entity-module
  (include entity)
  (predicate entity-module?)
  (slots
    ;; a direct pointer to the referenced module added later
    (module (type module) (uninitialized? #t))
    ))

(define-struct entity-var
  (include entity)
  (predicate entity-var?))

(define-struct entity-con
  (include entity)
  (predicate entity-con?))

(define-struct entity-abbreviated
  (include entity)
  (predicate entity-abbreviated?))

(define-struct entity-class
  (include entity)
  (predicate entity-class?)
  (slots
    (methods (type (list symbol)))))

(define-struct entity-datatype
  (include entity)
  (predicate entity-datatype?)
  (slots
    (constructors (type (list symbol)))))

(define-struct renaming
  (include ast-node)
  (slots
    (from (type symbol))
    (to (type symbol))
    (referenced? (type bool))))
		

;;; <fix> -> infixl [<digit>] <ops>
;;;       -> infixr [<digit>] <ops>
;;;       -> infix  [<digit>] <ops>
;;;
;;; <ops> -> <op> , ... , <op>
;;;
;;; <op>  -> <varop>
;;;       -> <conop>

;;; Not sure where to put this decl - jcp
(define-struct fixity
  (include ast-node)
  (slots
    (associativity (type (enum l n r)))
    (precedence (type int))))

(define-struct fixity-decl
  (include ast-node)
  (slots
    (fixity (type fixity))
    ;; <ops>
    (names (type (list symbol)))
    ))

