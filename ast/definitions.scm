;;; File: ast/definitions.scm   Author: John

;;; this file contains definitions for the named entities in the
;;; system.  These are used in both the front and back ends of the
;;; compiler.  These are created early in the compilation process
;;; (import/export) and filled in during compilation.  Binary interface
;;; files are just tables mapping names to definitions.

;;; All definitions have these fields for managing name spaces.  All
;;; names are uniquified; this requires adding `;' to the front of data
;;; constructors to separate them from type constructors.  Module names
;;; do not have a `definition' data structure - the `module' structure
;;; serves the same purpose.

;;; Definitions are found in two places: the symbol tables which are part of
;;; the module structure and the -ref nodes in the ast structure.  The -ref
;;; nodes have two fields: a name (from the parser) and a field which will
;;; point to the associated definition after name resolution.  Name resolution
;;; happens in a number of different places: top level definitions are
;;; resolved during import-export, type declarations are resolved during
;;; type declaration analysis, and everything else is resolved during scoping
;;; (alpha conversion).  The parser generates pre-resolved -ref nodes when
;;; parsing some constructs.  These refs denote pre-defined language
;;; constructs, such as lists, tuples, or prelude functions.

;;; A special set of definitions constitutes the `core' of Haskell.  These
;;; definitions are pre-allocated and are filled in during the compilation
;;; of the Prelude.  This allows the bootstrap of the system.


;;; All defs require name, unit, and module args to make.
;;; Other slots should all have appropriate defaults.

(define-struct def
  (slots
   ;; the uniquified name (from the definition)
   (name (type symbol))
   ;; compilation unit defined in
   (unit (type symbol))
   ;; name of the defining module
   (module (type symbol))
   ;; used by the closure check
   (exported? (type bool) (default '#f) (bit #t))
   ;; for symbols in `core' Haskell; special case for IO
   (core? (type bool) (default '#f) (bit #t))      
   ;; Always a core sym.  Used to avoid putting in sym table
   (prelude? (type bool) (default '#f) (bit #t))
   ))



;;; Variable information

(define-struct var
  (include def)
  (predicate var?)
  (slots
   ;; inferred during type inference
   (type             (type (maybe ntype))     (default '#f))
   ;; type affixed by sign-decl or class decl
   (signature        (type (maybe ntype))     (default '#f))
   (interface-type   (type (maybe ntype))     (default '#f))
   ;; most variables have no fixity information.
   (fixity           (type (maybe fixity))    (default '#f))
   ;; The following attributes are used by the backend
   (selector-fn?     (type bool)              (default '#f) (bit #t))
   (force-strict?    (type bool)              (default '#f) (bit #t))
   (force-inline?    (type bool)              (default '#f) (bit #t))
   (toplevel?        (type bool)              (default '#f) (bit #t))
   (simple?          (type bool)              (default '#f) (bit #t))
   (strict?          (type bool)              (default '#f) (bit #t))
   (optimized-refs?  (type bool)              (default '#f) (bit #t))
   (standard-refs?   (type bool)              (default '#f) (bit #t))
   (single-ref       (type (maybe int))       (default '#f))
   (arity            (type int)               (default 0))
   (referenced       (type int)               (default 0))
   (value            (type (maybe flic-exp))  (default '#f))
   (fullname         (type (maybe symbol))    (default '#f))
   (inline-value     (type (maybe flic-exp))  (default '#f))
   ;; Only function bindings use these slots
   (strictness       (type (list bool))       (default '()))
   (complexity       (type (maybe int))       (default '#f))
   (optimized-entry  (type (maybe symbol))    (default '#f))
   (annotations      (type (list annotation-value)) (default '()))
   (fn-referenced    (type int)               (default 0))
   (arg-invariant-value  (type (maybe flic-exp))  (default '#f))
   (arg-invariant?   (type bool)              (default '#f) (bit #t))
   ))
  

;;; This defines an individual class method

(define-struct method-var
  (include var)
  (predicate method-var?)
  (slots
   (class (type class) (uninitialized? #t))
   (default (type (maybe var)) (uninitialized? #t))
   (method-signature (type signature) (uninitialized? #t))))


;;; A data constructor

(define-struct con
  (include def)
  (predicate con?)
  (slots
   ;; These slots are initialized in the type declaration phase
   (arity (type int) (uninitialized? #t))
   (types (type (list type)) (uninitialized? #t))
   (slot-strict? (type (list bool)) (default '()))
   (tag (type int) (uninitialized? #t))
   (alg (type algdata) (uninitialized? #t))
   (infix? (type bool) (bit #t) (default '#f))
   (signature (type ntype) (uninitialized? #t))
   ;; Assigned during import-export phase
   (fixity (type (maybe fixity)) (default '#f))
   (lisp-fns (type t) (default '()))
   ))


;;; Definitions used by the type system.

(define-struct tycon-def
  (include def)
  (slots
   (arity (type integer) (default -1))))

(define-struct synonym
  (include tycon-def)
  (predicate synonym?)
  (slots
   ;; These slots are explicitly initialized in the type declaration phase.
   (args (type (list symbol)) (uninitialized? #t))
   (body (type type) (uninitialized? #t))  ; stored in ast form
   ))

(define-struct algdata
  (include tycon-def)
  (predicate algdata?)
  (slots
   ;; These slots are initialized explicitly in the type declaration phase
   ;; number of constructors
   (n-constr (type int) (uninitialized? #t)) 
   (constrs (type (list con)) (uninitialized? #t))
   (context (type (list context)) (uninitialized? #t))
   ;; arguments to tycon
   (tyvars (type (list symbol)) (uninitialized? #t))
   ;; signature for the type as a whole
   (signature (type (maybe ntype)) (default '#f))
   ;; classes this algdata is an instance of
   (classes (type (list class)) (uninitialized? #t))
   ;; true if all constructors have 0 arity
   (enum? (type bool) (bit #t) (uninitialized? #t))
   ;; true when only constructor
   (tuple? (type bool) (bit #t) (uninitialized? #t))
   ;; true for `tuple-syntax' tuples.
   (real-tuple? (type bool) (bit #t) (uninitialized? #t))
   ;; instances to derive
   (deriving (type (list class)) (uninitialized? #t))
   (export-to-lisp? (type bool) (default '#f) (bit #t))
   (implemented-by-lisp? (type bool) (default '#f) (bit #t))
   ))

(define-struct class
  (include def)
  (predicate class?)
  (slots
   ;; These slots are initialized in the import-export phase
   (method-vars (type (list method-var)) (uninitialized? #t))
   ;; These slots are explicitly initialized in the type declaration phase
   ;; immediate superclasses
   (super (type (list class)) (uninitialized? #t))
   ;; all superclasses
   (super* (type (list class)) (uninitialized? #t))
   ;; name of class type variable
   (tyvar (type symbol) (uninitialized? #t))
   (instances (type (list instance)) (uninitialized? #t))
   (kind (type (enum standard numeric other)) (uninitialized? #t))
   (n-methods (type int) (uninitialized? #t))
   (dict-size (type int) (uninitialized? #t))
   (selectors (type (list (tuple method-var var))) (uninitialized? #t))
   ))

;;; Since instances are not named there is no need to include def.  

(define-struct instance
  (include ast-node)
  (slots
   ;; These slots always have initializers supplied with MAKE.
   (algdata (type algdata))
   (tyvars (type (list symbol)))
   (class (type class))
   (context (type (list context)))
   (gcontext (type (list (list class))))
   (dictionary (type var))

   ;; Explicitly initialized during the type declaration phase.
   (methods (type (list (tuple method-var var))) (uninitialized? #t))

   ;; These slots usually default on creation.
   (decls (type (list decl)) (default '()))
   ;; used during verification of derived instances
   (ok? (type bool) (bit #t) (default #f))
   ;; marks magically generated tuple instances
   (special? (type bool) (bit #t) (default #f))
   (suppress-readers? (type bool) (bit #t) (default #f))
   ))

