;;; These are global variables used throughout the compiler.

;;; Configuration stuff

(define *prelude-unit-filename* "$PRELUDE/Prelude.hu")

(define *haskell-compiler-version* "Y2.0.5")
(define *haskell-compiler-update* "")


;;; Control over the init process
(define *haskell-initialized?* '#f)

;;; Error control
(define *break-on-error?* '#f)
(define *never-break?* '#f)

(define *runtime-abort* '())

(define *recoverable-error-handler* '())
(define *error-output-port* '())  ; initialized later

(define *context* '#f)  ; ast node being compiled.

(define *unit* '())

(define *standard-module-default* '())

(define *undefined-def* '())
(define *printer-class* '())
(define *printers* '(phase-time))

(define *all-printers*
  '(phase-time time compiling loading reading extension
    parse import type-decl scope depend
    type cfn depend2
    flic optimize optimize-extra strictness codegen codegen-flic
    dumper dump-stat))

;;; Global context stuff 
;;; ***This variable is actually only used by the parser.

(define *current-file* '())

(define *printed-tyvars* '())


;;; Used by the symbol table routines

(define *modules* '())  ; maps module name -> module structure
(define *module* '())   ; current module
(define *module-name* '())
(define *symbol-table* '())  ; part of the current module
(define *inverted-symbol-table* '())  ; maps def -> localname
(define *fixity-table* '())  ; name -> fixity
(define *suffix-table* '())  ; name -> int (for uniquifying names)

(define *special-parse-for-type-macros* '#f)

;;; These are for diagnostic purposes only

(define *big-let* '())

(define *show-end-of-phase* '#f)

;;; This is used to configure error messages & responses.

(define *emacs-mode* '#f)

;;; This is used to stash the Prelude symbol environment

(define *prelude-symbol-table* '())
(define *prelude-fixity-table* '())
(define *prelude-inverted-symbol-table* '())

