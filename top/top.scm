;;; top.scm -- compilation unit definition for the top level

;;; Global includes the ast definitions and all global data structures
;;; used in the compiler.

(define-compilation-unit global
  (source-filename "$Y2/top/")
  (require ast)
  (unit has-utils
    (source-filename "has-utils.scm"))
  (unit core-definitions
    (require has-utils)
    (source-filename "core-definitions.scm"))
  (unit core-symbols
    (require core-definitions)
    (source-filename "core-symbols.scm"))
  (unit core-init
    (require core-symbols)
    (source-filename "core-init.scm"))
  (unit globals
    (require core-init)
    (source-filename "globals.scm"))
  (unit has-macros
    (source-filename "has-macros.scm"))
  )


;;; These files do not need to be required by other units  

(define-compilation-unit top-level
  (source-filename "$Y2/top/")
  (require global)
  (unit phases
    (source-filename "phases.scm"))
  (unit system-init
    (source-filename "system-init.scm"))
  (unit errors
    (source-filename "errors.scm"))
  (unit tuple
    (source-filename "tuple.scm"))
  (unit symbol-table
    (source-filename "symbol-table.scm"))
  )
     


