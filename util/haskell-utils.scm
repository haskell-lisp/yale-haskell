(define-compilation-unit haskell-utils
  (source-filename "$Y2/util/")
  (require global)
  (unit constructors
    (source-filename "constructors.scm"))
  (unit prec-utils
    (source-filename "prec-utils.scm"))
  (unit walk-ast
    (source-filename "walk-ast.scm"))
  (unit pattern-vars
    (source-filename "pattern-vars.scm")    
    (require walk-ast))
  (unit instance-manager
    (source-filename "instance-manager.scm"))
  (unit signature
    (source-filename "signature.scm"))
  (unit type-utils
    (source-filename "type-utils.scm"))
  (unit annotation-utils
    (source-filename "annotation-utils.scm"))
  )

