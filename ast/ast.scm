;;; ast.scm -- compilation unit definition for ast definitions
;;;
;;; author :  John
;;; date   :  10 Dec 1991
;;;


(define-compilation-unit ast
  (source-filename "$Y2/ast/")
  (unit ast-td
    (source-filename "ast-td"))
  (unit modules
    (source-filename "modules.scm")
    (require ast-td))
  (unit type-structs
    (source-filename "type-structs.scm")
    (require ast-td modules))
  (unit tc-structs
    (source-filename "tc-structs.scm")
    (require ast-td modules))
  (unit valdef-structs
    (source-filename "valdef-structs.scm")
    (require ast-td modules))
  (unit definitions
    (source-filename "definitions.scm")
    (require ast-td modules))
  (unit exp-structs
    (source-filename "exp-structs.scm")
    (require ast-td modules))
  (unit predicates
    (require ast-td modules type-structs valdef-structs definitions
	     exp-structs tc-structs)
    (source-filename "predicates.scm")))
