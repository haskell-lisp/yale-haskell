;;; -- compilation unit definition for type declaration analysis
;;;
;;; author :  John
;;;

(define-compilation-unit tdecl
  (source-filename "$Y2/tdecl/")
  (require global)
  (unit type-declaration-analysis
    (source-filename "type-declaration-analysis.scm"))
  (unit tdecl-utils
    (source-filename "tdecl-utils.scm"))
  (unit alg-syn
    (source-filename "alg-syn.scm"))
  (unit class
    (source-filename "class.scm"))
  (unit instance
    (source-filename "instance.scm")))
