;;; -- compilation unit definition for derived instances
;;;
;;; author :  John
;;;

(define-compilation-unit derived
  (source-filename "$Y2/derived/")
  (require global)
  (unit derived-instances
    (source-filename "derived-instances.scm"))
  (unit ast-builders
    (source-filename "ast-builders"))
  (unit eq-ord
    (source-filename "eq-ord"))
  (unit ix-enum
    (source-filename "ix-enum"))
  (unit text-binary
    (source-filename "text-binary"))
  )


