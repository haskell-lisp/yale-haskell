(define-compilation-unit ie
  (source-filename "$Y2/import-export/")
  (require global)
  (unit ie-utils
    (source-filename "ie-utils"))
  (unit import-export
    (source-filename "import-export"))
  (unit init-modules
    (source-filename "init-modules"))
  (unit top-definitions
    (source-filename "top-definitions"))
  (unit locate-entity
    (source-filename "locate-entity"))
  (unit ie-errors
    (source-filename "ie-errors")))

