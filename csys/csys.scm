;;; csys.scm -- compilation unit definition for the compilation system

(define-compilation-unit csys
  (source-filename "$Y2/csys/")
  (require global runtime flic)
  (unit cache-structs
    (source-filename "cache-structs.scm"))
  (unit compiler-driver
    (require cache-structs)
    (source-filename "compiler-driver.scm"))
  (unit dump-params
    (require cache-structs)
    (source-filename "dump-params.scm"))
  (unit dump-macros
    (require dump-params)
    (source-filename "dump-macros.scm"))
  (unit dump-interface
    (require dump-macros)
    (source-filename "dump-interface.scm"))
  (unit dump-flic
    (require dump-macros)
    (source-filename "dump-flic.scm"))
  (unit dump-cse
    (require dump-macros)
    (source-filename "dump-cse.scm")))
