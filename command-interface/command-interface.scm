;;; csys.scm -- compilation unit definition for the compilation system

(define-compilation-unit command-interface
  (source-filename "$Y2/command-interface/")
  (require global)
  (unit command
    (source-filename "command.scm"))
  (unit command-utils
    (source-filename "command-utils.scm"))
  (unit incremental-compiler
    (source-filename "incremental-compiler.scm")))
