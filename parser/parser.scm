;;; parser.scm -- compilation unit definition for the lexer and parser
;;;
;;; author :  John
;;; date   :  10 Dec 1991
;;;

(define-compilation-unit parser
  (source-filename "$Y2/parser/")
  (require global)
  (unit parser-globals
    (source-filename "parser-globals.scm"))
  (unit parser-macros
    (source-filename "parser-macros.scm")
    (require parser-globals))
  (unit parser-errors
    (source-filename "parser-errors.scm")
    (require parser-macros))
  (unit lexer
    (source-filename "lexer.scm")
    (require parser-macros))
  (unit token
    (source-filename "token.scm")
    (require parser-macros))
  (unit parser-driver
    (source-filename "parser-driver.scm")
    (require parser-macros))
  (unit module-parser
    (source-filename "module-parser.scm")
    (require parser-macros))
  (unit interface-parser
    (source-filename "interface-parser.scm")
    (require parser-macros))
  (unit decl-parser
    (source-filename "decl-parser.scm")
    (require parser-macros))
  (unit type-parser
    (source-filename "type-parser.scm")
    (require parser-macros))
  (unit typedecl-parser
    (source-filename "typedecl-parser.scm")
    (require parser-macros))
  (unit exp-parser
    (source-filename "exp-parser.scm")
    (require parser-macros))
  (unit annotation-parser
    (source-filename "annotation-parser.scm")
    (require parser-macros))
  (unit pattern-parser
    (source-filename "pattern-parser.scm")
    (require parser-macros))
  (unit parser-debugger
    (source-filename "parser-debugger.scm")
    (require parser-macros)))

