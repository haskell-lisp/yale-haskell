;;; backend.scm -- compilation unit for code generator stuff
;;;
;;; author :  Sandra Loosemore
;;; date   :  13 May 1992
;;;


(define-compilation-unit backend
  (source-filename "$Y2/backend/")
  (require flic)
  (unit optimize
	(source-filename "optimize.scm"))
  (unit strictness
	(source-filename "strictness.scm"))
  (unit box
	(source-filename "box.scm"))
  (unit codegen
	(source-filename "codegen.scm"))
  (unit interface-codegen
	(source-filename "interface-codegen.scm")))

