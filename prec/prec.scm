;;; prec.scm -- module definition for scoping/precedence-parsing phase
;;;
;;; author :  Sandra Loosemore
;;; date   :  13 Feb 1992
;;;


(define-compilation-unit prec
  (source-filename "$Y2/prec/")
  (require ast haskell-utils)
  (unit scope
	(source-filename "scope.scm"))
  (unit prec-parse
	(source-filename "prec-parse.scm")))


	

