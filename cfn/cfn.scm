;;; cfn.scm -- module definition for CFN phase
;;;
;;; author :  Sandra Loosemore
;;; date   :  11 Mar 1992
;;;


(define-compilation-unit cfn
  (source-filename "$Y2/cfn/")
  (require ast haskell-utils)
  (unit main
	(source-filename "main.scm"))
  (unit misc
	(source-filename "misc.scm")
	(require main))
  (unit pattern
	(source-filename "pattern.scm")
	(require main)))

	

