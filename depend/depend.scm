;;; depend.scm -- module definition for dependency analysis
;;;
;;; author :  John
;;; date   :  24 Mar 1992
;;;


(define-compilation-unit depend
  (source-filename "$Y2/depend/")
  (require ast haskell-utils)
  (unit dependency-analysis
	(source-filename "dependency-analysis.scm")))
	