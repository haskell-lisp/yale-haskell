;;; flic.scm -- compilation unit for flic stuff
;;;
;;; author :  Sandra Loosemore
;;; date   :  7 Apr 1992
;;;


(define-compilation-unit flic
  (source-filename "$Y2/flic/")
  (unit flic-td
	(source-filename "flic-td.scm"))
  (unit flic-structs
	(source-filename "flic-structs.scm")
	(require flic-td))
  (unit print-flic
	(source-filename "print-flic.scm")
	(require flic-structs printer-support))
  (unit ast-to-flic
	(source-filename "ast-to-flic.scm")
	(require flic-structs ast haskell-utils))
  (unit flic-walker
	(source-filename "flic-walker.scm"))
  (unit copy-flic
	(source-filename "copy-flic.scm")
	(require flic-walker flic-structs))
  (unit invariant
	(source-filename "invariant.scm")
	(require flic-walker flic-structs))
  )
