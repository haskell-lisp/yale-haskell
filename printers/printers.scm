;;; printers.scm -- compilation unit definition for structure printers
;;;
;;; author :  Sandra Loosemore
;;; date   :  3 Jan 1992
;;;
;;;

(define-compilation-unit printer-support
  (source-filename "$Y2/printers/")
  (require global)
  (unit util
	(source-filename "util.scm")))

(define-compilation-unit printers
  (source-filename "$Y2/printers/")
  (require printer-support)
  (unit print-exps
	(source-filename "print-exps.scm"))
  (unit print-modules
	(source-filename "print-modules.scm"))
  (unit print-types
	(source-filename "print-types.scm"))
  (unit print-ntypes
	(source-filename "print-ntypes.scm"))
  (unit print-valdefs
	(source-filename "print-valdefs.scm"))
  )

