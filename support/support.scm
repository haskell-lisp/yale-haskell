;;; support.scm -- load support files shared by all systems
;;;
;;; author :  Sandra Loosemore
;;; date   :  28 Oct 1991
;;;
;;;


;;; Keep track of all compilation units defined.
;;; This has to go here and not in compile.scm because we don't want this
;;; list reinitialized every time that file is loaded.

(define compilation-units '())


;;; Load this file first; it defines the basic compilation system support.
;;; It doesn't matter if this ends up loading source because we'll compile
;;; and reload it below.  

(load "$Y2/support/compile.scm")


;;; Define a real compilation unit for shared support files.

(define-compilation-unit support
  (source-filename "$Y2/support/")
  (unit compile (source-filename "compile.scm"))
  (unit utils   (source-filename "utils.scm"))
  (unit xp
	(unit pprint (source-filename "pprint.scm"))
	(unit format (source-filename "format.scm")
	      (require pprint)))
  )


