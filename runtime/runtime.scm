;;; runtime.scm
;;;
;;; author :  John
;;;


(define-compilation-unit runtime
  (source-filename "$Y2/runtime/")
  (require global)
  (unit runtime-utils
	(source-filename "runtime-utils.scm"))
  (unit prims
	(require runtime-utils)
	(source-filename "prims.scm"))
  (unit io-primitives
	(require runtime-utils)
	(source-filename "io-primitives.scm"))
  (unit array-prims
	(require runtime-utils)
	(source-filename "array-prims.scm"))
  (unit debug-utils
	(require runtime-utils)
	(source-filename "debug-utils.scm"))
  (unit tuple-prims
        (require runtime-utils)
	(source-filename "tuple-prims.scm")))
