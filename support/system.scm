;;; system.scm -- haskell system setup
;;;
;;; author :  Sandra Loosemore
;;; date   :  22 Nov 1991
;;;
;;; This file loads in the compilation unit definition files for all
;;; of the components of the haskell system.  
;;;
;;; (The compilation unit facility is defined in support/shared/compile.scm.)


;;; First load the files containing module definitions.
;;; *** Add more files to the end of this list.  

(load "$Y2/support/support")
(load "$Y2/ast/ast")
(load "$Y2/top/top")
(load "$Y2/util/haskell-utils")
(load "$Y2/printers/printers")
(load "$Y2/parser/parser")
(load "$Y2/import-export/ie.scm")
(load "$Y2/tdecl/tdecl.scm")
(load "$Y2/derived/derived.scm")
(load "$Y2/prec/prec.scm")
(load "$Y2/depend/depend.scm")
(load "$Y2/type/type.scm")
(load "$Y2/cfn/cfn.scm")
(load "$Y2/flic/flic.scm")
(load "$Y2/backend/backend.scm")
(load "$Y2/runtime/runtime.scm")
(load "$Y2/csys/csys")
(load "$Y2/command-interface/command-interface")

;;; Define some functions to actually do the work.  The compilation unit 
;;; facility has conveniently kept a list of all of the unit definitions,
;;; so we can just rip through them in sequence.

(define (compile-haskell)
  (compile-and-load-unit-list compilation-units))

(define (recompile-haskell)
  (unless (null? remaining-units)
    (compile-and-load-unit-list remaining-units)))


(define (load-haskell)
  (load-unit-list compilation-units))

(define (reload-haskell)
  (unless (null? remaining-units)
    (load-unit-list remaining-units)))
