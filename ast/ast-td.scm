;;; ast-td.scm -- define ast type descriptor object
;;;
;;; author :  Sandra Loosemore
;;; date   :  6 Oct 1992
;;;


;;; Give the type descriptors for AST nodes extra slots to hold walker
;;; functions.

(define-struct ast-td
  (include type-descriptor)
  (slots
    (cfn-walker (type (maybe procedure)) (default '#f))
    (cfn-simple-transform-walker (type (maybe procedure)) (default '#f))
    (depend-walker (type (maybe procedure)) (default '#f))
    (ast-to-flic-walker (type (maybe procedure)) (default '#f))
    (scope-walker (type (maybe procedure)) (default '#f))
    (type-walker (type (maybe procedure)) (default '#f))
    (collect-pattern-vars-walker (type (maybe procedure)) (default '#f))))
