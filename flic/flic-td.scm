;;; flic-td.scm -- define type descriptor for flic structs
;;;
;;; author :  Sandra Loosemore
;;; date   :  6 Oct 1992
;;;

(define-struct flic-td
  (include type-descriptor)
  (slots
    (codegen-walker (type (maybe procedure)) (default '#f))
    (optimize-walker (type (maybe procedure)) (default '#f))
    (postoptimize-walker (type (maybe procedure)) (default '#f))
    (fun-strictness-walk-walker (type (maybe procedure)) (default '#f))
    (var-strictness-walk-walker (type (maybe procedure)) (default '#f))
    (compute-strictness-walk-walker (type (maybe procedure)) (default '#f))
    (print-strictness-walker (type (maybe procedure)) (default '#f))
    (box-analysis-walker (type (maybe procedure)) (default '#f))
    (copy-flic-walker (type (maybe procedure)) (default '#f))
    (dump-flic-walker (type (maybe procedure)) (default '#f))
    (flic-invariant?-walker (type (maybe procedure)) (default '#f))
    ))
