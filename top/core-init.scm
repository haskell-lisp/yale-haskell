

(define *core-symbols* '())
(define *prelude-core-symbols* '())

; expands into lots of (define *core-??* '())

(define-core-variables)

(define (init-core-symbols)
  (setf *core-symbols* (make-table))
  (setf *prelude-core-symbols* (make-table))
  (create-core-globals))

