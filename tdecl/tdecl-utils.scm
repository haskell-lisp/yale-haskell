;;; This file contains routines which generate the code for the
;;; dictionaries used in the class system.

(define (make-sel-node size i)
  (**lambda '(x)
     (if (eqv? size 1)
	 (**var 'x)
	 (**sel (tuple-constructor size) (**var 'x) i))))

(define (make-compose f1 f2)
  (**lambda '(x)
      (**app f1 (**app f2 (**var 'x)))))

(define (make-new-var name)  ; name is a string
  (create-definition *module* (string->symbol name) 'var))

