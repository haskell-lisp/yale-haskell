;;; logop-prims.scm -- primitives for logical operations on numbers
;;;
;;; author :  Sandra Loosemore
;;; date   :  19 Jun 1993
;;;


;;; Integer operations
;;; Note that bit counts are still guaranteed to be fixnums....

(define-syntax (logop.logior-integer i1 i2)
  `(the integer (lisp:logior (the integer ,i1) (the integer ,i2))))
(define-syntax (logop.logxor-integer i1 i2)
  `(the integer (lisp:logxor (the integer ,i1) (the integer ,i2))))
(define-syntax (logop.logand-integer i1 i2)
  `(the integer (lisp:logand (the integer ,i1) (the integer ,i2))))
(define-syntax (logop.logeqv-integer i1 i2)
  `(the integer (lisp:logeqv (the integer ,i1) (the integer ,i2))))
(define-syntax (logop.lognand-integer i1 i2)
  `(the integer (lisp:lognand (the integer ,i1) (the integer ,i2))))
(define-syntax (logop.lognor-integer i1 i2)
  `(the integer (lisp:lognor (the integer ,i1) (the integer ,i2))))
(define-syntax (logop.logandc1-integer i1 i2)
  `(the integer (lisp:logandc1 (the integer ,i1) (the integer ,i2))))
(define-syntax (logop.logandc2-integer i1 i2)
  `(the integer (lisp:logandc2 (the integer ,i1) (the integer ,i2))))
(define-syntax (logop.logorc1-integer i1 i2)
  `(the integer (lisp:logorc1 (the integer ,i1) (the integer ,i2))))
(define-syntax (logop.logorc2-integer i1 i2)
  `(the integer (lisp:logorc2 (the integer ,i1) (the integer ,i2))))
(define-syntax (logop.lognot-integer i1)
  `(the integer (lisp:lognot (the integer ,i1))))
(define-syntax (logop.logtest-integer i1 i2)
  `(the integer (lisp:logtest (the integer ,i1) (the integer ,i2))))
(define-syntax (logop.logbitp-integer i1 i2)
  `(the integer (lisp:logbitp (the fixnum ,i1) (the integer ,i2))))
(define-syntax (logop.ash-integer i1 i2)
  `(the integer (lisp:ash (the integer ,i1) (the fixnum ,i2))))
(define-syntax (logop.logcount-integer i1)
  `(the fixnum (lisp:logcount (the integer ,i1))))
(define-syntax (logop.integer-length-integer i1)
  `(the fixnum (lisp:integer-length (the integer ,i1))))


;;; Fixnum operations

(define-syntax (logop.logior-int i1 i2)
  `(the fixnum (lisp:logior (the fixnum ,i1) (the fixnum ,i2))))
(define-syntax (logop.logxor-int i1 i2)
  `(the fixnum (lisp:logxor (the fixnum ,i1) (the fixnum ,i2))))
(define-syntax (logop.logand-int i1 i2)
  `(the fixnum (lisp:logand (the fixnum ,i1) (the fixnum ,i2))))
(define-syntax (logop.logeqv-int i1 i2)
  `(the fixnum (lisp:logeqv (the fixnum ,i1) (the fixnum ,i2))))
(define-syntax (logop.lognand-int i1 i2)
  `(the fixnum (lisp:lognand (the fixnum ,i1) (the fixnum ,i2))))
(define-syntax (logop.lognor-int i1 i2)
  `(the fixnum (lisp:lognor (the fixnum ,i1) (the fixnum ,i2))))
(define-syntax (logop.logandc1-int i1 i2)
  `(the fixnum (lisp:logandc1 (the fixnum ,i1) (the fixnum ,i2))))
(define-syntax (logop.logandc2-int i1 i2)
  `(the fixnum (lisp:logandc2 (the fixnum ,i1) (the fixnum ,i2))))
(define-syntax (logop.logorc1-int i1 i2)
  `(the fixnum (lisp:logorc1 (the fixnum ,i1) (the fixnum ,i2))))
(define-syntax (logop.logorc2-int i1 i2)
  `(the fixnum (lisp:logorc2 (the fixnum ,i1) (the fixnum ,i2))))
(define-syntax (logop.lognot-int i1)
  `(the fixnum (lisp:lognot (the fixnum ,i1))))
(define-syntax (logop.logtest-int i1 i2)
  `(the fixnum (lisp:logtest (the fixnum ,i1) (the fixnum ,i2))))
(define-syntax (logop.logbitp-int i1 i2)
  `(the fixnum (lisp:logbitp (the fixnum ,i1) (the fixnum ,i2))))
(define-syntax (logop.ash-int i1 i2)
  `(the fixnum (lisp:ash (the fixnum ,i1) (the fixnum ,i2))))
(define-syntax (logop.logcount-int i1)
  `(the fixnum (lisp:logcount (the fixnum ,i1))))
(define-syntax (logop.integer-length-int i1)
  `(the fixnum (lisp:integer-length (the fixnum ,i1))))



