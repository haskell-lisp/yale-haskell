;;; array-prims.scm -- array primitives
;;;
;;; author :  John & Sandra
;;; date   :  14 May 1993


;;; Vector reference, returning unboxed value

(define-syntax (prim.vector-sel vec i)
  `(vector-ref ,vec ,i))


;;; Destructive vector update.  All arguments are unboxed.

(define-syntax (prim.vector-update vec i newval)
  `(setf (vector-ref ,vec ,i) ,newval))


;;; Make a vector whose elements are initialized to val (which is boxed).

(define-syntax (prim.make-vector size val)
  `(make-vector ,size ,val))


;;; Copy an existing vector.

(define-syntax (prim.copy-vector vec)
  `(vector-copy ,vec))


;;; Explicit force operation

(define-syntax (prim.force x)
  `(force ,x))


;;; The first parameter is forced first since this prim is declared to
;;; be strict in the first arg.

(define-syntax (prim.strict1 force-this leave-this)
  `(begin
     ;; Can't ignore the first argument entirely since doing so
     ;; might result in variable-bound-but-not-referenced errors.
     ;; Hopefully the Lisp compiler will be smart enough to get
     ;; rid of this when appropriate.
     ,force-this
     ;; Don't generate a stupid (force (delay x)) sequence here if
     ;; we don't need to.
     ,(if (and (pair? leave-this)
	       (or (eq? (car leave-this) 'delay)
		   (eq? (car leave-this) 'box)))
	  (cadr leave-this)
	  `(force ,leave-this))))


