;;; cl-types.lisp -- type-related stuff
;;;
;;; author :  Sandra Loosemore
;;; date   :  5 Oct 1992
;;;


;;; Export CL symbols for type names

(define-mumble-import t)

#+lucid
(define-mumble-type mumble::procedure () 'system::procedure)
#+(or cmu akcl allegro mcl lispworks)
(define-mumble-type mumble::procedure () 'function)
#+wcl
(define-mumble-type mumble::procedure () 'lisp:procedure)
#-(or lucid cmu akcl allegro mcl lispworks wcl)
(missing-mumble-definition procedure)

(define-mumble-type mumble::pair () 'cons)

(define-mumble-import null)

(define-mumble-type mumble::list (&optional element-type)
  ;; *** Common Lisp provides no way to make use of the element type
  ;; *** without using SATISFIES.
  (declare (ignore element-type))
  'list)

(define-mumble-import symbol)

(define-mumble-type mumble::char () 'character)
(define-mumble-type mumble::string () 'simple-string)
(define-mumble-type mumble::vector () 'simple-vector)

(define-mumble-import number)
(define-mumble-import integer)
(define-mumble-import rational)
(define-mumble-import float)
(define-mumble-import fixnum)

(define-mumble-type mumble::int () 'fixnum)

(define-mumble-type mumble::table (&optional key-type value-type)
  ;; *** Common Lisp provides no way to make use of the element type
  ;; *** without using SATISFIES.
  (declare (ignore key-type value-type))
  'hash-table)


;;; Extensions

(define-mumble-type mumble::enum (&rest values)
  `(member ,@values))

(define-mumble-type mumble::tuple (&rest element-types)
  ;; *** Common Lisp provides no way to make use of the element type
  ;; *** without using SATISFIES.
  (let ((n  (length element-types)))
    (cond ((< n 2)
	   (error "Too few arguments to TUPLE type specifier."))
	  ((eql n 2)
	   'cons)
	  (t
	   'simple-vector))))

(define-mumble-type mumble::bool () 't)

(define-mumble-type mumble::alist (&optional key-type value-type)
  `(mumble::list (tuple ,key-type ,value-type)))

(define-mumble-type mumble::maybe (type)
  `(or ,type null))



;;; Functions, etc.

(define-mumble-import the)
(define-mumble-synonym mumble::subtype? subtypep)

(define-mumble-function-inline mumble::is-type? (type object)
  (typep object type))

(define-mumble-macro mumble::typecase (data &rest cases)
  (let ((last  (car (last cases))))
    (if (eq (car last) 'mumble::else)
	`(typecase ,data ,@(butlast cases) (t ,@(cdr last)))
	`(typecase ,data ,@cases))))
