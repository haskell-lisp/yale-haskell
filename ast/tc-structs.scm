;;; These structures are used by the type checker for the internal
;;; representation of type information.  These are referred to in
;;; general as `ntype' structures.  Conversions are required between
;;; ast types and ntypes.

(define-struct ntype
  (include ast-node))

(define-struct ntycon
  (include ntype)
  (predicate ntycon?)
  (slots
   (tycon (type def))
   (args (type (list ntype)))))

(define-struct ntyvar
  (include ntype)
  (predicate ntyvar?)
  (slots
   ;; non-instantiated tyvars use #f for a value.
   (value (type (maybe ntype)))
   ;; could be encoded in value.
   (context (type (list class)) (default ()))
   (read-only? (type bool) (default #f) (bit #t))
   (dict-params (type (list (tuple valdef (list (tuple class var))))))
   ))

;;; This is used only at the top level of a type during letrec type
;;; checking.

(define-struct recursive-type
  (include ntype)
  (predicate recursive-type?)
  (slots
   (type (type ntype))
   (placeholders (type (list exp)))))

;;; Gtypes are generalized types which can be copied quickly & stored in
;;; interfaces.  They may contain monomorphic type variables which will not
;;; be copied.

(define-struct gtype
  (include ntype)
  (predicate gtype?)
  (slots
   (context (type (list (list class))))
   (type (type ntype))))

;;; These tyvars just index a list of pre-allocated tyvars.

(define-struct gtyvar
  (include ntype)
  (predicate gtyvar?)
  (slots
   (varnum (type int))))

(define-struct const-type
  (include ntype)
  (predicate const-type?)
  (slots
   (type (type ntype))))

