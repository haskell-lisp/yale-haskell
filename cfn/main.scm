;;; main.scm -- main entry point for CFN pass
;;;
;;; author :  Sandra Loosemore
;;; date   :  27 Feb 1992
;;;


;;;===================================================================
;;; Basic support
;;;===================================================================


;;; Define the basic walker and some helper functions.

(define-walker cfn ast-td-cfn-walker)

(define (cfn-ast-1 x)
  (call-walker cfn x))

(define (cfn-ast/list l)
  (map (lambda (x) (cfn-ast-1 x)) l))


;;; This is the main entry point.  It is called by the driver on
;;; each top-level decl in the module.

(define (cfn-ast x)
  (let ((result  (cfn-ast-1 x)))
;    (pprint result)  ;*** debug
    result))



;;;===================================================================
;;; Default traversal methods
;;;===================================================================


(define-local-syntax (make-cfn-code slot type)
  (let ((stype  (sd-type slot))
        (sname  (sd-name slot)))
    (cond ((and (symbol? stype)
                (or (eq? stype 'exp)
                    (subtype? stype 'exp)))
           `(setf (struct-slot ',type ',sname object)
                  (cfn-ast-1 (struct-slot ',type ',sname object))))
          ((and (pair? stype)
                (eq? (car stype) 'list)
                (symbol? (cadr stype))
                (or (eq? (cadr stype) 'exp)
                    (subtype? (cadr stype) 'exp)))
           `(setf (struct-slot ',type ',sname object)
                  (cfn-ast/list (struct-slot ',type ',sname object))))
          ((and (pair? stype)
                (eq? (car stype) 'list)
                (eq? (cadr stype) 'decl))
           `(setf (struct-slot ',type ',sname object)
                  (cfn-valdef-list (struct-slot ',type ',sname object))))
          (else
;          (format '#t "Cfn: skipping slot ~A in ~A~%"
;                  (sd-name slot)
;                  type)
           '#f))))

(define-modify-walker-methods cfn
  (let if
   exp-sign
   app
   var-ref con-ref
   integer-const float-const char-const string-const
   con-number sel is-constructor
   void
   case-block return-from and-exp
   )
  (object)
  make-cfn-code)


;;; These have specialized walkers:
;;; lambda, case, valdef, list-comp  (pattern.scm)
;;; list-exp, list-comp, section-l, section-r, dict-placeholder,
;;; recursive-placeholder, save-old-exp (misc.scm)

