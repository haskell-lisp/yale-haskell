;;; print-types.scm -- print type-related AST structures
;;;
;;; author :  Sandra Loosemore
;;; date   :  15 Jan 1991
;;;
;;; This file corresponds to the stuff in ast/type-structs.scm
;;;

(define-ast-printer tyvar (object xp)
  (write-avarid (tyvar-name object) xp))


;;; Various type special cases have a magic cookie in the def field.

(define-ast-printer tycon (object xp)
  (print-general-tycon (tycon-def object) (tycon-args object) object xp))

(define (print-general-tycon def args object xp)
    (cond ((eq? def (core-symbol "Arrow"))
	   (write-arrow-tycon args xp))
	  ((eq? def (core-symbol "UnitType"))
	   (write-unit-tycon xp))
	  ((eq? def (core-symbol "List"))
	   (write-list-tycon args xp))
	  ((is-tuple-tycon? def)
	   (write-tuple-tycon args xp))
	  (else
	   (write-ordinary-tycon def args object xp))))

(define (write-arrow-tycon args xp)
  (with-ast-block (xp)
    (write-btype (car args) xp)
    (write-string " ->" xp)
    (write-whitespace xp)
    (write (cadr args) xp)))

(define (write-unit-tycon xp)
  (write-string "()" xp))

(define (write-list-tycon args xp)
  (with-ast-block (xp)
    (write-char #\[ xp)
    (write (car args) xp)
    (write-char #\] xp)))

(define (write-tuple-tycon args xp)
  (write-commaized-list args xp))

(define (write-ordinary-tycon def args object xp)
  (with-ast-block (xp)
    (if (tycon? object)
	(write-tyconid (tycon-name object) xp)
	(write-tyconid (def-name def) xp))
    (when (not (null? args))
      (write-whitespace xp)
      (write-delimited-list
        args xp (function write-atype) "" "" ""))))


;;; All of the special cases above except "Arrow" are atypes, as is
;;; a tyvar or a tycon with no arguments.

(define (write-atype object xp)
 (let ((object (maybe-prune object)))
  (if (or (tyvar? object)
	  (gtyvar? object)
	  (ntyvar? object)
	  (is-some-tycon? object
	     (lambda (def)
	       (or (eq? def (core-symbol "UnitType"))
		   (eq? def (core-symbol "List"))
		   (is-tuple-tycon? def)))))
      (write object xp)
      (begin
        (write-char #\( xp)
	(write object xp)
	(write-char #\) xp)))))


;;; A btype is any type except the arrow tycon.

(define (write-btype object xp)
 (let ((object (maybe-prune object)))
  (if (or (and (tycon? object)
	       (eq? (tycon-def object) (core-symbol "Arrow")))
	  (and (ntycon? object)
	       (eq? (ntycon-tycon object) (core-symbol "Arrow"))))
      (begin
        (write-char #\( xp)
	(write object xp)
	(write-char #\) xp))
      (write object xp))))
      
(define (maybe-prune object)
  (if (ntyvar? object)
      (prune object)
      object))

(define (is-some-tycon? object fn)
  (let ((object (maybe-prune object)))
    (or (and (tycon? object)
	     (or (null? (tycon-args object))
		 (funcall fn (tycon-def object))))
	(and (ntycon? object)
	     (or (null? (ntycon-args object))
		 (funcall fn (ntycon-tycon object)))))))

(define-ast-printer context (object xp)
  (with-ast-block (xp)
    (write (context-class object) xp)
    (write-whitespace xp)
    (write-avarid (context-tyvar object) xp)))

(define-ast-printer signature (object xp)
  (write-contexts (signature-context object) xp)
  (write (signature-type object) xp))

(define (write-contexts contexts xp)
  (when (not (null? contexts))
    (if (null? (cdr contexts))
	(write (car contexts) xp)
	(write-commaized-list contexts xp))
    (write-string " =>" xp)
    (write-whitespace xp)))

(define-ast-printer synonym-decl (object xp)
  (with-ast-block (xp)
    (write-string "type " xp)
    (write (synonym-decl-simple object) xp)
    (write-string " =" xp)
    (write-whitespace xp)
    (write (synonym-decl-body object) xp)))

(define-ast-printer data-decl (object xp)
  (with-ast-block (xp)
    (write-string "data " xp)
    (write-contexts (data-decl-context object) xp)
    (write (data-decl-simple object) xp)
    (write-whitespace xp)
    (write-char #\= xp)
    (write-whitespace xp)
    (write-delimited-list
      (data-decl-constrs object) xp (function write) " |" "" "")
    (write-whitespace xp)
    (let ((deriving  (data-decl-deriving object)))
      (when (not (null? deriving))
	(write-string "deriving " xp)
	(if (null? (cdr deriving))
	    (write (car deriving) xp)
	    (write-commaized-list deriving xp))))))

(define-ast-printer constr (object xp)
  (if (con-ref-infix? (constr-constructor object))
      (with-ast-block (xp)
        (write-btype (car (constr-types object)) xp)
	(write-whitespace xp)
	(write (constr-constructor object) xp)
	(write-whitespace xp)
	(write-btype (cadr (constr-types object)) xp))
      (with-ast-block (xp)
	(write (constr-constructor object) xp)
	(when (not (null? (constr-types object)))
	  (write-whitespace xp)
	  (write-delimited-list
	   (constr-types object) xp (function write-atype) "" "" "")))))


(define-ast-printer class-decl (object xp)
  (with-ast-block (xp)
    (write-string "class " xp)
    (write-contexts (class-decl-super-classes object) xp)
    (write (class-decl-class object) xp)
    (write-whitespace xp)
    (write-avarid (class-decl-class-var object) xp)
    (write-wheredecls (class-decl-decls object) xp)))

(define-ast-printer instance-decl (object xp)
  (with-ast-block (xp)
    (write-string "instance " xp)
    (write-contexts (instance-decl-context object) xp)
    (write (instance-decl-class object) xp)
    (write-whitespace xp)
    (write-atype (instance-decl-simple object) xp)
    (write-wheredecls (instance-decl-decls object) xp)))


;;; Don't print out default decl if the value is the default.

(define-ast-printer default-decl (object xp)
  (with-ast-block (xp)
    (write-string "default " xp)
    (let ((types  (default-decl-types object)))
      (if (null? (cdr types))
	  (write (car types) xp)
	  (write-commaized-list types xp)))))

(define-ast-printer class-ref (object xp)
  (write-tyclsid (class-ref-name object) xp))
        
  
  
