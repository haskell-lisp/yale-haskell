;;; walk-ast.scm -- general-purpose walkers for AST structures.
;;;
;;; author :  Sandra & John
;;; date   :  30 Jan 1992
;;;
;;;

;;;=====================================================================
;;; Basic support, macros
;;;=====================================================================


;;; Here is a macro for accessing the walker function for a particular
;;; type.
;;; The walk-type names the walker.
;;; If an accessor argument is provided, it must name a SETF'able function
;;; or macro that takes a type descriptor as an argument.  This is used to
;;; do the lookup of the walker function for the given type.
;;; If no explicit accessor is provided, one will be created.  It will
;;; use a hash table keyed off the type names to store the walker functions.
;;; In either case, the mapping between the walker name and accessor is
;;; stored in the hash table ast-walker-table.

(define ast-walker-table (make-table))

(define-syntax (define-walker walk-type . maybe-accessor)
  (let ((accessor-name  (if (null? maybe-accessor)
			    (symbol-append walk-type '-walker)
			    (car maybe-accessor))))
    (setf (table-entry ast-walker-table walk-type) accessor-name)
	`(begin
	   ,@(if (null? maybe-accessor)
		 (let ((accessor-table (symbol-append '* walk-type '-table*)))
		   `((define ,accessor-table (make-table))
		     (define-syntax (,accessor-name td)
		       (list 'table-entry
			     ',accessor-table
			     (list 'td-name td)))))
		 '())
	   (setf (table-entry ast-walker-table ',walk-type)
		 ',accessor-name)
	   ',walk-type)))

(define-syntax (ast-walker walk-type td)
  (let ((accessor  (table-entry ast-walker-table walk-type)))
    `(,accessor ,td)))


;;; This macro dispatches a walker on an object of type ast-node.

(define-syntax (call-walker walk-type object . args)
  (let ((temp (gensym "OBJ")))
    `(let ((,temp ,object))
       (funcall (or (ast-walker ,walk-type (struct-type-descriptor ,temp))
		    (walker-not-found-error ',walk-type ,temp))
		,temp
		,@args))
    ))

(define (walker-not-found-error walk-type object)
  (error "There is no ~a walker for structure ~A defined."
	 walk-type (td-name (struct-type-descriptor object))))



;;; Define an individual walker for a particular type.  The body should
;;; return either the original object or a replacement for it.

(define-syntax (define-walker-method walk-type type args . body)
  (let ((function-name  (symbol-append walk-type '- type)))
    `(begin
       (define (,function-name ,@args) ,@body)
       (setf (ast-walker ,walk-type (lookup-type-descriptor ',type))
	     (function ,function-name))
       ',function-name)))



;;;=====================================================================
;;; Support for default walker methods
;;;=====================================================================

;;; Two kinds of walkers are supported: a collecting walker, which
;;; walks over a tree collecting some sort of returned result while
;;; not changing the tree itself, and a rewriting walker which maps
;;; ast to ast.

;;; The basic template for a collecting walk is:
;;; (define-walker-method walk-type type (object accum)
;;;   (sf1 (sf2 object ... (sfn accum)))
;;; where sfi = slot function for the ith slot.
;;;
;;; The slot-processor should be the name of a macro that is called with four
;;; arguments:  a slot descriptor, the object type name, a form 
;;; representing the object being traversed, and a form representing the 
;;; accumulated value.
;;; If the slot does not participate in the walk, this last argument should
;;; be returned unchanged as the expansion of the macro.

(define-syntax (define-collecting-walker-methods walk-type types
		 slot-processor)
  `(begin
     ,@(map (lambda (type)
	      (make-collecting-walker-method walk-type type slot-processor))
	    types)))

(define (make-collecting-walker-method walk-type type slot-processor)
  `(define-walker-method ,walk-type ,type (object accum)
     object   ; prevent possible unreferenced variable warning
     ,(make-collecting-walker-method-body
       'accum
       type
       (td-slots (lookup-type-descriptor type))
       slot-processor)))

(define (make-collecting-walker-method-body base type slots slot-processor)
  (if (null? slots)
      base
      `(,slot-processor ,(car slots) ,type object 
		 ,(make-collecting-walker-method-body
		     base type (cdr slots) slot-processor))))



;;; A rewriting walker traverses the ast modifying various subtrees.
;;; The basic template here is:
;;; (define-walker-method walker type (object . args)
;;;   (setf (slot1 object) (walk (slot1 object)))
;;;   (setf (slot2 object) (walk (slot2 object)))
;;;   ...
;;;   object)

;;; The basic macro to generate default walkers is as above except
;;; that the slot-processor macro is called with only 
;;; two arguments, the slot and object type.
;;; The `args' is the actual lambda-list for the methods, and bindings
;;; can be referenced inside the code returned by the macro.
;;; If a slot participates in the walk, the macro should return code
;;; to SETF the slot, as in the template above.  Otherwise, the macro
;;; should just return #f.

(define-syntax (define-modify-walker-methods walk-type types args
		 slot-processor)
  `(begin
     ,@(map (lambda (type)
	      (make-modify-walker-method walk-type type args
					 slot-processor))
	    types)))

(define (make-modify-walker-method walk-type type args slot-processor)
  `(define-walker-method ,walk-type ,type ,args
     ,@(cdr args)  ; prevent possible unreferenced variable warnings
     ,@(map (lambda (slot)
	      `(,slot-processor ,slot ,type))
	    (td-slots (lookup-type-descriptor type)))
     ,(car args)))
