;;; This defines all core symbols.

;;; Core symbols are stored in global variables.  The core-symbol
;;; macro just turns a string into a variable name.

(define-syntax (core-symbol str)
  (make-core-symbol-name str))

(define (make-core-symbol-name str)
  (string->symbol (string-append "*core-" str "*")))

(define (symbol->core-var name)
  (make-core-symbol-name (symbol->string name)))

(define (get-core-var-names vars type)
  (let ((res (assq type vars)))
    (if (eq? res '#f)
	'()
	(map (function string->symbol) (tuple-2-2 res)))))

;;; This is just used to create a define for each var without a
;;; value.

(define-syntax (define-core-variables)
  `(begin
     ,@(define-core-variables-1 *haskell-prelude-vars*)
     ,@(define-core-variables-1 *haskell-noncore-vars*)))

(define (define-core-variables-1 vars)
  (concat (map (lambda (ty)
		 (map (function init-core-symbol)
		      (get-core-var-names vars ty)))
	       '(classes methods types constructors synonyms values))))

(define (init-core-symbol sym)
  `(define ,(symbol->core-var sym) '()))

(define-syntax (create-core-globals)
  `(begin
     (begin ,@(create-core-defs *haskell-prelude-vars* '#t))
     (begin ,@(create-core-defs *haskell-noncore-vars* '#f))))

(define (create-core-defs defs prelude-core?)
  `(,@(map (lambda (x) (define-core-value x prelude-core?))
	   (get-core-var-names defs 'values))
     ,@(map (lambda (x) (define-core-method x prelude-core?))
	   (get-core-var-names defs 'methods))
     ,@(map (lambda (x) (define-core-synonym x prelude-core?))
	   (get-core-var-names defs 'synonyms))
     ,@(map (lambda (x) (define-core-class x prelude-core?))
	   (get-core-var-names defs 'classes))
     ,@(map (lambda (x) (define-core-type x prelude-core?))
	    (get-core-var-names defs 'types))
     ,@(map (lambda (x) (define-core-constr x prelude-core?))
	    (get-core-var-names defs 'constructors))))


(define (define-core-value name pc?)
    `(setf ,(symbol->core-var name)
	   (make-core-value-definition ',name ',pc?)))

(define (make-core-value-definition name pc?)
  (install-core-sym
    (make var (name name) (module '|*Core|) (unit '|*Core|))
    name
    pc?))

(define (define-core-method name pc?)
    `(setf ,(symbol->core-var name)
	   (make-core-method-definition ',name ',pc?)))

(define (make-core-method-definition name pc?)
  (install-core-sym
    (make method-var (name name) (module '|*Core|) (unit '|*Core|))
    name
    pc?))

(define (define-core-class name pc?)
    `(setf ,(symbol->core-var name)
	   (make-core-class-definition ',name ',pc?)))

(define (make-core-class-definition name pc?)
  (install-core-sym
    (make class (name name) (module '|*Core|) (unit '|*Core|))
    name
    pc?))

(define (define-core-synonym name pc?)
    `(setf ,(symbol->core-var name)
	   (make-core-synonym-definition ',name ',pc?)))

(define (make-core-synonym-definition name pc?)
  (install-core-sym
    (make synonym (name name) (module '|*Core|) (unit '|*Core|))
    name
    pc?))

(define (define-core-type name pc?)
    `(setf ,(symbol->core-var name)
	   (make-core-type-definition ',name ',pc?)))

(define (make-core-type-definition name pc?)
  (install-core-sym
    (make algdata (name name) (module '|*Core|) (unit '|*Core|))
    name
    pc?))

(define (define-core-constr name pc?)
    `(setf ,(symbol->core-var name)
	   (make-core-constr-definition ',name ',pc?)))

(define (make-core-constr-definition name pc?)
  (setf name (add-con-prefix/symbol name))
  (install-core-sym
    (make con (name name) (module '|*Core|) (unit '|*Core|))
    name
    pc?))

(define (install-core-sym def name preludecore?)
  (setf (def-core? def) '#t)
  (when preludecore? 
    (setf (def-prelude? def) '#t))
  (setf (table-entry (dynamic *core-symbols*) name) def)
  (when preludecore?
    (setf (table-entry (dynamic *prelude-core-symbols*) name) def))
  def)
