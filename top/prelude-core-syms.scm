;;; This should be used to create core symbols for every name exported
;;; by PreludeCore.  This only needs to run when the Prelude definition
;;; changes.  

(define (def->name-string x)
  (symbol->string (def-name x)))


(define (generate-prelude-core-symbols)
  (initialize-compilation)
  (load-compilation-unit *prelude-unit-filename* '#t '#f '#f '#f)
  (let* ((core (table-entry *modules* '|PreludeCore|))
	 (export-table (module-export-table core))
	 (vars '())
	 (classes '())
	 (types '())
	 (constrs '())
	 (syns '())
	 (methods '()))
    (table-for-each
      (lambda (k v)
	(declare (ignore k))
	(let ((def (tuple-2-2 (car v))))
	  (cond ((var? def)
		 (push (def->name-string def) vars))
		((synonym? def)
		 (push (def->name-string def) syns))
		((algdata? def)
		 (push (def->name-string def) types)
		 (dolist (x (cdr v))
		     (push (remove-con-prefix (def->name-string (tuple-2-2 x)))
			   constrs)))
		((class? def)
		 (push (def->name-string def) classes)
		 (dolist (x (cdr v))
		     (push (def->name-string (tuple-2-2 x))
			   methods)))
		(else (error "? strange def")))))
      export-table)
  (call-with-output-file "/tmp/prelude-syms"
      (lambda (port)
	(pprint `(define *haskell-prelude-vars*
		   '((classes ,@classes)
		     (methods ,@methods)
		     (types ,@types)
		     (constructors ,@constrs)
		     (synonyms ,@syns)
		     (values ,@vars)))
		port)))))



(define (create-prelude-init-code defs)
  (let* ((name (def-name def))
	 (sym-name (make-core-symbol-name name)))
    `(define sym-name '())))

