;;; print-modules.scm -- print routines for module-related AST structures
;;;
;;; author :  Sandra Loosemore
;;; date   :  6 Jan 1992
;;;
;;;
;;; This file corresponds to the file ast/modules.scm.

;;; Note: by default, only the module name is printed.  To print the
;;; full module, the function print-full-module must be called.

(define *print-abbreviated-modules* '#t)

(define-ast-printer module (object xp)
 (if *print-abbreviated-modules*
     (begin
       (write-string "Module " xp)
       (write-string (symbol->string (module-name object)) xp))
     (do-print-full-module object xp)))

(define (print-full-module object . maybe-stream)
  (let ((stream  (if (not (null? maybe-stream))
		     (car maybe-stream)
		     (current-output-port))))
    (dynamic-let ((*print-abbreviated-modules* '#f))
       (pprint object stream))))

(define (do-print-full-module object xp)
 (dynamic-let ((*print-abbreviated-modules* '#t))
  (let ((modid    (module-name object))
	(exports  (module-exports object))
	(body     (append (module-imports object)
			  (module-fixities object)
			  (module-synonyms object)
			  (module-algdatas object)
			  (module-classes object)
			  (module-instances object)
			  (if (or (not (module-default object))
				  (eq? (module-default object)
				       *standard-module-default*))
			      '()
			      (list (module-default object)))
			  (module-decls object))))
    (write-string "module " xp)
    (write-modid modid xp)
    (when (not (null? exports))
      (write-whitespace xp)
      (write-commaized-list exports xp))
    (write-wheredecls body xp))))

(define-ast-printer import-decl (object xp)
  (let ((modid     (import-decl-module-name object))
	(mode      (import-decl-mode object))
	(specs     (import-decl-specs object))
	(renamings (import-decl-renamings object)))
    (with-ast-block (xp)
      (write-string "import " xp)
      (write-modid modid xp)
      (if (eq? mode 'all)
	  (when (not (null? specs))
	    (write-whitespace xp)
	    (write-string "hiding " xp)
	    (write-commaized-list specs xp))
	  (begin
	    (write-whitespace xp)
	    (write-commaized-list specs xp)))
      (when (not (null? renamings))
	(write-whitespace xp)
	(write-string "renaming " xp)
	(write-commaized-list renamings xp))
      )))

(define-ast-printer entity-module (object xp)
  (write-modid (entity-name object) xp)
  (write-string ".." xp))

(define-ast-printer entity-var (object xp)
  (write-varid (entity-name object) xp))

(define-ast-printer entity-con (object xp)
  (write-tyconid (entity-name object) xp))

(define-ast-printer entity-abbreviated (object xp)
  (write-tyconid (entity-name object) xp)
  (write-string "(..)" xp))

(define-ast-printer entity-class (object xp)
  (with-ast-block (xp)
    (write-tyclsid (entity-name object) xp)
    (write-whitespace xp)
    (write-delimited-list (entity-class-methods object) xp
			  (function write-varid) "," "(" ")")))

(define-ast-printer entity-datatype (object xp)
  (with-ast-block (xp)
    (write-tyconid (entity-name object) xp)
    (write-whitespace xp)
    (write-delimited-list (entity-datatype-constructors object) xp
			  (function write-conid) "," "(" ")")))


(define-ast-printer renaming (object xp)
  (with-ast-block (xp)
    (write-varid-conid (renaming-from object) xp)
    (write-string " to" xp)
    (write-whitespace xp)
    (write-varid-conid (renaming-to object) xp)))

;;; *** Should it omit precedence if it's 9?

(define-ast-printer fixity-decl (object xp)
  (let* ((fixity         (fixity-decl-fixity object))
	 (associativity  (fixity-associativity fixity))
	 (precedence     (fixity-precedence fixity))
	 (ops            (fixity-decl-names object)))
    (with-ast-block (xp)
      (cond ((eq? associativity 'l)
	     (write-string "infixl " xp))
	    ((eq? associativity 'r)
	     (write-string "infixr " xp))
	    ((eq? associativity 'n)
	     (write-string "infix " xp)))
      (write precedence xp)
      (write-whitespace xp)
      (write-delimited-list ops xp (function write-varop-conop) "," "" ""))))
