;;; This is the parser for interface files.

(define (parse-tokens/interface tokens)
  (init-token-stream tokens)
  (let ((interface (token-case
		    (|interface| (parse-interface))
		    (|module| (interface-required-error))
		    (else (crud-in-interface-error)))))
    (cons interface (parse-interface-list))))

(define (interface-required-error)
  (parser-error 'interface-required "Expecting `interface' keyword"))

(define (crud-in-interface-error)
  (parser-error 'unexpected-interface-crud "Junk after interface"))

(define (parse-interface-list)
  (token-case
   (|interface|
     (let ((interface (parse-interface)))
       (cons interface (parse-interface-list))))
   (|module| (interface-required-error))
   (eof '())
   (else (crud-in-interface-error))))

(define (parse-interface)
  (token-case
   (modid
    (let ((module-name (token->symbol)))
      (require-token |where|
       (signal-missing-token "`where'" "interface definition"))
      (let ((mod-ast (make module (name module-name)
			          (type 'interface)
				  (exports '()))))
	(start-layout (lambda (in-layout?)
		       (parse-interface-decls mod-ast in-layout? 'import))))))))

(define (parse-interface-decls mod-ast in-layout? state)
  (token-case
    (|import| (let ((import (parse-import)))
		(when (not (eq? (import-decl-mode import) 'by-name))
		   (phase-error 'illegal-import
    "Imports in interfaces must specify specific entities"))
		(if (eq? state 'import)
		    (push-decl-list import (module-imports mod-ast))
		    (signal-misplaced-import)))
	      (terminate-interface-topdecl mod-ast in-layout? state))
    (|infix| (terminate-interface-topdecl mod-ast in-layout?
			       (parse-fixity 'n mod-ast state)))
    (|infixl| (terminate-interface-topdecl mod-ast in-layout?
			       (parse-fixity 'l mod-ast state)))
    (|infixr| (terminate-interface-topdecl mod-ast in-layout?
			       (parse-fixity 'r mod-ast state)))
    (|data| (let ((data-decl (parse-type-decl '#t)))
	      (push-decl-list data-decl (module-algdatas mod-ast)))
	    (terminate-interface-topdecl mod-ast in-layout? 'topdecl))
    (|type| (let ((synonym-decl (parse-synonym-decl)))
	     (push-decl-list synonym-decl (module-synonyms mod-ast)))
	    (terminate-interface-topdecl mod-ast in-layout? 'topdecl))
    (|class| (let ((class-decl (parse-class-decl)))
	       (check-class-default-decls class-decl)
	       (push-decl-list class-decl (module-classes mod-ast)))
	     (terminate-interface-topdecl mod-ast in-layout? 'topdecl))
    (|instance| (let ((instance-decl (parse-instance-decl '#t)))
		  (push-decl-list instance-decl (module-instances mod-ast)))
		(terminate-interface-topdecl mod-ast in-layout? 'topdecl))
    (var (let ((decl (parse-signdecl)))
	   (setf (module-decls mod-ast)
		 (decl-push decl (module-decls mod-ast))))
	 (terminate-interface-topdecl mod-ast in-layout? 'topdecl))
    ((begin-annotation no-advance)
     (let ((annotations (parse-annotations)))
       (setf (module-annotations mod-ast)
	     (append (module-annotations mod-ast) annotations)))
     (terminate-interface-topdecl mod-ast in-layout? state))
    (else
     (maybe-end-interface mod-ast in-layout?))))

(define (maybe-end-interface mod-ast in-layout?)
  (cond ((or (eq-token? '|interface|) (eq-token? 'eof) (eq-token? '\})
	     (eq-token? '$\}))
	 (close-layout in-layout?)
	 (wrapup-module mod-ast)
	 mod-ast)
	(else
	 (signal-invalid-syntax "a topdecl"))))

(define (terminate-interface-topdecl mod-ast in-layout? state)
  (token-case
   (\; (parse-interface-decls mod-ast in-layout? state))
   (else (maybe-end-interface mod-ast in-layout?))))

(define (check-class-default-decls class-decl)
  (dolist (d (class-decl-decls class-decl))
    (when (valdef? d)
      (remember-context d
       (recoverable-error 'no-defaults-in-interface
         "Class defaults should not be put in interface files")))))
