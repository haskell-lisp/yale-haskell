;;; ==================================================================

;;; This deals with incremental compilation as used by the command interface.
;;; The basic theory is to create new modules which import the entire
;;; symbol table of an existing module.


;;; This adds a new module to the extension environment.  This env is an alist
;;; of module names & extended modules.

(define *extension-env* '())

(define (extend-module mod-name new-ast)
  (push (tuple mod-name new-ast) *extension-env*))

;;; This cleans out extensions for a module.

(define (remove-extended-modules mod-name)
  (setf *extension-env* (rem-ext1 *extension-env* mod-name)))

(define (rem-ext1 env name)
  (cond ((null? env)
	 '())
	((eq? (tuple-2-1 (car env)) name)
	 (rem-ext1 (cdr env) name))
	(else
	 (cons (car env) (rem-ext1 (cdr env) name)))))

(define (clear-extended-modules)
  (setf *extension-env* '()))

;;; This retrieves the current extension to a module (if any).

(define (updated-module name)
  (let ((name+mod (assq name *extension-env*)))
    (if (not (eq? name+mod '#f))
	(tuple-2-2 name+mod)
	(let ((mod-in-table (table-entry *modules* name)))
	  (cond ((eq? mod-in-table '#f)
		 (signal-module-not-ready name))
		((eq? (module-type mod-in-table) 'interface)
		 (signal-cant-eval-interface name))
		(else mod-in-table))))))

(define (signal-module-not-ready name)
  (fatal-error 'module-not-ready
	       "Module ~A is not loaded and ready."
	       name))

(define (signal-cant-eval-interface name)
  (fatal-error 'no-evaluation-in-interface
	       "Module ~A is an interface: evaluation not allowed."
	       name))

(define (compile-fragment module str filename)
  (let ((mod-ast (updated-module module)))
    (dynamic-let
       ((*printers* (if (memq 'extension *printers*) *printers* '()))
	(*abort-phase*   '#f))
     (mlet (((t-code new-ast) (compile-fragment1 module mod-ast str filename)))
       (cond ((eq? t-code 'error)
	      'error)
	     (else
	      (eval t-code)
	      new-ast))))))

(define (compile-fragment1 mod-name mod-ast str filename)
  (let/cc x
    (dynamic-let ((*abort-compilation* (lambda () (funcall x 'error '()))))
     (let* ((mods (parse-from-string
		   (format '#f "module ~A where~%~A~%" mod-name str)
		   (function parse-module-list)
		   filename))
	   (new-mod (car mods)))
	(when (not (null? (cdr mods)))
	  (signal-module-decl-in-extension))
	(when (not (null? (module-imports new-mod)))
	  (signal-import-decl-in-extension))
	(fragment-initialize new-mod mod-ast)
	(values (modules->lisp-code mods) new-mod)))))

(define (signal-module-decl-in-extension)
  (fatal-error 'module-decl-in-extension
	       "Module declarations are not allowed in extensions."))

(define (signal-import-decl-in-extension)
  (fatal-error 'import-decl-in-extension
	       "Import declarations are not allowed in extensions."))


;;; Copy stuff into the fragment module structure from its parent module.
;;; The inverted symbol table is not necessary since the module contains
;;; no imports.

(define (fragment-initialize new old)
  (setf (module-name new) (gensym))
  (setf (module-type new) 'extension)
  (setf (module-unit new) (module-unit old))
  (setf (module-uses-standard-prelude? new)
	(module-uses-standard-prelude? old))
  (setf (module-inherited-env new) old)
  (setf (module-fixity-table new)
        (copy-table (module-fixity-table old)))
  (setf (module-default new) (module-default old)))
  
;;; This code deals with the actual evaluation of Haskell code.

;;; This decides whether a variable has type `Dialogue'.

(define (io-type? var)
  (let ((type (var-type var)))
    (when (not (gtype? type))
      (error "~s is not a Gtype." type))
    (and (null? (gtype-context type))
	 (is-dialogue? (gtype-type type)))))

(define (is-dialogue? type)
  (let ((type (expand-ntype-synonym type)))
    (and (ntycon? type)
	 (eq? (ntycon-tycon type) (core-symbol "Arrow"))
	 (let* ((args (ntycon-args type))
		(a1 (expand-ntype-synonym (car args)))
		(a2 (expand-ntype-synonym (cadr args))))
	   (and
	    (ntycon? a1)
	    (eq? (ntycon-tycon a1) (core-symbol "SystemState"))
	    (ntycon? a2)
	    (eq? (ntycon-tycon a2) (core-symbol "IOResult")))))))

(define (is-list-of? type con)
  (and (ntycon? type)
       (eq? (ntycon-tycon type) (core-symbol "List"))
       (let ((arg (expand-ntype-synonym (car (ntycon-args type)))))
	 (and (ntycon? arg) (eq? (ntycon-tycon arg) con)))))

(define (apply-exec var)
   (initialize-io-system)
   (mlet (((_ sec)
	   (time-execution
	     (lambda ()
	       (let/cc x
		 (setf *runtime-abort* (lambda () (funcall x 'error)))
		 (let ((fn (eval (fullname var))))
		   (unless (var-strict? var)
		       (setf fn (force fn)))
		   (funcall fn (box 'state))))))))
      (say "~%")
      (when (memq 'time *printers*)
	 (say "Execution time: ~A seconds~%" sec)))
   'done)

(define (eval-module mod)
  (dolist (v (module-vars mod))
     (when (io-type? v)
	(when (not (string-starts? "temp_" (symbol->string (def-name v))))
	   (say/ne "~&Evaluating ~A.~%" v))
	(apply-exec v))))

(define (run-program name)
  (compile/load name)
  (let ((main-mod (table-entry *modules* '|Main|)))
    (if main-mod
	(let ((main-var (table-entry (module-symbol-table main-mod) '|main|)))
	  (if main-var
	      (apply-exec main-var)
	      (error "Variable main missing")))
	(error "module Main missing"))))

