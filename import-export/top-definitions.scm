;;; File: top-definitions.scm

;;; Description: This creates definitions for all top level (exportable)
;;;  object in a module.

(define (create-top-definitions)
  (dolist (decl (module-decls *module*))
    (if (eq? (module-type *module*) 'interface)
	(when (signdecl? decl)
	   (create-var-definitions decl (signdecl-vars decl)))
	(when (valdef? decl)
	   (create-var-definitions
	    decl (collect-pattern-vars (valdef-lhs decl))))))
  (dolist (algdata (module-algdatas *module*))
    (create-alg-definitions algdata))
  (dolist (synonym (module-synonyms *module*))
    (create-syn-definitions synonym))
  (dolist (class (module-classes *module*))
    (create-class-definitions class)))

;;; ------------------------------------------------------------------------
;;; creation of definitions
;;; ------------------------------------------------------------------------

(define (create-var-definitions decl vars)
  (remember-context decl
    (dolist (v vars)
     (let* ((var-name (var-ref-name v))
	    (def (create-top-definition var-name 'var)))
       (setf (var-ref-var v) def)
       (push def (module-vars *module*))
       (add-new-group var-name def)))))

;;; This also creates definitions for the constructors

(define (create-alg-definitions algdata)
  (remember-context algdata
    (with-slots data-decl (simple constrs) algdata
      (let* ((alg-name (tycon-name simple))
	     (def (create-top-definition alg-name 'algdata)))
	(setf (tycon-def simple) def)
	(let ((constr-group
	       (map (lambda (constr) 
		     (let* ((con-ref (constr-constructor constr))
			    (con-name (con-ref-name con-ref))
			    (con-def (create-top-definition con-name 'con)))
		        (setf (con-ref-con con-ref) con-def)
			(tuple con-name con-def)))
		    constrs)))
	  (setf (algdata-constrs def) (map (function tuple-2-2) constr-group))
	  (setf (tycon-def-arity def) (length (tycon-args simple)))
	  (add-new-group alg-name def constr-group))))))

(define (create-class-definitions class-decl)
  (remember-context class-decl
    (with-slots class-decl (class decls) class-decl
      (let* ((class-name (class-ref-name class))
	     (class-def (create-top-definition class-name 'class)))
	(setf (class-ref-class class) class-def)
	(let ((method-group
	       (concat
		(map
		 (lambda (decl) 
		  (if (is-type? 'signdecl decl)
		      (remember-context decl
		       (map (lambda (method-var)
			      (let* ((var-name (var-ref-name method-var))
				     (def (create-top-definition
					      var-name 'method-var)))
				(setf (method-var-class def) class-def)
				(setf (method-var-default def) '#f)
				(setf (var-ref-var method-var) def)
				(tuple var-name def)))
			    (signdecl-vars decl)))
		      '()))
		decls))))
	  (setf (class-method-vars class-def)
		(map (function tuple-2-2) method-group))
	  (add-new-group class-name class-def method-group))))))

(define (create-syn-definitions synonym-decl)
  (remember-context synonym-decl
    (let* ((simple (synonym-decl-simple synonym-decl))
	   (syn-name (tycon-name simple))
	   (def (create-top-definition syn-name 'synonym)))
      (setf (tycon-def simple) def)
      (setf (tycon-def-arity def) (length (tycon-args simple)))
      (add-new-group syn-name def))))

(define (add-new-group name def . others)
  (when (memq *module* (module-exported-modules *module*))
      (export-group (cons (tuple name def)
			  (if (null? others)
			      '()
			      (car others))))))



