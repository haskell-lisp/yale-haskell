;;; This initializes the module ast structures.

;;; This requires that the module table be created and updated with new
;;; modules first.  *unit* must also be defined.

;;; Things initialized there:
;;;  all tables in the module structure
;;;  the module slot of all import declarations and entity-modules
;;;  The import Prelude is added when necessary
;;;  Empty export lists are explicated

(define (init-module-structure)
  (when (not (eq? (module-type *module*) 'extension))
    ;; If this is an extension, the incremental compiler has already
    ;; filled in the compilation unit.
    (setf (module-unit *module*) *unit*))
  ;;; This processes the annotations.  Annotations used at the top
  ;;; level of the module:
  ;;;   {-#PRELUDE#-} : this contains definitions in the Haskell prelude
  (setf (module-prelude? *module*) '#f)
  (setf (module-interface-codefile *module*) '())
  (dolist (a (module-annotations *module*))
    (when (annotation-value? a)
      (let ((name (annotation-value-name a)))
	(cond ((eq? name '|Prelude|)
	       (setf (module-prelude? *module*) '#t))))))
  (cond ((eq? (module-type *module*) 'interface)
	 (setf (module-exported-modules *module*) (list *module*))
	 (process-interface-imports *module*))
	((eq? (module-type *module*) 'standard)
	 (init-standard-module))))

(define (init-standard-module)
   (let ((seen-prelude? '#f))
    (dolist (import (module-imports *module*))
      (let* ((name (import-decl-module-name import))
	     (imported-mod (locate-module name)))
	(when (eq? name '|Prelude|)
	   (setf seen-prelude? '#t))
	(if (eq? imported-mod '#f)
	    (signal-undefined-module-import name)
	    (setf (import-decl-module import) imported-mod))
	(when (eq? name *module-name*)
	  (signal-self-import name))))
    (when (null? (module-exports *module*))
	(setf (module-exports *module*)
	      (list (make entity-module (name *module-name*)
			                (module *module*)))))
    (when (not seen-prelude?)
      (let ((prelude (locate-module '|Prelude|)))
	(cond ((eq? prelude '#f)
	       (signal-missing-prelude))
	      ((module-prelude? *module*)
	       (setf (module-uses-standard-prelude? *module*) '#f)
	       (add-imported-module prelude))
	      (else
	       (setf (module-uses-standard-prelude? *module*) '#t)
	       (let ((fix-table (module-fixity-table *module*)))
		 (table-for-each (lambda (k v)
				   (setf (table-entry fix-table k) v))
				 *prelude-fixity-table*))))))
    (let ((prelude-core (locate-module '|PreludeCore|)))
       (if (eq? prelude-core '#f)
	   (signal-missing-prelude-core)
	   (when (module-prelude? *module*)
		 (add-imported-module prelude-core))))
    (setf (module-exports *module*)
	  (filter-complete-module-exports (module-exports *module*))))
    )


(define (add-imported-module module)
  (setf (module-imports *module*)
	(cons (make import-decl
		    (module-name (module-name module))
		    (module module)
		    (mode 'all)
		    (specs '())
		    (renamings '()))
	      (module-imports *module*))))

(define (filter-complete-module-exports exports)
  (if (null? exports)
      '()
      (let ((export (car exports))
	    (others (filter-complete-module-exports (cdr exports))))
	(if (is-type? 'entity-module export)
	    (let* ((name (entity-name export))
		   (exported-mod (locate-module name)))
	      (when (eq? exported-mod '#f)
		(signal-undefined-module-export name))
	      (push exported-mod (module-exported-modules *module*))
	      (when (not (memq name
			   (cons *module-name*
				 (map
				   (lambda (import)
				     (import-decl-module-name import))
				   (module-imports *module*)))))
		(signal-export-not-imported name))
	      others)
	    (cons export others)))))

(define (process-interface-imports module)
  (let ((imports '()))
    (dolist (i (module-imports module))
      (let ((module (import-decl-module-name i))
	    (renamings (import-decl-renamings i)))
	(dolist (s (import-decl-specs i))
          (let* ((n (entity-name s))
		 (n1 (do-interface-rename n renamings)))
	    (when (assq n1 imports)
               (signal-multiple-imports n1))
	    (push (tuple n1 (tuple module n)) imports)
	    (cond ((entity-class? s)
		   (dolist (m (entity-class-methods s))
                     (let ((m1 (do-interface-rename m renamings)))
		       (when (assq m1 imports)
                          (signal-multiple-imports m1))
		       (push (tuple m1 (tuple module m)) imports))))
		  ((entity-datatype? s)
		   (dolist (m (entity-datatype-constructors s))
                     (let ((m1 (do-interface-rename m renamings)))
		       (when (assq m1 imports)
                          (signal-multiple-imports m1))
		       (push (tuple m1 (tuple module m)) imports)))))))))
    (setf (module-interface-imports module) imports)))

(define (signal-multiple-imports name)
  (phase-error 'multuple-interface-import
    "Interface file has more than one definition of ~A~%" name))

(define (do-interface-rename name renamings)
  (if (has-con-prefix? (symbol->string name))
      (let* ((n1 (remove-con-prefix/symbol name))
	     (res (locate-renaming n1 renamings)))
	(if (eq? res '#f)
	    name
	    (add-con-prefix/symbol (renaming-to res))))
      (let ((res (locate-renaming name renamings)))
	(if (eq? res '#f)
	    name
	    (renaming-to res)))))
