;;; These routines deal with the global symbol table.  The symbol table
;;; is represented in two stages: a module table which maps module names
;;; onto module structures and local tables within each module which
;;; map names (symbols) to definitions.

;;; The following functions deal with the module table (*modules*):

;;;  (initialize-module-table) - this clears out all modules from the
;;;      symbol table.  Every compilation should start with this.
;;;  (add-module-to-module-table module) - this takes a module ast,
;;;      either from a .exp file or previous compilation with the same
;;;      incarnation of the compiler and adds it to the set of `known'
;;;      modules.  Incomplete module ast's in the process of compilation
;;;      are also added to this table.


(define (initialize-module-table)
  (setf *modules* (make-table)))

(define (add-module-to-symbol-table module)
  (let* ((name (module-name module))
	 (old-module (table-entry *modules* name)))
    (when (not (eq? old-module '#f))
      (if (eq? *unit* (module-unit old-module))
	  (signal-module-double-definition name)
	  (signal-module-already-defined name)))
    (setf (table-entry *modules* name) module)))

(define (remove-module-from-symbol-table module)
  (let ((name (module-name module)))
    (setf (table-entry *modules* name) '#f)))

(define (locate-module name)
  (table-entry *modules* name))

;;;  (walk-modules fn mod-list) - this calls fn for each module in the
;;;      mod-list.  It also binds the global variable *module* to the
;;;      current module, *symbol-table* to the local symbol
;;;      table.  The fixity table is also placed in a global.

(define (walk-modules mods fn)
  (dolist (mod mods)
    (dynamic-let ((*module* mod)
		  (*module-name* (module-name mod))
		  (*symbol-table* (module-symbol-table mod))
		  (*fixity-table* (module-fixity-table mod))
		  (*inverted-symbol-table* (module-inverted-symbol-table mod)))
       (funcall fn))))

;;; create-definition makes a new definition object 

(define (create-definition module name type)
  (cond ((module-prelude? module)
	 (let ((def (table-entry *core-symbols* name)))
	   (cond ((eq? def '#f)
		  (create-definition/non-core module name type))
		 (else
		  (setf (def-unit def) *unit*)
		  (setf (def-module def) (module-name module))
		  ;; *** Should any other properties be reinitialized here?
		  (cond ((or (eq? type 'var) (eq? type 'method-var))
			 (setf (var-fixity def) '#f)
			 (setf (var-signature def) '#f))
			((eq? type 'con)
			 (setf (con-fixity def) '#f)))
		  def))))
	(else (create-definition/non-core module name type))))

;(define (create-definition/non-core module name type)
;  (create-definition/new module name type)
;      (let* ((interface (module-interface-module module))
;	     (old-def (table-entry (module-symbol-table interface) name)))
;	(if (eq? old-def '#f)
;	    (create-definition/new module name type)
;	    (cond ((eq? type 'var)
;		   (unless (var? old-def)	
;		       (def-conflict module name type old-def))
;		   (setf (var-interface-type old-def) (var-type old-def)))
;		  ((eq? type 'con)
;		   (unless (con? old-def)
;		      (def-conflict module name type old-def)))
;		  ((eq? type 'synonym)
;		   (unless (synonym? old-def)
;		      (def-conflict module name type old-def)))
;		  ((eq? type 'algdata)
;		   (unless (algdata? old-def)
;		      (def-conflict module name type old-def)))
;		  ((eq? type 'class)
;		   (unless (class? old-def)
;		      (def-conflict module name type old-def)))
;		  ((eq? type 'method-var)
;		   (unless (method-var? old-def)
;		      (def-conflict module name type old-def)))))
;	(setf (def-unit old-def) *unit*)
;	old-def)))
;
;(define (def-conflict module name type def)
;  (phase-error 'interface-conflict
;    "The ~A ~A in module ~A was defined as a ~A in an interface."
;    (cond ((var? def) "variable")
;	  ((class? def) "class")
;	  ((algdata? def) "data type")
;	  ((synonym? def) "synonym")
;	  ((con? def) "constructor")
;	  (else "widgit"))
;    name (module-name module) type))    

(define (create-definition/non-core module name type)
  (let ((mname  (module-name module)))
    (when (eq? (module-type *module*) 'interface)
       (mlet (((mod name1) (rename-interface-symbol name)))
         (setf mname mod)
	 (setf name name1)))
    (create-definition/inner mname name type)))

(define (create-definition/inner mname name type)
    (cond ((eq? type 'var)
	   (make var (name name) (module mname) (unit *unit*)))
	  ((eq? type 'con)
	   (make con (name name) (module mname) (unit *unit*)))
	  ((eq? type 'synonym)
	   (make synonym (name name) (module mname) (unit *unit*)))
	  ((eq? type 'algdata)
	   (make algdata (name name) (module mname) (unit *unit*)))
	  ((eq? type 'class)
	   (make class (name name) (module mname) (unit *unit*)))
	  ((eq? type 'method-var)
	   (make method-var (name name) (module mname) (unit *unit*)))
	  (else
	   (error "Bad type argument ~s." type))))


(define (create-top-definition name type)
  (let ((def (create-definition *module* name type)))
    (insert-top-definition name def)
    def))

;;; Interfaces have a special table which resolves imports in the
;;; interface.  Given a name in an interface module this returns the
;;; corresponding full name: a (module,original-name) pair.  Symbols not
;;; imported are assumed to be defined in the interface.

(define (rename-interface-symbol name)
  (let ((res (assq name (module-interface-imports *module*))))
    (if (eq? res '#f)
	(values *module-name* name)
	(values (tuple-2-1 (tuple-2-2 res))
		(tuple-2-2 (tuple-2-2 res))))))

;;; This creates a locally defined var node.

(define (create-local-definition name)
  (let ((var     (make var (name name) (module *module-name*) (unit *unit*))))
    (setf (var-fixity var) (table-entry *fixity-table* name))
    var))


;;; This function creates a new variable. 
;;; The "root" may be either a symbol or a string.
;;; *unit* defines the home module of the variable.

;;; *** Maybe it would be possible to hack this so that it doesn't
;;; *** create any symbol at all until the name is demanded by something,
;;; *** but that seems like a rather sweeping change.

(define (create-temp-var root)
  (let* ((name   (gensym (if (symbol? root) (symbol->string root) root)))
	 (module  *unit*))
    (make var (name name) (module module) (unit *unit*))))


;;; The following routines install top level definitions into the symbol
;;; table.

(predefine (signal-multiple-name-conflict name old-local-name def))
    ; in import-export/ie-errors.scm

(define (insert-top-definition name def)
  (let ((old-definition (resolve-toplevel-name name)))
    (cond ((eq? old-definition '#f)
	   (when (not (def-prelude? def))
	       (setf (table-entry *symbol-table* name) def))
	   (when (and (var? def) (not (eq? (var-fixity def) '#f)))
             (setf (table-entry *fixity-table* name)
		   (var-fixity def)))
	   (when (and (con? def) (not (eq? (con-fixity def) '#f)))
             (setf (table-entry *fixity-table* name)
		   (con-fixity def)))
	   (when (not (def-prelude? def))
 	    (if (eq? (local-name def) '#f)
		(setf (table-entry *inverted-symbol-table* def) name)
		(signal-multiple-name-conflict name (local-name def) def))))
	  ((eq? old-definition def)
	   'OK)
	  ((def-prelude? old-definition)
	   (signal-core-redefinition name))
	  ((and (module-uses-standard-prelude? *module*)
		(table-entry *prelude-symbol-table* name))
	   (if (eq? (def-module def) *module-name*)
	       (signal-prelude-redefinition name)
	       (signal-prelude-reimport name (def-module def))))
	  ((eq? (def-module def) *module-name*)
	   (signal-multiple-definition-in-module name *module-name*))
	  ((eq? (def-module old-definition) *module-name*)
	   (signal-redefinition-by-imported-symbol name *module-name*))
	  (else
	   (signal-multiple-import name *module-name*)))))

;;; Gets the fixity of a name.

(define (get-local-fixity name)
  (table-entry *fixity-table* name))

;;; These routines support general scoping issues.  Only vars have local
;;; definitions - all other names are resolved from the global symbol table.

;;; This is used when the name must be in the top symbols.

(define (fetch-top-def name type)
  (let ((def (resolve-toplevel-name name)))
    (cond ((eq? def '#f)
	   (cond ((eq? (module-type *module*) 'interface)
		  (mlet (((mod name1) (rename-interface-symbol name)))
		    (if (eq? mod *module-name*)
			(undefined-topsym name)
			(let ((new-def (create-definition/inner
					mod name1 type)))
			  (insert-top-definition name1 new-def)
			  (cond ((algdata? new-def)
				 (setf (algdata-n-constr new-def) 0)
				 (setf (algdata-constrs new-def) '())
				 (setf (algdata-context new-def) '())
				 (setf (algdata-tyvars new-def) '())
				 (setf (algdata-classes new-def) '#f)
				 (setf (algdata-enum? new-def) '#f)
				 (setf (algdata-tuple? new-def) '#f)
				 (setf (algdata-real-tuple? new-def) '#f)
				 (setf (algdata-deriving new-def) '()))
				((class? new-def)
				 (setf (class-method-vars new-def) '())
				 (setf (class-super new-def) '())
				 (setf (class-super* new-def) '())
				 (setf (class-tyvar new-def) '|a|)
				 (setf (class-instances new-def) '())
				 (setf (class-kind new-def) 'other)
				 (setf (class-n-methods new-def) 0)
				 (setf (class-dict-size new-def) 0)
				 (setf (class-selectors new-def) '()))) 
			  new-def))))
		 (else
		  (undefined-topsym name))))
	  (else def))))

(define (undefined-topsym name)
  (signal-undefined-symbol name)
  *undefined-def*)


(define (resolve-toplevel-name name)
  (let ((pc (table-entry *prelude-core-symbols* name)))
    (cond ((not (eq? pc '#f))
	   pc)
	  ((module-uses-standard-prelude? *module*)
	   (let ((res (table-entry *prelude-symbol-table* name)))
	     (if (eq? res '#f)
		 (resolve-toplevel-name-1 name)
		 res)))
	  (else
	   (resolve-toplevel-name-1 name)))))

(define (resolve-toplevel-name-1 name)
  (cond ((eq? (module-inherited-env *module*) '#f)
	 (table-entry *symbol-table* name))
	(else
	 (let ((res (search-inherited-tables
		     name (module-inherited-env *module*))))
	   (if (eq? res '#f)
	       (table-entry *symbol-table* name)
	       res)))))

(define (search-inherited-tables name mod)
  (if (eq? mod '#f)
      '#f
      (let ((res (table-entry (module-symbol-table mod) name)))
	(if (eq? res '#f)
	    (search-inherited-tables name (module-inherited-env mod))
	    res))))

;;; Con-ref's are special in that the naming convention (;Name) ensures
;;; that if a def is found it must be a con.

(define (resolve-con con-ref)
  (when (eq? (con-ref-con con-ref) *undefined-def*)
    (remember-context con-ref
      (let ((def (fetch-top-def (con-ref-name con-ref) 'con)))
	(setf (con-ref-con con-ref) def)))))

(define (resolve-class class-ref)
  (when (eq? (class-ref-class class-ref) *undefined-def*)
    (remember-context class-ref
      (let ((def (fetch-top-def (class-ref-name class-ref) 'class)))
	(when (not (class? def))
	  (signal-class-name-required def (class-ref-name class-ref)))
	(setf (class-ref-class class-ref) def)))))


(define (resolve-tycon tycon)
  (when (eq? (tycon-def tycon) *undefined-def*)
    (remember-context tycon
      (let ((def (fetch-top-def (tycon-name tycon) 'algdata)))
	(when (class? def)
	  (signal-tycon-name-required (tycon-name tycon)))
	(setf (tycon-def tycon) def)))))


;;; This should be used after the local environment has been searched.
;;; Other routines dealing with variable scoping are elsewhere.

(define (resolve-var var-ref)
  (when (eq? (var-ref-var var-ref) *undefined-def*)
    (remember-context var-ref
      (let ((def (fetch-top-def (var-ref-name var-ref) 'var)))
	(setf (var-ref-var var-ref) def)))))


;;; *** The inverted-symbol-table is the only table in the whole
;;; *** system that is not keyed off of symbols.  If this is a problem,
;;; *** things that use it could probably be rewritten to do something
;;; *** else, like store an a-list on the def itself.

;;; This does not need to consult the inherited-env flag because when this
;;; is used in extensions only new symbols get inserted.

(define (local-name def)
  (cond ((def-prelude? def)
	 (def-name def))
	((module-uses-standard-prelude? *module*)
	 (let ((res (table-entry *prelude-inverted-symbol-table* def)))
	   (if (eq? res '#f)
	    (table-entry *inverted-symbol-table* def)
	    res)))
	(else
	 (table-entry *inverted-symbol-table* def))))
    
(define (print-name x)
  (let ((res (local-name x)))
    (if (eq? res '#f)
	(def-name x)
	res)))


;;; Error signalling routines.

(define (signal-module-double-definition name)
  (fatal-error 'module-double-definition
    "Module ~s is defined more than once."
    name))

(define (signal-module-already-defined name)
  (fatal-error 'module-already-defined
    "Module ~a is defined more than once in the current unit."
    name))

(define (signal-multiple-definition-in-module name modname)
 (if (eq? (module-type *module*) 'extension)
     (phase-error 'cant-redefine-in-extension
        "An extension for module ~A cannot redefine the symbol ~A"
	modname name)
     (phase-error 'multiple-definition-in-module
        "There is more than one definition for the name ~a in module ~a."
	name modname)))

(define (signal-redefinition-by-imported-symbol name modname)
  (phase-error 'redefinition-by-imported-symbol
    "The name ~a is defined in module ~a, and cannot be imported."
    name modname))

(define (signal-core-redefinition name)
  (phase-error 'prelude-redefinition
    "The name ~a is defined in the prelude core and cannot be redefined."
    name))

(define (signal-prelude-redefinition name)
  (phase-error 'prelude-redefinition
    "The name ~a is defined in the prelude.~%You must hide it if you wish to use this name."
    name))

(define (signal-prelude-reimport name modname)
  (phase-error 'prelude-redefinition
    "The name ~a is both imported from ~A and defined in the prelude.~%"
    name modname))

(define (signal-multiple-import name modname)
  (phase-error 'multiple-import
    "The name ~a is imported into module ~a multiple times."
    name modname))

(define (signal-undefined-symbol name)
  (phase-error 'undefined-symbol
    "The name ~A is undefined."
    name))

(define (signal-class-name-required name def)
  (phase-error 'class-name-required
    "The name ~A defines a ~A, but a class name is required."
    name
    (if (synonym? def) "synonym" "data type")))

(define (signal-tycon-name-required name)
  (phase-error 'tycon-required
    "The name ~A defines a class, but a type constructor name is required."
    name))
