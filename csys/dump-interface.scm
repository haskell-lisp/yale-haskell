;;; dump-interface.scm -- interface file writer/loader
;;;
;;; author :  John & Sandra
;;; date   :  8 Jul 1992
;;;
;;; This writes binary interface files.  A binary interface file is just
;;; a lisp (mumble) source file which directly builds the ast structure
;;; created by a compilation.  These files could be stored in either
;;; source or binary (compiled lisp) form.

;;; An interface may reference entities defined in other interfaces.
;;; To ensure consistancy between when an interface is written and
;;; when it is read back in, a stamp is assigned to all interface files
;;; which serves as a unique id.  The stamps of all imported units are
;;; saved and examined at load time.



;;;==================================================================
;;; Interface to compilation system
;;;==================================================================


;;; For compiled code, don't actually write out all the source code.
;;; Use a magic macro to memoize the form to be compiled.

(define *form-to-compile* '#f)
(define *magic-file-to-compile* "$HASKELL/bin/magic.scm")


;;; The output from compiling the prelude can completely overwhelm
;;; the Lisp compiler.  If this variable is a number, it specifies
;;; a "reasonable" number of top-level forms which can be compiled
;;; and write-compiled-code-file will try to break up the input
;;; code automagically.

(define *magic-chunk-size* '#f)


;;; This is called to write both the code file and the interface file.

(define (write-compiled-code-file filename code code-quality chunk-size)
  (let ((phase-start-time (get-run-time))
        (forms            (flatten-forms code)))
    (dynamic-let ((*magic-chunk-size*
		   (or chunk-size (dynamic *magic-chunk-size*)))
		  (*code-quality*
		   (or code-quality (dynamic *code-quality*))))
      (if (or (not (dynamic *magic-chunk-size*))
	      (<= (the fixnum (length forms))
		  (the fixnum (dynamic *magic-chunk-size*))))
	  (write-compiled-code-file-aux filename `(begin ,@forms))
	  (with-compilation-unit ()
	    (write-compiled-code-file-aux
	      filename
	      `(begin
		 ,@(map (lambda (f) `(load ,f))
			(write-compiled-code-file-split filename forms)))
	      ))))
    (when (memq 'phase-time *printers*)
      (let* ((current-time (get-run-time))
	     (elapsed-time (- current-time phase-start-time)))
	(format '#t "Lisp compilation complete: ~A seconds~%" elapsed-time)))
    ))

(define (write-compiled-code-file-split filename forms)
  (let ((place     (filename-place filename))
	(name      (filename-name filename))
	(type      (filename-type filename))
	(result    '()))
    (do ((i 0 (1+ i)))
	((null? forms))
	(multiple-value-bind (head tail)
	    (split-list forms (dynamic *magic-chunk-size*))
	  (let ((fname
		  (assemble-filename
		    place (format '#f "~a-part~a" name i) type)))
	    (push fname result)
	    (write-compiled-code-file-aux fname `(begin ,@head))
	    (setf forms tail))))
    (nreverse result)))

(define (flatten-forms code)
  (if (and (pair? code) (eq? (car code) 'begin))
      (nreverse (flatten-forms-aux (cdr code) '()))
      (list code)))

(define (flatten-forms-aux forms result)
  (dolist (f forms)
    (if (and (pair? f) (eq? (car f) 'begin))
	(setf result (flatten-forms-aux (cdr f) result))
	(push f result)))
  result)
	

(define (write-compiled-code-file-aux filename code)
  (dynamic-let ((*form-to-compile*  code))
    (compile-file (dynamic *magic-file-to-compile*) filename)))

(define-syntax (magic-form-to-compile)
  (dynamic *form-to-compile*))


;;; Writing source code is good for debugging purposes, but slow.
;;; The *print-circle* and *print-shared* flags have to be set because
;;; the code printed out may contain gensyms, and this will ensure
;;; that the code can be read in again.

(define (write-interpreted-code-file filename code hairy?)
  (dynamic-let ((*print-circle*   '#t)
		(*print-shared*   '#t))
    (call-with-output-file
      filename
      (lambda (port)
	(if hairy?
	    (pprint-flatten code port)
	    (print-flatten code port))))))


;;; This attempts to read a compiled interface for a unit.  This is
;;; done whenever the unit file is newer than the source file.  If
;;; imported units have changed, the load will fail and recompilation
;;; will be attempted.  
;;; The caller is responsible for making sure that the interface file exists
;;; and for making sure that the interface file is up-to-date with
;;; respect to imported modules and that all the imported modules are
;;; known.

;;; These variables are assigned by the code in the dump file.

(define *modules-loaded* '())
(define *modules-imported* '())
(define *defs-referenced* '())
(define *saved-cse-values* '())
(define *writer-version* '())

(define (read-binary-interface unit)
  (dynamic-let ((*modules-loaded*  '())
		(*modules-imported* '())
		(*defs-referenced*  '())
		(*saved-cse-values* '())
		(*writer-version* '()))
    (let ((file-date
	   (load-more-recent-file (ucache-cifile unit) (ucache-sifile unit))))
      (cond ((string=? *writer-version* *haskell-compiler-version*)
	     (setf (ucache-idate unit) file-date)
	     (setf (ucache-modules unit) (vector->list *modules-loaded*))
	     (setf (ucache-ifile-loaded unit) '#t)
	     '#t)
	    (else
	     (signal-incompatible-interface-file (ucache-cifile unit))
	     '#f)))))

(define (signal-incompatible-interface-file filename)
  (fatal-error 'incompatible-interface-file
    "File ~A~%~
     was written by a different version of the Haskell system.~%~
     You must remove it and recompile."
    filename))


(define (load-more-recent-file cfile sfile)
  (cond ((file-exists? cfile)
	 (if (or (not (file-exists? sfile))
		 (> (file-write-date cfile)
		    (file-write-date sfile)))
	     (load-compiled-interface-file cfile)
	     (load-interpreted-interface-file sfile)))
	((file-exists? sfile)
	 (load-interpreted-interface-file sfile))
	(else
	 (signal-file-not-found cfile))))

(define (load-interpreted-interface-file file)
  (load file)
  (file-write-date file))

(define (load-compiled-interface-file file)
  (load file)
  (file-write-date file))


;;;==================================================================
;;; Dump code generator
;;;==================================================================

;;; Globals

(define *dump-defs* '())
(define *dump-slot-init-code* '())
(define *dump-def-counter* 0)
(define *dump-def-code-table* (make-table))
(define *cse-objects* '())
(define *cse-value-num* 0)
(define *cse-object-num* '())
(define *gtype-class-index* '())
(define *context-class-index* '())
(define *gtype-tycon-index* '())
(define *gtype-list-index* '())
(define *gtype-index* '())
(define *number-vars-dumped* 0)


(define-syntax (def-dump-code def)
  `(table-entry *dump-def-code-table* ,def))

;;; This saves slot initialization code.

(define (add-dump-init code)
  (push code *dump-slot-init-code*))


;;; Here is the top-level call.

(define (create-dump-code unit modules load-prelude?)
  (dynamic-let ((*unit* (module-unit (car modules)))
		(*dump-defs*  '())
		(*dump-slot-init-code*  '())
		(*dump-def-counter* 0)
		(*dump-def-code-table* (make-table))
		(*cse-objects* '())
		(*cse-object-num* *num-saved-gtyvars*)
		(*gtype-class-index* '())
		(*context-class-index* '())
		(*gtype-tycon-index* '())
		(*gtype-list-index* '())
		(*gtype-index* '())
		(*number-vars-dumped* 0)
		(*number-types-dumped* 0)
		(*number-classes-dumped* 0))
    (let ((res (create-dump-code-aux unit modules load-prelude?)))
      (when (memq 'dumper (dynamic *printers*))
        (pprint* res))
      (when (memq 'dump-stat (dynamic *printers*))
	(format '#t
	  "~&Dumped ~A definitions, ~A type objects, and ~A classes.~%"
          *number-vars-dumped* *number-types-dumped*
	  *number-classes-dumped*)
	(format '#t "Used ~A definitions and ~A type cells.~%"
		*dump-def-counter* (length *cse-objects*)))
      res)))

;;; This assumes all modules are in the same compilation unit and that
;;; *unit* is set to that unit.
;;; imod-code establishes local bindings for all the imported modules.
;;; dmod-code establishes local bindings for all the modules defined in
;;; this compilation unit.

(define (create-dump-code-aux unit modules load-prelude?)
  (let* ((imod-counter  0)
	 (imod-alist    '())
	 (explicit-imports (collect-all-imported-modules unit))
	 (all-imports   (if load-prelude?
			    (append (collect-prelude-modules) explicit-imports)
			    explicit-imports))
	 (imod-code     (map (lambda (m)
			       (push (cons (module-name m) imod-counter)
				     imod-alist)
			       (incf imod-counter)
			       `(locate-module ',(module-name m)))
			     all-imports))
	 (dmod-counter  0)
	 (dmod-alist    '())
	 (dmod-code     (map (lambda (m)
			       (push (cons (module-name m) dmod-counter)
				     dmod-alist)
			       (incf dmod-counter)
			       `(make module
				      (unit ',(module-unit m))
				      (name ',(module-name m))
				      (type ',(module-type m))))
			     modules)))
    ;; This actually does most of the work.  It dumps the module asts by
    ;; placing inits for each slot into *dump-slot-init-code*.  A list of
    ;; definitions referenced is maintained in *dump-defs*.
    (dolist (m modules)
      (dump-module m (cdr (assq (module-name m) dmod-alist))))
    ;; This creates the final code
    `(begin
       (setf *writer-version* ',*haskell-compiler-version*)
       (setf *modules-imported* (vector ,@imod-code))
       (setf *modules-loaded* (vector ,@dmod-code))
       ;; This sets the elements individually instead of using the vector
       ;; function, because the vector may be longer than
       ;; call-arguments-limit.
       (setf *defs-referenced*
	     (make-vector ,(dynamic *dump-def-counter*)))
       ,@(map (lambda (d)
		`(setf ,(def-dump-code d)
		       ,(make-def-init-code d imod-alist dmod-alist)))
	      *dump-defs*)
       ,@(cse-init-code)
       ,@(dynamic *dump-slot-init-code*)
       )
    ))


;;; Runtime support

(define-syntax (lookup-imported-mod i)
  `(vector-ref *modules-imported* ,i))

(define-syntax (lookup-defined-mod i)
  `(vector-ref *modules-loaded* ,i))

(define (set-export-from-def-vector table key index)
  (setf (table-entry table key)
	(list (cons key (vector-ref *defs-referenced* index)))))

(define (set-export-from-def table key def)
  (setf (table-entry table key)
	(list (cons key def))))

(define (set-symtab-from-def-vector table key index)
  (setf (table-entry table key)
	(vector-ref *defs-referenced* index)))

(define (init-variable-slots var exported? toplevel? type simple? strict?)
  (setf (def-exported? var) exported?)
  (setf (var-toplevel? var) toplevel?)
  (setf (var-type var) type)
  (setf (var-simple? var) simple?)
  (setf (var-strict? var) strict?)
  var)

(define (init-function-slots var exported? toplevel? type simple? strict?
			     arity strictness opt-entry)
  (setf (def-exported? var) exported?)
  (setf (var-toplevel? var) toplevel?)
  (setf (var-type var) type)
  (setf (var-simple? var) simple?)
  (setf (var-strict? var) strict?)
  (setf (var-arity var) arity)
  (setf (var-strictness var) strictness)
  (setf (var-optimized-entry var) opt-entry)
  var)

(define (init-method-var-slots var class default method-signature)
  (setf (method-var-class var) class)
  (setf (method-var-default var) default)
  (setf (method-var-method-signature var) method-signature)
  var)

(define (init-constructor-slots
  	   con arity types signature tag alg fixity infix?)
  (setf (con-arity con) arity)
  (setf (con-types con) types)
  (setf (con-signature con) signature)
  (setf (con-tag con) tag)
  (setf (con-alg con) alg)
  (setf (con-fixity con) fixity)
  (setf (con-infix? con) infix?)
  (dotimes (i arity)
    (push '#f (con-slot-strict? con)))
  con)

(define (make-new-instance algdata tyvars class context gcontext dictionary m)
  (make instance
	(algdata algdata)
	(tyvars tyvars)
	(class class)
	(context context)
	(gcontext gcontext)
	(dictionary dictionary)
	(methods m)
	(ok? '#t)))


;;; This computes the transitive closure of all modules available to
;;; a unit.

(define (collect-all-imported-modules unit)
  (collect-all-modules-1 (ucache-imported-units unit) '() '()))

(define (collect-all-modules-1 units mods-so-far units-seen)
  (cond ((null? units)
	 mods-so-far)
	((mem-string (car units) units-seen)
	 (collect-all-modules-1 (cdr units) mods-so-far units-seen))
	(else
	 (let ((u (lookup-compilation-unit (car units))))
	   (collect-all-modules-1
	    (append (ucache-imported-units u) (cdr units))
	    (append (ucache-modules u) mods-so-far)
	    (cons (ucache-ufile u) units-seen))))
	))

(define (collect-prelude-modules)
  (let ((prelude-unit (lookup-compilation-unit *prelude-unit-filename*)))
    (append (ucache-modules prelude-unit)
	    (collect-all-imported-modules prelude-unit))))

(define (def->core-name-string def)
  (if (con? def)
      (remove-con-prefix (symbol->string (def-name def)))
      (symbol->string (def-name def))))

;;; This code returns the load time definition for an object.  When the
;;; object is a core symbol or in a different unit, previously
;;; created definitions are returned.  Otherwise, a new definition is
;;; created.
  
(define (make-def-init-code d imod-alist dmod-alist)
  (declare (ignore dmod-alist))
  (cond ((def-core? d)
	 `(core-symbol ,(def->core-name-string d)))
	((eq? (def-unit d) *unit*)
	 `(create-definition/inner
	    ',(def-module d)
	    ',(def-name d)
	    ',(cond ((method-var? d) 'method-var)
		    ((var? d) 'var)
		    ((con? d) 'con)
		    ((synonym? d) 'synonym)
		    ((algdata? d) 'algdata)
		    ((class? d) 'class))))
	((is-tuple-constructor? d)
	 `(tuple-constructor ,(tuple-constructor-arity d)))
	((is-tuple-tycon? d)
	 `(tuple-tycon ,(tuple-constructor-arity (car (algdata-constrs d)))))
	(else
	 (let ((m (assq (def-module d) imod-alist)))
	   ;; This is a bogus error message.  The problem is that nothing
	   ;; so far ensures units are closed under import/export: some
	   ;; modules may be referenced that are accidentally in the symbol
	   ;; table.  The unif file for the current module needs to be
	   ;; updated when this happens.
	   (when (eq? m '#f)
	     (fatal-error 'symbol-not-in-unit
 "Reference to symbol ~A in module ~A: not in compilation unit.~%"
                (def-name d) (def-module d)))
	 `(table-entry
	    (module-symbol-table
	      (lookup-imported-mod ,(tuple-2-2 m)))
	    ',(def-name d))))
	))


;;; Once a module has been compiled, most of its slots are useless.
;;; All we really need to save are the identifying information,
;;; symbol table, and export table.
;;; Instances also need to be dumped here instead of with class objects;
;;; this is because links can go across compilation unit boundaries.
;;; They are fixed up when pulling units out of the cache.
;;; The identifying info is stored when the module variable is bound.


(define (dump-module module index)
  (let ((mod-exp `(lookup-defined-mod ,index))
	(save-all-symbols (or (eq? (module-type module) 'standard)
			      (eq? (module-name module) '|Prelude|))))
    ;; Dump symbol table entries only for defs for which this is
    ;; the "home" module.  (In other words, ignore imported defs.)
    ;; The purpose of this is to allow references from other
    ;; interface files to be resolved; see make-def-init-code.
    ;; Jcp: we need to save the complete symbol table for incremental
    ;; compilation to work.
    (let ((code  '()))
      (table-for-each
        (lambda (key val)
	  (when (or save-all-symbols
		    (eq? (def-module val) (module-name module)))
	    (let ((def  (dump-object val)))
	      (push
	        (if (and (pair? def)
			 (eq? (car def) 'vector-ref)
			 (eq? (cadr def) '*defs-referenced*))
		    `(set-symtab-from-def-vector table ',key ,(caddr def))
		    `(setf (table-entry table ',key) ,def))
		code))))
	(module-symbol-table module))
      (add-dump-init `(setf (module-symbol-table ,mod-exp)
			    (let ((table  (make-table))) ,@code table))))
    ;; dump the fixity table - needed by the incremental compiler
    (when save-all-symbols
      (let ((code  '()))
	(table-for-each
	  (lambda (key val)
	    (push `(setf (table-entry table ',key)
			 (make-fixity ',(fixity-associativity val)
				      ',(fixity-precedence val)))
		  code))
	  (module-fixity-table module))
	(add-dump-init `(setf (module-fixity-table ,mod-exp)
			      (let ((table  (make-table))) ,@code table)))))
    ;; Dump all export table entries.  This is used by the import/export
    ;; phase to resolve references.  
    (let ((code  '()))
      (table-for-each
        (lambda (key val)
	  ;; val is an a-list of (sym . def) pairs.
	  ;; Look for shortcut to reduce size of generated code.
	  (push
	    (if (and (null? (cdr val))
		     (eq? (car (car val)) key))
		(let ((def  (dump-object (cdr (car val)))))
		  (if (and (pair? def)
			   (eq? (car def) 'vector-ref)
			   (eq? (cadr def) '*defs-referenced*))
		      `(set-export-from-def-vector table ',key ,(caddr def))
		      `(set-export-from-def table ',key ,def)))
		`(setf (table-entry table ',key) ,(dump-object val)))
	    code))
	(module-export-table module))
      (add-dump-init `(setf (module-export-table ,mod-exp)
			    (let ((table  (make-table))) ,@code table))))
    ;; Dump the instances.
    (add-dump-init `(setf (module-instance-defs ,mod-exp)
			  ,(dump-object (module-instance-defs module))))
    (add-dump-init `(setf (module-default ,mod-exp)
			  ,(dump-object (module-default module))))
    (add-dump-init `(setf (module-uses-standard-prelude? ,mod-exp)
			  ,(dump-object
			    (module-uses-standard-prelude? module))))
    ))

(define (make-fixity a p)
  (make fixity (associativity a) (precedence p)))


;;;==================================================================
;;; Dump structure traversal
;;;==================================================================

;;; This is the general object dumper.  It recognizes the basic Lisp
;;; objects and dumps them.  Given an object, this generates lisp code
;;; to recreate the object at load time.

(define (dump-object x)
  (cond ((struct? x)
	 (dump x))
	((or (symbol? x) (null? x))
	 ;; Symbols and lists must be quoted.
	 `',x)
	((or (number? x)
	     (eq? x '#t)
	     (eq? x '#f)
	     (string? x)   ; This makes dumped strings immutable.
	     (char? x))
	 ;; These objects are self-evaluating.
	 x)
	((list? x)
	 ;; True lists
	 `(list ,@(map (function dump-object) x)))
	((pair? x)
	 `(cons ,(dump-object (car x))
		,(dump-object (cdr x))))
	((vector? x)
	 `(vector ,@(map (function dump-object) (vector->list x))))
	((table? x)
	 `(list->table ,@(dump-object (table->list x))))
	(else
	 (error "Don't know how to dump ~A." x))))


;;; *** Should install the walker in the type descriptor.

(define-walker dump)

(define (dump x)
  (call-walker dump x))



;;;==================================================================
;;; Dumpers for defs
;;;==================================================================


;;; All walkers for def structures should call this macro.  The body
;;; is invoked only if the def belongs to the current compilation unit
;;; and hasn't already been traversed.  Within the body, the 
;;; variable "v" is bound to a form that will evaluate to the 
;;; corresponding def structure at run time.  This is also
;;; the return value from the macro.

(define-local-syntax (with-new-def (v d stat-var) . body)
  (let ((temp   (gensym))
	(expvar (gensym)))
    `(let ((,temp  ,d)
	   (,expvar '#f))
       (if (not (def-dump-code ,temp))
	   (begin
	     (cond ((not (def-core? ,temp))
		    (setf ,expvar
			  (list 'vector-ref
				'*defs-referenced*
				(dynamic *dump-def-counter*)))
		    (incf (dynamic *dump-def-counter*))
		    (push ,temp *dump-defs*))
		   (else
		    (setf ,expvar
			  (make-core-symbol-name
			    (def->core-name-string ,temp)))))
	     (setf (def-dump-code ,temp) ,expvar)
	     (when (eq? (def-unit ,temp) *unit*)
	       (incf (dynamic ,stat-var))
	       (let ((,v  ,expvar))
		 ,@body))
	     ,expvar)
	   (def-dump-code ,temp)))))


;;; This macro is used to save the value of a structure slot in the
;;; initforms of the dump.

(define-local-syntax (dump-def-slots obj-var type dexp slots)
  `(add-dump-init
     (list 'update-slots ',type ,dexp
	   ,@(map (lambda (s)
		    `(list ',s
			   (dump-object (struct-slot ',type ',s ,obj-var))))
		  slots)))
  )



(define-walker-method dump var (var)
  (dump-var/n var))

(define (dump-var/n var)
  (with-new-def (dexp var *number-vars-dumped*)
    (do-dump-var dexp var '#f)))

(define (do-dump-var dexp var method-var?)
  (let ((code            '())
	(exported?       (def-exported? var))
	(toplevel?       (var-toplevel? var))
	(type            (var-type var))
	(simple?         (var-simple? var))
	(strict?         (var-strict? var))
	(arity           (var-arity var))
	(strictness      (var-strictness var))
	(opt-entry       (var-optimized-entry var))
	(complexity      (var-complexity var))
	(fixity          (var-fixity var))
	(value           (var-value var))
	(inline-value    (var-inline-value var))
	(sel?            (var-selector-fn? var)))
    ;; Some slots are useless for vars that don't name functions.
    (if (eqv? arity 0)
	(push `(init-variable-slots var
	         ',exported?
		 ',toplevel?
		 ,(dump-object type)
		 ',simple?
		 ',strict?)
	      code)
	(push `(init-function-slots var
		 ',exported?
		 ',toplevel?
		 ,(dump-object type)
		 ',simple?
		 ',strict?
		 ',arity
		 ,(dump-strictness strictness)
		 ',opt-entry)
	      code))
    ;; These slots rarely need to be tweaked from the default.
    (when sel?
      (push `(setf (var-selector-fn? var) '#t) code))
    (when complexity
      (push `(setf (var-complexity var) ,complexity) code))
    (when fixity
      (push `(setf (var-fixity var) ,(dump-object fixity)) code))
    ;; Save values of simple variables to permit inlining.
    ;; Save values of structured constants to permit folding of flic-sel
    ;; operations -- this is necessary to optimize dictionary lookups.
    (when (or simple? sel?
	      (and value
		   (is-type? 'flic-app value)
		   (structured-constant-app?
		     (flic-app-fn value) (flic-app-args value))))
      (push `(setf (var-value var) ,(dump-flic-top value)) code))
    (when inline-value
      (push `(setf (var-inline-value var) ,(dump-flic-top inline-value)) code))
    ;; Save extra stuff for method vars
    (when method-var?
      (push `(init-method-var-slots var
	       ,(dump-object (method-var-class var))
	       ,(dump-object (method-var-default var))
	       ,(dump-object (method-var-method-signature var)))
	    code))
    ;; Push the whole mess onto the init code.
    (add-dump-init `(let ((var  ,dexp)) ,@(nreverse code)))))


(define-walker-method dump method-var (var)
  (dump-method-var/n var))

(define (dump-method-var/n var)
  (with-new-def (dexp var *number-vars-dumped*)
    (do-dump-var dexp var '#t)))

(define-walker-method dump con (con)
  (dump-con/n con))

(define (dump-con/n con)
  (with-new-def (dexp con *number-types-dumped*)
    (add-dump-init
      `(let ((con (init-constructor-slots
		   ,dexp
		   ,(con-arity con)
		   ,(dump-object (con-types con))
		   ,(dump-object (con-signature con))
		   ,(con-tag con)
		   ,(dump-object (con-alg con))
		   ,(dump-object (con-fixity con))
		   ',(con-infix? con))))
	 ,@(if (memq '#t (con-slot-strict? con))
	       `((setf (con-slot-strict? con) ',(con-slot-strict? con)))
	       '())
	 ,@(if (eq? (con-lisp-fns con) '())
	       '()
	       `((setf (con-lisp-fns con) ',(con-lisp-fns con))))
	 con))))

;;; *** Could define similar init functions for other defs instead
;;; *** of setting slots inline, but I'm lazy and they don't show up
;;; *** nearly as often as the others.

(define-walker-method dump algdata (alg)
  (dump-algdata/n alg))

(define (dump-algdata/n alg)
  (with-new-def (dexp alg *number-types-dumped*)
    (dump-def-slots alg algdata dexp
		    (arity n-constr constrs context tyvars signature
			   enum? tuple? real-tuple? implemented-by-lisp?))))

(define-walker-method dump synonym (syn)
  (dump-synonym/n syn))

(define (dump-synonym/n syn)
  (with-new-def (dexp syn *number-types-dumped*)
    (dump-def-slots syn synonym dexp (arity args body))))

(define-walker-method dump class (class)
  (dump-class/n class))

(define (dump-class/n class)
  (with-new-def (dexp class *number-classes-dumped*)
    (dump-def-slots class class dexp
		    (super super* tyvar method-vars selectors kind
		     n-methods dict-size))))


;;;==================================================================
;;; Dumpers for non-def AST structs
;;;==================================================================

;;; This section contains dumpers to handle type-related structs that
;;; are referenced by the various def guys.


(define-walker-method dump instance (o)
  (if (not (instance-ok? o))
      (error "Attempt to dump instance that's not ok!"))
  `(make-new-instance
     ,(dump-object (instance-algdata o))
     ,(dump-object (instance-tyvars o))
     ,(dump-object (instance-class o))
     ,(dump-object (instance-context o))
     ,(dump-object (instance-gcontext o))
     ,(dump-object (instance-dictionary o))
     ,(dump-object (instance-methods o))))



(define-walker-method dump gtype (o)
  (dump-gtype/cse o))

(define-walker-method dump fixity (o)
  `(**fixity ',(fixity-associativity o) ',(fixity-precedence o)))

(define-walker-method dump tyvar (o)
  `(**tyvar ',(tyvar-name o)))

(define-walker-method dump class-ref (o)
  `(**class/def ,(dump-object (class-ref-class o))))

(define-walker-method dump context (o)
  `(**context ,(dump-object (context-class o))
	      ,(dump-object (context-tyvar o))))

(define-walker-method dump tycon (o)
  `(**tycon/def ,(dump-object (tycon-def o))
		,(dump-object (tycon-args o))))

(define-walker-method dump default-decl (o)
  `(make default-decl (types ,(dump-object (default-decl-types o)))))

(define-walker-method dump signature (o)
  `(make signature (context ,(dump-object (signature-context o)))
	           (type ,(dump-object (signature-type o)))))

;;; All ntyvars should be instantiated at this point

; (define-walker-method dump ntyvar (o)
;  (dump-object (prune o)))
