;;; compile.scm -- compilation utilities
;;;
;;; author :  Sandra Loosemore
;;; date   :  24 Oct 1991
;;;
;;; This file defines a makefile-like compilation system that supports
;;; a hierarchy of dependencies.
;;; The external entry points are define-compilation-unit, load-unit, and
;;; compile-and-load-unit.



;;;=====================================================================
;;; Parsing
;;;=====================================================================


;;; Establish global defaults for filenames.

(define compile.source-filename source-file-type)
(define compile.binary-filename binary-file-type)
(define compile.binary-subdir (string-append lisp-implementation-name "/"))
(define compile.delayed-loads '())


;;; Top level units are stored in this table.
;;; This is really a slight wart on the whole scheme of things; this
;;; is done instead of storing the top-level units in variables because
;;; we were getting unintentional name collisions.

(define compile.unit-table (make-table))

(define-syntax (compile.lookup-unit name)
  `(table-entry compile.unit-table ,name))

(define (mung-global-units names lexical-units)
  (map (lambda (n)
	 (if (memq n lexical-units)
	     n
	     `(compile.lookup-unit ',n)))
       names))


;;; Top-level compilation units are defined with define-compilation-unit.
;;; The body can consist of the following clauses:
;;;
;;; (source-filename <filename>)
;;; (binary-filename <filename>)
;;;   Specify source and/or binary file names.  For nested units, these
;;;   are merged with defaults from outer units.  If you don't specify
;;;   an explicit binary filename, it's inherited from the source file
;;;   name.
;;; (require ...)
;;;   Specify compile/load dependencies.  Arguments are names of other
;;;   units/component files; these names have scoping like let*, so a unit
;;;   can require previously listed units at the same or outer level.
;;; (unit name ....)
;;;   Specifies a nested unit.  This can appear multiple times.
;;;   If a unit doesn't include any nested units, then it's a leaf
;;;   consisting of a single source file.
;;; (load <boolean>)
;;;   If supplied and false, the unit isn't loaded unless it is needed
;;;   to satisfy a require clause.  Used for files containing compilation
;;;   support stuff.
;;; (compile <boolean>)
;;;   If supplied and false, the unit isn't compiled.  Only useful for
;;;   leaf nodes.  Typically used in combination with (load '#f) to suppress
;;;   compilation of stuff only used at compile time.

(define-syntax (define-compilation-unit name . clauses)
  `(begin
     (let ((unit  ,(compile.process-unit-spec name clauses '#t '())))
       (setf (compile.lookup-unit ',name) unit)
       (setf compilation-units (append compilation-units (list unit))))
     ',name))


;;; The basic approach is to turn the compilation unit definition into
;;; a big LET*, and put calls to build the actual unit object inside
;;; of this.
;;; 

(define (compile.process-unit-spec name clauses top-level? lexical-units)
  (multiple-value-bind
      (source-filename binary-filename require nested-units
		       load? compile?)
      (compile.parse-unit-spec clauses lexical-units)
    `(let* ((compile.source-filename ,source-filename)
	    (compile.binary-filename ,binary-filename)
	    (compile.unit-require    (list ,@require))
	    (compile.delayed-loads   (append compile.delayed-loads
					     (compile.select-delayed-loads
						     compile.unit-require)))
	    ,@nested-units)
       (make compile.unit
	     (name ',name)
	     (source-filename compile.source-filename)
	     (binary-filename compile.binary-filename)
	     (components (list ,@(map (function car) nested-units)))
	     (require compile.unit-require)
	     (top-level? ',top-level?)
	     (load? ,load?)
	     (compile? ,compile?)
	     (delayed-loads compile.delayed-loads)))))

(define (compile.parse-unit-spec clauses lexical-units)
  (let ((source-filename  '#f)
	(binary-filename  '#f)
	(require          '#f)
	(nested-units     '())
	(load?            ''#t)
	(compile?         ''#t))
    (dolist (c clauses)
      (cond ((not (pair? c))
	     (compile.unit-syntax-error c))
	    ((eq? (car c) 'source-filename)
	     (if source-filename
		 (compile.unit-duplicate-error c)
		 (setf source-filename (cadr c))))
	    ((eq? (car c) 'binary-filename)
	     (if binary-filename
		 (compile.unit-duplicate-error c)
		 (setf binary-filename (cadr c))))
	    ((eq? (car c) 'require)
	     (if require
		 (compile.unit-duplicate-error c)
		 (setf require (mung-global-units (cdr c) lexical-units))))
	    ((eq? (car c) 'unit)
	     (push (list (cadr c)
			 (compile.process-unit-spec (cadr c) (cddr c)
						    '#f lexical-units))
		   nested-units)
	     (push (cadr c) lexical-units))
	    ((eq? (car c) 'load)
	     (setf load? (cadr c)))
	    ((eq? (car c) 'compile)
	     (setf compile? (cadr c)))
	    (else
	     (compile.unit-syntax-error c))))
    (values
        (if source-filename
	    `(compile.merge-filenames ,source-filename
		     compile.source-filename '#f)
	    'compile.source-filename)
	(if binary-filename
	    `(compile.merge-filenames ,binary-filename
		     compile.binary-filename '#f)
	    (if source-filename
		'(compile.merge-filenames compile.binary-filename
			 compile.source-filename
			 compile.binary-subdir)
		'compile.binary-filename))
	(or require '())
	(nreverse nested-units)
	load?
	compile?)))


(predefine (error format . args))

(define (compile.unit-syntax-error c)
  (error "Invalid compilation unit clause ~s." c))

(define (compile.unit-duplicate-error c)
  (error "Duplicate compilation unit clause ~s." c))



;;;=====================================================================
;;; Representation and utilities
;;;=====================================================================

;;; Here are constructors and accessors for unit objects.
;;; Implementationally, the compilation unit has the following slots:
;;;
;;; * The unit name.
;;; * The source file name.
;;; * The binary file name.
;;; * A list of component file/units.
;;; * A list of units/files to require.
;;; * A load timestamp.
;;; * A timestamp to keep track of the newest source file.
;;; * Flags for compile and load.

(define-struct compile.unit
  (predicate compile.unit?)
  (slots
    (name             (type symbol))
    (source-filename  (type string))
    (binary-filename  (type string))
    (components       (type list))
    (require          (type list))
    (top-level?       (type bool))
    (load?            (type bool))
    (compile?         (type bool))
    (delayed-loads    (type list))
    (load-time        (type (maybe integer)) (default '#f))
    (source-time      (type (maybe integer)) (default '#f))
    (last-update      (type (maybe integer)) (default 0))
    ))

(define (compile.newer? t1 t2)
  (and t1
       t2
       (> t1 t2)))

(define (compile.select-newest t1 t2)
  (if (compile.newer? t1 t2) t1 t2))

(define (compile.get-source-time u)
  (let ((source-file  (compile.unit-source-filename u)))
    (if (file-exists? source-file)
	(file-write-date source-file)
	'#f)))

(define (compile.get-binary-time u)
  (let ((binary-file  (compile.unit-binary-filename u)))
    (if (file-exists? binary-file)
	(file-write-date binary-file)
	'#f)))

(define (compile.load-source u)
  (load (compile.unit-source-filename u))
  (setf (compile.unit-load-time u) (current-date)))

(define (compile.load-binary u)
  (load (compile.unit-binary-filename u))
  (setf (compile.unit-load-time u) (current-date)))

(define (compile.compile-and-load u)
  (let ((source-file  (compile.unit-source-filename u))
	(binary-file  (compile.unit-binary-filename u)))
    (compile-file source-file binary-file)
    (load binary-file)
    (setf (compile.unit-load-time u) (current-date))))

(define (compile.do-nothing u)
  u)

      
;;;=====================================================================
;;; Runtime support for define-compilation-unit
;;;=====================================================================

(define (compile.select-delayed-loads require)
  (let ((result  '()))
    (dolist (r require)
      (if (not (compile.unit-load? r))
	  (push r result)))
    (nreverse result)))

(define (compile.merge-filenames fname1 fname2 add-subdir)
  (let ((place1  (filename-place fname1))
	(name1   (filename-name fname1))
	(type1   (filename-type fname1)))
    (assemble-filename
        (if (string=? place1 "")
	    (if add-subdir
		(string-append (filename-place fname2) add-subdir)
		fname2)
	    place1)
	(if (string=? name1 "") fname2 name1)
	(if (string=? type1 "") fname2 type1))))



;;;=====================================================================
;;; Load operation
;;;=====================================================================

;;; Load-unit and compile-and-load-unit are almost identical.  The only 
;;; difference is that load-unit will load source files as necessary, while
;;; compile-and-load-unit will compile them and load binaries instead.

(define (load-unit u)
  (compile.update-unit-source-times u '#f (current-date))
  (compile.load-unit-aux u))

(define (compile.load-unit-aux u)
  (with-compilation-unit ()
    (compile.load-unit-recursive u '#f)))

(define (compile-and-load-unit u)
  (compile.update-unit-source-times u '#f (current-date))
  (compile.compile-and-load-unit-aux u))

(define (compile.compile-and-load-unit-aux u)
  (with-compilation-unit ()
    (compile.load-unit-recursive u '#t)))


;;; Load a bunch of compilation units as a group.  This is useful because
;;; it can prevent repeated lookups of file timestamps.  Basically, the
;;; assumption is that none of the source files will change while the loading
;;; is in progress.
;;; In case of an error, store the units left to be compiled in a global
;;; variable.

(define remaining-units '())

(define (load-unit-list l)
  (let ((timestamp  (current-date)))
    (dolist (u l)
      (compile.update-unit-source-times u '#f timestamp))
    (setf remaining-units l)
    (dolist (u l)
      (compile.load-unit-aux u)
      (pop remaining-units))))

(define (compile-and-load-unit-list l)
  (let ((timestamp  (current-date)))
    (dolist (u l)
      (compile.update-unit-source-times u '#f timestamp))
    (setf remaining-units l)
    (dolist (u l)
      (compile.compile-and-load-unit-aux u)
      (pop remaining-units))))


;;; Walk the compilation unit, updating the source timestamps.

(define (compile.update-unit-source-times u newest-require timestamp)
  (unless (eqv? timestamp (compile.unit-last-update u))
    (setf (compile.unit-last-update u) timestamp)
    (dolist (r (compile.unit-require u))
      (if (compile.unit-top-level? r)
	  (compile.update-unit-source-times r '#f timestamp))
      (setf newest-require
	    (compile.select-newest newest-require
				   (compile.unit-source-time r))))
    (let ((components  (compile.unit-components u)))
      (if (not (null? components))
	  (let ((source-time  newest-require))
	    (dolist (c components)
	      (compile.update-unit-source-times c newest-require timestamp)
	      (setf source-time
		    (compile.select-newest source-time
					   (compile.unit-source-time c))))
	    (setf (compile.unit-source-time u) source-time))
	  (setf (compile.unit-source-time u)
		(compile.select-newest
		  newest-require
		  (compile.get-source-time u)))))))


;;; Load a compilation unit.  Do this by first loading its require list,
;;; then by recursively loading each of its components, in sequence.  
;;; Note that because of the way scoping of units works and the
;;; sequential nature of the load operation, only top-level
;;; units in the require list have to be loaded explicitly.

(define (compile.load-unit-recursive u compile?)
  (let ((components       (compile.unit-components u)))
    ;; First recursively load dependencies.
    ;; No need to update time stamps again here.
    (dolist (r (compile.unit-require u))
      (if (compile.unit-top-level? r)
	  (compile.load-unit-aux r)))
    (if (not (null? components))
	;; Now recursively load subunits.
	(dolist (c components)
	  (unless (not (compile.unit-load? c))
	    (compile.load-unit-recursive c compile?)))
	;; For a leaf node, load either source or binary if necessary.
	(let ((source-time  (compile.unit-source-time u))
	      (binary-time  (compile.get-binary-time u))
	      (load-time    (compile.unit-load-time u)))
	  (cond ((compile.newer? load-time source-time)
		 ;; The module has been loaded since it was last changed,
		 ;; but maybe we want to compile it now.
		 (if (and compile?
			  (compile.unit-compile? u)
			  (compile.newer? source-time binary-time))
		     (begin
		       (compile.do-delayed-loads
			       (compile.unit-delayed-loads u)
			       compile?)
		       (compile.compile-and-load u))
		     (compile.do-nothing u)))
		((compile.newer? binary-time source-time)
		 ;; The binary is up-to-date, so load it.
		 (compile.load-binary u))
		(else
		 ;; The binary is out-of-date, so either load source or
		 ;; recompile the binary.
		 (compile.do-delayed-loads
			 (compile.unit-delayed-loads u)
			 compile?)
		 (if (and compile? (compile.unit-compile? u))
		     (compile.compile-and-load u)
		     (compile.load-source u)))
		)))))


(define (compile.do-delayed-loads units compile?)
  (dolist (u units)
    (compile.load-unit-recursive u compile?)))




;;;=====================================================================
;;; Extra stuff
;;;=====================================================================


;;; Reload a unit without testing to see if any of its dependencies are
;;; out of date.

(define (reload-unit-source u)
  (let ((components  (compile.unit-components u)))
    (if (not (null? components))
	(dolist (c components)
	  (reload-unit-source c))
	(compile.load-source u))))

(define (reload-unit-binary u)
  (let ((components  (compile.unit-components u)))
    (if (not (null? components))
	(dolist (c components)
	  (reload-unit-binary c))
	(compile.load-binary u))))


;;; Find a (not necessarily top-level) compilation unit with the given
;;; name.

(define (find-unit name)
  (compile.find-unit-aux name compilation-units))

(define (compile.find-unit-aux name units)
  (block find-unit-aux
    (dolist (u units '#f)
      (if (eq? name (compile.unit-name u))
	  (return-from find-unit-aux u)
	  (let* ((components (compile.unit-components u))
		 (result     (compile.find-unit-aux name components)))
	    (if result
		(return-from find-unit-aux result)))))))


;;; Combine the two above:  reload a compilation unit.

(define-syntax (reload name)
  `(reload-unit-source
     (or (find-unit ',name)
	 (error "Couldn't find unit named ~s." ',name))))
