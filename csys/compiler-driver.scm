;;; compiler-driver.scm -- compilation unit management
;;;
;;; author :  John & Sandra
;;;
;;;


;;; Flags for controlling various low-level behaviors of the compiler.
;;; You might want to tweak these in the system-building scripts for
;;; different Lisps, but users don't normally need to mess with them.

(define *compile-interface* '#f)
(define *interface-code-quality* 2)
(define *interface-chunk-size* '#f)
(define *default-code-quality* 2)
(define *optimized-code-quality* 3)
(define *code-chunk-size* 300)



;;;=====================================================================
;;; Main entry point
;;;=====================================================================

;;; This is the top level driver for the compiler.  It takes a file name
;;; and output controls.  It returns '#f if compilation fails.

(define *codefile-cache* '())

(define (haskell-compile filename cflags)
  (initialize-haskell-system)
  (let/cc abort-compile
    (dynamic-let ((*abort-compilation*
		   (lambda () (funcall abort-compile '#f))))
     (initialize-compilation)
     (let ((unit (find-cunit-name filename)))
       (let ((res (load-compilation-unit unit cflags)))
	 (map (lambda (x) (module-name x)) (ucache-modules res)))))))

;;; this is the initialization code that occurs at the start of compilation.

(define (initialize-compilation)
  (initialize-module-table)
  (for-each-unit
   (lambda (u)
     (setf (ucache-status u) 'available))))



;;;=====================================================================
;;; Filename utilities
;;;=====================================================================

;;; File extensions

(define *source-file-extensions* '(".hs" ".lhs"))
(define *unit-file-extension* ".hu")
(define *interface-file-extension* ".hi")
(define *lisp-file-extensions* '(".lisp" ".scm"))

(define (source-extension? x)
  (mem-string x *source-file-extensions*))

(define (unit-extension? x)
  (string=? x *unit-file-extension*))

(define (interface-extension? x)
  (string=? x *interface-file-extension*))

(define (lisp-extension? x)
  (mem-string x *lisp-file-extensions*))


;;; Build file names.

(define (make-cifilename filename)
  (let ((place  (filename-place filename))
	(name   (string-append (filename-name filename) "-hci")))
    (assemble-filename place name binary-file-type)))

(define (make-sifilename filename)
  (let ((place  (filename-place filename))
	(name   (string-append (filename-name filename) "-hci")))
    (assemble-filename place name source-file-type)))

(define (make-cfilename filename)
  (add-extension filename binary-file-type))

(define (make-sfilename filename)
  (add-extension filename source-file-type))


;;; This take a file name (extension ignored) & searches for a unit file.

(define (locate-existing-cunit name)
  (locate-extension name (list *unit-file-extension*)))

;;; This take a file name (extension ignored) & searches for a source file.

(define (locate-existing-source-file name)
  (locate-extension name *source-file-extensions*))

(define (locate-extension name extensions)
  (if (null? extensions)
      '#f
      (let ((name-1 (add-extension name (car extensions))))
	(if (file-exists? name-1)
	    name-1
	    (locate-extension name (cdr extensions))))))


;;; This delivers the name of a compilation unit.  The extension of the name
;;; is ignored & a test for the presence of a compilation unit with 
;;; the same name is done.  If none is found, signal an error.

(define (find-cunit-name name)
  (or (locate-existing-cunit name)
      (locate-existing-source-file name)
      (signal-file-not-found name)))



;;;=====================================================================
;;; Compilation unit file parsing
;;;=====================================================================

;;; This parses a unit file.  The file simply contains a list of file names.
;;; The files are sorted into two catagories: other compilation units and
;;; source files in the current unit.  When a file has no extension, the system
;;; checks for a unit file first and then a source file.

(define (parse-compilation-unit filename)
 (let ((unit-type (filename-type filename)))
  (if (or (source-extension? unit-type) (interface-extension? unit-type))
      (create-ucache filename filename (list filename) '() '() '#f '#t
		     '#f '() '#f '() '#f)
      (parse-compilation-unit-aux
        filename
	(call-with-input-file filename (function gather-file-names))))))

(define (create-ucache filename output-filename
		       source-files imports lisp-files
		       stable? load-prelude?
		       printers-set? printers optimizers-set? optimizers
		       chunk-size)
  (let* ((cifilename
	  (make-cifilename output-filename))
	 (sifilename
	  (make-sifilename output-filename))
	 (all-imports
	  (if load-prelude?
	      (cons *prelude-unit-filename* imports)
	      imports))
	 (cache-entry
	  (make ucache
		(ufile filename)
		(sifile sifilename)
		(cifile cifilename)
		(sfile (make-sfilename output-filename))
		(cfile (make-cfilename output-filename))
		(udate (current-date))
		(idate (get-latest-ifiledate cifilename sifilename))
		(stable? stable?)
		(load-prelude? load-prelude?)
		(status 'loading)
		(ifile-loaded '#f)
		(code-loaded '#f)
		(source-files source-files)
		(imported-units all-imports)
		(lisp-files lisp-files)
		(modules '())
		(printers-set? printers-set?)
		(printers printers)
		(optimizers-set? optimizers-set?)
		(optimizers optimizers)
		(chunk-size chunk-size))))
    (install-compilation-unit filename cache-entry)
    cache-entry))

(define (get-latest-ifiledate cifilename sifilename)
  (max (or (and (file-exists? cifilename)
		(file-write-date cifilename))
	   0)
       (or (and (file-exists? sifilename)
		(file-write-date sifilename))
	   0)))


;;; This returns a list of strings.  Blank lines and lines starting in -
;;; are ignored.

(define (gather-file-names port)
  (let ((char (peek-char port)))
    (cond ((eof-object? char)
	   '())
	  ((or (char=? char '#\newline) (char=? char '#\-))
	   (read-line port)
	   (gather-file-names port))
	  (else
	   (let ((line (read-line port)))
	     (cons line (gather-file-names port)))))))


;;; Actually parse contents of the unit file.

;;; These are in the command-interface stuff.
(predefine (set-printers args mode))
(predefine (set-optimizers args mode))
(predefine (parse-command-args string start next end))

(define (parse-compilation-unit-aux filename strings)
  (let ((input-defaults   filename)
	(output-defaults  filename)
	(import-defaults  filename)
	(stable?          '#f)
	(load-prelude?    '#t)
	(filenames        '())
	(imports          '())
	(sources          '())
	(lisp-files       '())
	(printers         '())
	(printers-set?    '#f)
	(optimizers       '())
	(optimizers-set?  '#f)
	(chunk-size       '#f)
	(temp             '#f))
    ;;; First look for magic flags.
    (dolist (s strings)
      (cond ((setf temp (string-match-prefix ":input" s))
	     (setf input-defaults (merge-file-defaults temp filename)))
	    ((setf temp (string-match-prefix ":output" s))
	     (setf output-defaults (merge-file-defaults temp filename)))
	    ((setf temp (string-match-prefix ":import" s))
	     (setf import-defaults (merge-file-defaults temp filename)))
	    ((string=? ":stable" s)
	     (setf stable? '#t))
	    ((string=? ":prelude" s)
	     (setf load-prelude? '#f))
	    ((setf temp (string-match-prefix ":p=" s))
	     (setf printers-set? '#t)
	     (setf printers
		   (set-printers
		      (parse-command-args temp 0 0 (string-length temp))
		      '=)))
	    ((setf temp (string-match-prefix ":o=" s))
	     (setf optimizers-set? '#t)
	     (setf optimizers
		   (set-optimizers
                      (parse-command-args temp 0 0 (string-length temp))
		      '=)))
	    ((setf temp (string-match-prefix ":chunk-size" s))
	     (setf chunk-size (string->number temp)))
	    (else
	     (push s filenames))))
    ;;; Next sort filenames into imports and source files.
    (dolist (s filenames)
      (let ((type    (filename-type s))
	    (fname   '#f))
	(cond ((string=? type "")  ; punt for now on this issue
	       (signal-extension-needed s))
;	      ((cond ((setf fname 
;			    (locate-existing-cunit
;			      (merge-file-defaults s import-defaults)))
;		      (push fname imports))
;		     ((setf fname
;			    (locate-existing-source-file
;			      (merge-file-defaults s input-defaults)))
;		      (push fname sources))
;		     (else
;		      (signal-unit-not-found s))))
	      ((unit-extension? type)
	       (setf fname  (merge-file-defaults s import-defaults))
	       (if (file-exists? fname)
		   (push fname imports)
		   (signal-unit-not-found fname)))
	      ((or (source-extension? type) (interface-extension? type))
	       (setf fname  (merge-file-defaults s input-defaults))
	       (if (file-exists? fname)
		   (push fname sources)
		   (signal-unit-not-found fname)))
	      ((lisp-extension? type)
	       (setf fname (merge-file-defaults s input-defaults))
	       (if (file-exists? fname)
		   (push (cons fname
			       (add-extension
			         (merge-file-defaults s output-defaults)
				 binary-file-type))
			 lisp-files)
		   (signal-unit-not-found fname)))
	      (else
	       (signal-unknown-file-type s)))))
    ;; Finally create the unit object.
    (create-ucache filename output-defaults
		   sources imports lisp-files
		   stable? load-prelude?
		   printers-set? printers optimizers-set? optimizers
		   chunk-size)))


;;; Helper functions for the above.

(define (string-match-prefix prefix s)
  (let ((prefix-length  (string-length prefix))
	(s-length       (string-length s)))
    (if (>= s-length prefix-length)
	(string-match-prefix-aux prefix s prefix-length s-length 0)
	'#f)))

(define (string-match-prefix-aux prefix s prefix-length s-length i)
  (cond ((eqv? i prefix-length)
	 (string-match-prefix-aux-aux s s-length i))
	((not (char=? (string-ref s i) (string-ref prefix i)))
	 '#f)
	(else
	 (string-match-prefix-aux prefix s prefix-length s-length (1+ i)))))

(define (string-match-prefix-aux-aux s s-length i)
  (cond ((eqv? i s-length)
	 "")
	((let ((ch  (string-ref s i)))
	   (or (char=? ch '#\space) (char=? ch #\tab)))
	 (string-match-prefix-aux-aux s s-length (1+ i)))
	(else
	 (substring s i s-length))))

(define (merge-file-defaults filename defaults)
  (let ((place  (filename-place filename))
	(name   (filename-name filename))
	(type   (filename-type filename)))
    (assemble-filename
      (if (string=? place "") defaults place)
      (if (string=? name "") defaults name)
      (if (string=? type "") defaults type))))
    
    
;;;=====================================================================
;;; Guts
;;;=====================================================================


;;; This is the main entry to the compilation system.  This causes a
;;; unit to be compiled and/or loaded.

(define (load-compilation-unit filename cflags)
  (let ((cunit (lookup-compilation-unit filename)))
    (cond ((eq? cunit '#f)
	   ;; Unit not found in cache.
	   (load-compilation-unit-aux
	     (parse-compilation-unit filename) cflags))
	  ((eq? (ucache-status cunit) 'loaded)
	   ;; Already loaded earlier in this compile.
	   cunit)
	  ((eq? (ucache-status cunit) 'loading)
	   (signal-circular-unit filename))
	  (else
	   (load-compilation-unit-aux cunit cflags))
	  )))


(define (load-compilation-unit-aux c cflags)
  (setf (ucache-status c) 'loading)
  (load-imported-units c cflags)
  (if (unit-valid? c cflags)
      (load-compiled-unit c (cflags-load-code? cflags))
      (locally-compile c cflags))
  (setf (ucache-status c) 'loaded)
  ;; Hack, hack.  When loading the prelude, make sure magic symbol
  ;; table stuff is initialized.
  (when (string=? (ucache-ufile c) *prelude-unit-filename*)
    (init-prelude-globals))
  c)

(define (load-compiled-unit c load-code?)
  (when (and load-code? (not (ucache-code-loaded c)))
    (when (memq 'loading *printers*)
      (format '#t "~&Loading unit ~s.~%" (ucache-ufile c))
      (force-output))
    (load-lisp-files (ucache-lisp-files c))
    (load-more-recent-file (ucache-cfile c) (ucache-sfile c))
    (setf (ucache-code-loaded c) '#t))
  (when (not (ucache-ifile-loaded c))
     (read-binary-interface c))
  (dolist (m (ucache-modules c))
      (add-module-to-symbol-table m))
  (link-instances (ucache-modules c)))


;;; These globals save the Prelude symbol table to avoid copying it
;;; into all modules which use the Prelude.

;;; Danger!  This assumes that every local symbol in the Prelude is
;;; exported.

(define *prelude-initialized* '#f)

(define (init-prelude-globals)
  (when (not *prelude-initialized*)
    (let ((pmod (locate-module '|Prelude|)))
      (setf *prelude-symbol-table* (module-symbol-table pmod))
      (setf *prelude-fixity-table* (module-fixity-table pmod))
      (when (eq? (module-inverted-symbol-table pmod) '#f)
	(let ((table (make-table)))
	  (table-for-each (lambda (name def)
			    (setf (table-entry table def) name))
			  *prelude-symbol-table*)
	  (setf (module-inverted-symbol-table pmod) table)))
      (setf *prelude-inverted-symbol-table*
	    (module-inverted-symbol-table pmod)))
    (setf *prelude-initialized* '#t)))


;;; This recursively loads all units imported by a given unit.

(define (load-imported-units c cflags)
  (dolist (filename (ucache-imported-units c))
    (load-compilation-unit filename cflags)))



;;; Load or compile lisp files.

(define (load-lisp-files lisp-files)
  (dolist (f lisp-files)
    (load-more-recent-file (cdr f) (car f))))

(define (compile-lisp-files lisp-files)
  (dolist (f lisp-files)
    (let ((source  (car f))
	  (binary  (cdr f)))
      (when (not (lisp-binary-current source binary))
	(compile-file source binary))
      (load binary))))



;;; This determines whether a unit is valid.

(define (unit-valid? c cflags)
  (and (or (ucache-stable? c)
	   ;; If the unit is not stable, make sure its source files
	   ;; haven't changed.
	   (and (all-imports-current (ucache-imported-units c)
				     (ucache-idate c))
		(all-sources-current (ucache-source-files c)
				     (ucache-idate c))
		(all-lisp-sources-current (ucache-lisp-files c)
					  (ucache-idate c))))
       (or (ucache-ifile-loaded c)
	   ;; If the interface hasn't been loaded already, make sure
	   ;; that the interface file exists.
	   (file-exists? (ucache-cifile c))
	   (file-exists? (ucache-sifile c)))
       (or (not (cflags-load-code? cflags))
	   ;; If we're going to load code, make sure that the code file
	   ;; exists.
	   (ucache-code-loaded c)
	   (file-exists? (ucache-cfile c))
	   (file-exists? (ucache-sfile c)))
       (or (not (cflags-write-code? cflags))
	   ;; If we need to produce a code file, make sure this has
	   ;; already been done.
	   ;; Don't write files for stable units which have already
	   ;; been loaded, regardless of whether or not the file exists.
	   (and (ucache-stable? c) (ucache-code-loaded c))
	   (file-exists? (ucache-cfile c))
	   (and (not (cflags-compile-code? cflags))
		(file-exists? (ucache-sfile c))))
       (or (not (cflags-compile-code? cflags))
	   ;; If we need to compile the lisp files, make sure this has
	   ;; already been done.
	   ;; Don't do this for stable units which have already
	   ;; been loaded.
	   (and (ucache-stable? c) (ucache-code-loaded c))
	   (all-lisp-binaries-current (ucache-lisp-files c)))
       (or (not (cflags-write-interface? cflags))
	   ;; If we need to produce an interface file, make sure this has
	   ;; already been done.
	   ;; Don't write files for stable units which have already
	   ;; been loaded, regardless of whether or not the file exists.
	   (and (ucache-stable? c) (ucache-ifile-loaded c))
	   (file-exists? (ucache-cifile c))
	   (and (not *compile-interface*)
		(file-exists? (ucache-sifile c))))
       ))

(define (all-sources-current sources unit-write-date)
  (every (lambda (s)
	   (let ((d  (file-write-date s)))
	     (and d (> unit-write-date d))))
	 sources))

(define (all-imports-current imports unit-write-date)
  (every (lambda (s) (> unit-write-date
			(ucache-idate (lookup-compilation-unit s))))
	 imports))

(define (all-lisp-sources-current lisp-files unit-write-date)
  (every (lambda (s)
	   (let ((d  (file-write-date (car s))))
	     (and d (> unit-write-date d))))
	 lisp-files))

(define (all-lisp-binaries-current lisp-files)
  (every (lambda (s)
	   (lisp-binary-current (car s) (cdr s)))
	 lisp-files))

(define (lisp-binary-current source binary)
  (and (file-exists? binary)
       (let ((sd  (file-write-date source))
	     (bd  (file-write-date binary)))
	 (and sd bd (> bd sd)))))


;;; This does the actual job of compilation.

(define (locally-compile c cflags)
  (dynamic-let ((*printers*
		  (if (ucache-printers-set? c)
		      (ucache-printers c)
		      (dynamic *printers*)))
		(*optimizers*
		  (if (ucache-optimizers-set? c)
		      (ucache-optimizers c)
		      (dynamic *optimizers*))))
    (when (memq 'compiling *printers*)
       (format '#t "~&Compiling unit ~s.~%Optimizers: ~A~%"
	       (ucache-ufile c)
	       *optimizers*)
	       (force-output))
    (if (cflags-compile-code? cflags)
	(compile-lisp-files (ucache-lisp-files c))
	(load-lisp-files (ucache-lisp-files c)))
    (multiple-value-bind (mods code)
	(compile-haskell-files (ucache-source-files c))
      ;; General bookkeeping to update module interface in cache.
      (setf (ucache-modules c) mods)
      (setf (ucache-idate c) (current-date))
      (setf (ucache-ifile-loaded c) '#t)
      ;; Write interface file if necessary.
      (when (cflags-write-interface? cflags)
	(let ((phase-start-time (get-run-time))
	      (icode  (create-dump-code c mods (ucache-load-prelude? c))))
	  (if (dynamic *compile-interface*)
	      (write-compiled-code-file
	        (ucache-cifile c)
		icode
		(dynamic *interface-code-quality*)
		(dynamic *interface-chunk-size*))
	      (write-interpreted-code-file (ucache-sifile c) icode '#f))
	  (when (memq 'phase-time *printers*)
	    (let* ((current-time (get-run-time))
		   (elapsed-time (- current-time phase-start-time)))
	      (format '#t "Interface complete: ~A seconds~%" elapsed-time)
	      (force-output)))))
      ;; Write code file if necessary.
      (when (cflags-write-code? cflags)
	(if (cflags-compile-code? cflags)
	    (write-compiled-code-file
	      (ucache-cfile c)
	      code
	      (if (memq 'lisp (dynamic *optimizers*))
		  (dynamic *optimized-code-quality*)
		  (dynamic *default-code-quality*))
	      (or (ucache-chunk-size c) (dynamic *code-chunk-size*)))
	    (write-interpreted-code-file (ucache-sfile c) code '#t)))
      ;; Load or evaluate code if necessary.
      ;; If we just wrote a compiled code file, load that; otherwise
      ;; do eval or in-core compilation.
      (when (cflags-load-code? cflags)
	(if (and (cflags-write-code? cflags)
		 (cflags-compile-code? cflags))
	    (load (ucache-cfile c))
	    (eval code (cflags-compile-code? cflags)))
	(setf (ucache-code-loaded c) '#t))
      )))



;;;=====================================================================
;;; Cache manager
;;;=====================================================================

;;; This is the cache manager for compilation units.  We use an alist at
;;; the moment.

(define *unit-cache* '())

(define (reset-unit-cache)
  (setf *unit-cache* '()))


;;; This checks to make sure that the compilation unit it finds
;;; in the cache has not been made out-of-date by updates to the unit file.

(define (lookup-compilation-unit name)
  (let ((r (ass-string name *unit-cache*)))
    (if r
	(let ((c  (cdr r)))
	 (if (or (ucache-stable? c)
		 (> (ucache-udate c)
		    (or (file-write-date (ucache-ufile c)) 0)))
	     c
	     '#f))
	'#f)))

(define (install-compilation-unit name c)
  (let ((r (ass-string name *unit-cache*)))
    (if (eq? r '#f)
	(push (cons name c) *unit-cache*)
	(setf (cdr r) c))))

(define (for-each-unit proc)
  (dolist (c *unit-cache*)
     (funcall proc (cdr c))))


;;;=====================================================================
;;; Error utilities
;;;=====================================================================

(define (signal-circular-unit filename)
  (fatal-error 'circular-unit
    "The compilation unit ~a has a circular dependency."
    filename))

(define (signal-unit-not-found filename)
  (fatal-error 'unit-not-found
    "The compilation unit file ~a was not found."
    filename))

(define (signal-extension-needed filename)
  (fatal-error 'extension-needed
    "You must provide an extension on the filename ~a in the .hu file."
     filename))





