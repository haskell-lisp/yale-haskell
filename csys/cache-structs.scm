;;; these structures deal with the compilation system and the unit cache.

;;; An entry in the unit cache:

(define-struct ucache
  (slots
    (ufile (type string))  ; the name of the file containing the unit definition
    (cifile (type string)) ; the filename of the (compiled) interface file
    (sifile (type string)) ; the filename of the (uncompiled) interface file
    (cfile (type string))  ; the filename of the (compiled) output file
    (sfile (type string))  ; the filename of the (uncompiled) output file
    (udate (type integer))   ; the write date of ufile
    (idate (type integer))   ; the time stamp of the binary interface file
    (stable? (type bool))     ; the stable flag
    (load-prelude? (type bool))  ; true if unit uses standard prelude
    ;; status is initially available (in cache).  It is set to loading when
    ;; requested and loaded once all imported units are loaded.
    (status (type (enum loaded loading available)))
    (ifile-loaded (type bool))  ; true when interface is loaded (modules)
    (code-loaded (type bool))   ; true when the associated code is in memory
    (source-files (type (list string)))  ; source files in the unit
    (imported-units (type (list string))) ; the filenames of imported unit files
    (lisp-files (type (list (tuple string string))))  ; source/binary pairs
    (modules (type (list module)))
    (printers-set? (type bool))
    (printers (type (list symbol)))
    (optimizers-set? (type bool))
    (optimizers (type (list symbol)))
    (chunk-size (type (maybe int)))
    ))


;;; This is used to hold various flags used by the compilation system,
;;; instead of passing them all as individual arguments.

(define-struct cflags
  (slots
    ;; Whether to load code for unit into core
    (load-code?           (type bool) (default '#t))
    ;; Whether to create an output code file.
    (write-code?          (type bool) (default '#t))
    ;; Affects whether write-code? creates a source or compiled file,
    ;; and whether load-code? uses the interpreter or compiler.
    ;; Ignored if load-code? and write-code? are both false.
    (compile-code?        (type bool) (default '#t))
    ;; Whether to create an output interface file.
    (write-interface?     (type bool) (default '#t))
    ))
