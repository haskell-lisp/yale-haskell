;;; command-interface/command-utils.scm

;;; These are utilities used by the command interface.

;;; These send output to the user

;;; This is used in emacs mode

(define (say/em . args)
  (say1 args))

;;; This is for both ordinary text to emacs and output to the command interface

(define (say . args)
  (say1 args))

(define (say1 args)
  (apply (function format) (cons (current-output-port) args)))

;;; This is for non-emacs output

(define (say/ne . args)
  (when (not *emacs-mode*)
     (say1 args)))


;;; These random utilities should be elsewhere

;;; This determines whether the current module is loaded & available.
;;; If the module is Main, an empty Main module is created.

(define (cm-available?)
  (cond ((table-entry *modules* *current-mod*)
	 '#t)
	((eq? *current-mod* '|Main|)
	 (make-empty-main)
	 '#t)
	(else
	 '#f)))

;;; This creates a empty module named Main to use as a scratch pad.

(define (make-empty-main)
  (compile/load "$PRELUDE/Prelude")
  (setf *unit* '|Main|)
  (setf *current-mod* '|Main|)
  (let ((mods (parse-from-string
	       "module Main where {import Prelude}"
	       (function parse-module-list)
	       "foo")))
    ;;; This should generate no code at all so the returned code is ignored.
    (modules->lisp-code mods)
    (setf (table-entry *modules* *current-mod*) (car mods))
    (clear-extended-modules)))

(define (eval-fragment eval?)
  (cond ((not (cm-available?))
	 (say "~&Module ~A is not loaded.~%" *current-mod*)
	 'error)
	((memq *fragment-status* '(Compiled Saved))
	 (when eval?
	     (eval-module *extension-module*))
	 'ok)
	((eq? *fragment-status* 'Error)
	 (say/ne "~&Errors exist in current fragment.~%")
	 'error)
	((string=? *current-string* "")
	 (say/ne "~&Current extension is empty.~%")
	 'error)
	(else
	 (let ((res (compile-fragment
		     *current-mod* *current-string*
		     *extension-file-name*)))
	   (cond ((eq? res 'error)
		  (setf *fragment-status* 'Error)
		  (notify-error))
		 (else
		  (setf *extension-module* res)
		  (setf *fragment-status* 'Compiled)
		  (when eval?
			(eval-module *extension-module*))))))))

(define (set-current-file file)
  (cond ((null? file)
	 '())
	((null? (cdr file))
	 (setf *remembered-file* (car file)))
	(else
	 (say "~&Invalid file spec ~s.~%" file)
	 (funcall *abort-command*))))

(define (select-current-mod mods)
  (when (pair? mods)
    (when (not (memq *current-mod* mods))
      (setf *current-mod* (car mods))
      (say/ne "~&Now in module ~A.~%" *current-mod*))))

;;;  Emacs mode stuff

;;; *** bogus alert!!!  This coercion may fail to produce a
;;; *** real character in some Lisps.

(define *emacs-notify-char* (integer->char 1))

(define (notify-ready)
  (when *emacs-mode*
     (say/em "~Ar" *emacs-notify-char*)
     (force-output (current-output-port))))

(define (notify-input-request)
  (when *emacs-mode*
     (say/em "~Ai" *emacs-notify-char*)
     (force-output (current-output-port))))

(define (notify-error)
  (when *emacs-mode*
     (say/em "~Ae" *emacs-notify-char*)
     (force-output (current-output-port))))

(define (notify-printers printers)
  (notify-settings "p" printers))

(define (notify-optimizers optimizers)
  (notify-settings "o" optimizers))

(define (notify-settings flag values)
  (when *emacs-mode*
    (say/em "~A~A(" *emacs-notify-char* flag)
    (dolist (p values)
      (say/em " ~A" (string-downcase (symbol->string p))))
    (say/em ")~%")
    (force-output (current-output-port))))

(define (notify-status-line str)
  (when *emacs-mode*
     (say/em "~As~A~%" *emacs-notify-char* str)
     (force-output (current-output-port))))

;;; These are used to drive the real compiler. 

(define *compile/compile-cflags*
  (make cflags
	(load-code?          '#t)
	(compile-code?       '#t)
	(write-code?         '#t)
	(write-interface?    '#t)))


(define (compile/compile file)
  (haskell-compile file *compile/compile-cflags*))


(define *compile/load-cflags*
  (make cflags
	(load-code?          '#t)
	(compile-code?       '#f)
	(write-code?         '#f)
	(write-interface?    '#f)))

(define (compile/load file)
  (haskell-compile file *compile/load-cflags*))


;;; Printer setting support

(define (set-printers args mode)
  (set-switches *printers* (strings->syms args)
		mode *all-printers* "printers"))

(define (set-optimizers args mode)
  (set-switches *optimizers* (strings->syms args)
		mode *all-optimizers* "optimizers"))

(define (set-switches current new mode all name)
  (dolist (s new)
    (when (and (not (eq? s 'all)) (not (memq s all)))
      (signal-invalid-value s name all)))
  (let ((res (cond ((eq? mode '+)
		    (set-union current new))
		   ((eq? mode '-)
		    (set-difference current new))
		   ((eq? mode '=)
		    (if (equal? new '(all))
			all
			new)))))
    res))

(define (signal-invalid-value s name all)
  (recoverable-error 'invalid-value
    "~A is not one of the valid ~A.  Possible values are: ~%~A"
    s name all))

(define (print-file file)
  (call-with-input-file file (function write-all-chars)))

(define (write-all-chars port)
  (let ((line  (read-line port)))
    (if (eof-object? line)
	'ok
	(begin
	  (write-line line)
	  (write-all-chars port)))))

(define (strings->syms l)
  (map (lambda (x)
	 (string->symbol (string-upcase x)))
       l))

