
;;; Globals used by the command interpreter

(define *current-string* "")
(define *current-mod* '|Main|)
(define *current-command* '())
(define *remembered-file* "Foo")
(define *fragment-status* '())
(define *temp-counter* 0)
(define *last-compiled* "")
(define *abort-command* '())
(define *command-dispatch* '())
(define *extension-module* '())
(define *extension-file-name* "interactive")

(define (prompt mod)
  (format '#f "~A> " mod))

(define-local-syntax (define-command name&args helpstr . body)
  (let* ((str (car name&args))
	 (args (cdr name&args))
	 (fname (string->symbol (string-append "CMD-" str))))
    `(begin
       (define (,fname arguments)
	 (verify-command-args ',args arguments ',helpstr)
	 (apply (lambda ,args ,@body) arguments))
       (setf *command-dispatch*
	     (nconc *command-dispatch*
		    (list (cons ',str (function ,fname)))))
       ',fname)))
		     
(define (heval)
  (initialize-haskell-system)
  (setf *current-string* "")
  (setf *fragment-status* 'Building)
  (say "~&Yale Haskell ~A~A   ~A~%Type :? for help.~%"
       *haskell-compiler-version* *haskell-compiler-update* (identify-system))
  (read-commands))


;;; This loop reads commands until a quit 

(define (read-commands)
  (do ((cmd-status (read-command) (read-command)))
      ((eq? cmd-status 'quit-command-loop) (exit))))

;;; This processes a single line of input.

(define (read-command)
  (let/cc abort-command
    (setf *abort-command* (lambda () (funcall abort-command 'error)))
    (setf *abort-compilation* *abort-command*)
    (setf *phase* 'command-interface)
    (setf *in-error-handler?* '#f)
    (ready-for-input-line)
    (let ((ch (peek-char)))
      (cond ((eof-object? ch)
	     'quit-command-loop)
	    ((char=? ch '#\:)
	     (read-char)
	     (execute-command))
	    ((and (char=? ch '#\newline)
		  (not (eq? *fragment-status* 'Building)))
	     (read-char)
	     'Ignored)
	    (else
	     (when (not (eq? *fragment-status* 'Building))
	       (setf *fragment-status* 'Building)
	       (setf *current-string* ""))
	     (cond ((eqv? ch '#\=)
		    (read-char)
		    (append-to-current-string (expand-print-abbr (read-line))))
		   ((eqv? ch '#\@)	
		    (read-char)
		    (append-to-current-string (expand-exec-abbr (read-line))))
		   (else
		    (append-to-current-string (read-line))))
	     'OK)
	    ))))

(define (append-to-current-string string)
  (setf *current-string*
	(string-append *current-string*
		       string
		       (string #\newline))))


(define (expand-print-abbr string)
  (incf *temp-counter*)
  (format '#f "temp_~a = print temp1_~a where temp1_~a = ~a"
	  *temp-counter* *temp-counter* *temp-counter* string))

(define (expand-exec-abbr string)
  (incf *temp-counter*)
  (format '#f "temp_~a :: Dialogue~%temp_~a = ~a"
	  *temp-counter* *temp-counter* string))


(define (ready-for-input-line)
  (when (not *emacs-mode*)
     (fresh-line (current-output-port))
     (write-string (prompt *current-mod*) (current-output-port))
     (force-output (current-output-port)))
  (notify-ready))

(define (execute-command)
  (if (char=? (peek-char) '#\() ;this is the escape to the lisp evaluator
      (let ((form (read)))
	(eval form)
	'OK)
      (let* ((string    (read-line))
	     (length    (string-length string))
	     (cmd+args  (parse-command-args string 0 0 length)))
	(cond ((null? cmd+args)
	       (say "~&Eh?~%")
	       'OK)
	      (else
	       (let ((fn (assoc/test (function string-starts?)
				     (car cmd+args)
				     *command-dispatch*)))
		 (cond ((eq? fn '#f)
			(say "~&~A: unknown command.  Use :? for help.~%"
			     (car cmd+args))
			'OK)
		       (else
			(funcall (cdr fn) (cdr cmd+args))))))))))


;;; This parses the command into a list of substrings.  
;;; Args are separated by spaces.

(define (parse-command-args string start next end)
  (declare (type fixnum start next end)
	   (type string string))
  (cond ((eqv? next end)
	 (if (eqv? start next)
	     '()
	     (list (substring string start next))))
	((char=? (string-ref string next) '#\space)
	 (let ((next-next  (+ next 1)))
	   (if (eqv? start next)
	       (parse-command-args string next-next next-next end)
	       (cons (substring string start next)
		     (parse-command-args string next-next next-next end)))))
	(else
	 (parse-command-args string start (+ next 1) end))))

(define (verify-command-args template args help)
  (cond ((and (null? template) (null? args))
	 '#t)
	((symbol? template)
	 '#t)
	((or (null? template) (null? args))
	 (say "~&Command error.~%~A~%" help)
	 (funcall *abort-command*))
	(else
	 (verify-command-args (car template) (car args) help)
	 (verify-command-args (cdr template) (cdr args) help))))

(define-command ("?")
  ":?            Print the help file."
  (print-file "$HASKELL/command-interface-help"))

(define-command ("eval")
  ":eval            Evaluate current extension."
  (eval-fragment '#t)
  'OK)

(define-command ("save")
  ":save     Save current extension"
  (eval-fragment '#f)
  (cond ((eq? *fragment-status* 'Error)
	 (say/ne "~&Cannot save: errors encountered.~%"))  
	((eq? *fragment-status* 'Compiled)
	 (extend-module *current-mod* *extension-module*)
	 (setf *fragment-status* 'Saved)))
  'OK)

(define-command ("quit")
  ":quit        Quit the Haskell evaluator."
  'quit-command-loop)

(define-command ("module" mod)
  ":module module-name    Select module for incremental evaluation."
  (setf *current-mod* (string->symbol mod))
  (when (not (cm-available?))
      (say/ne "~&Warning: module ~A is not currently loaded.~%" *current-mod*))
  'OK)

(define-command ("run" . file)
  ":run <file>   Compile, load, and run a file."
  (set-current-file file)
  (clear-extended-modules)
  (let ((mods (compile/load *remembered-file*)))
    (when (pair? mods)
      (dolist (m mods)
	 (eval-module (table-entry *modules* m)))))
  'OK)

(define-command ("compile" . file)
  ":compile <file> Compile and load a file."
  (set-current-file file)
  (clear-extended-modules)
  (select-current-mod (compile/compile *remembered-file*))
  'OK)

(define-command ("load" . file)
  ":load <file>      Load a file."
  (set-current-file file)
  (clear-extended-modules)
  (select-current-mod (compile/load *remembered-file*))
  'OK)

(define-command ("Main")
  ":Main           Switch to an empty Main module."
  (make-empty-main)
  'OK)

(define-command ("clear")
  ":clear   Clear saved definitions from current module."
  (remove-extended-modules *current-mod*)
  (setf *current-string* "")
  (setf *fragment-status* 'Building))

(define-command ("list")
  ":list          List current extension."
  (say "~&Current Haskell extension:~%~a" *current-string*)
  (cond ((eq? *fragment-status* 'Error)
	 (say "Extension contains errors.~%"))  
	((eq? *fragment-status* 'Compiled)
	 (say "Extension is compiled and ready.~%")))
  'OK)

(define-command ("kill")
  ":kill      Clear the current fragment."
  (when (eq? *fragment-status* 'Building)
    (setf *current-string* ""))
  'OK)

(define-command ("p?")
  ":p?            Show available printers."
  (if *emacs-mode*
      (notify-printers (dynamic *printers*))
      (begin
	(print-file "$HASKELL/emacs-tools/printer-help.txt")
	(say "~&Active printers: ~A~%" (dynamic *printers*)))
    ))

(define-command ("p=" . passes)
  ":p= pass1 pass2 ...  Set printers."
  (setf *printers* (set-printers passes '=))
  (say/ne "~&Setting printers: ~A~%" *printers*))

(define-command ("p+" . passes)
  ":p+ pass1 pass2 ...  Add printers."
  (setf *printers* (set-printers passes '+))
  (say/ne "~&Setting printers: ~A~%" *printers*))

(define-command ("p-" . passes)
  ":p- pass1 pass2 ...  Turn off printers."
  (setf *printers* (set-printers passes '-))
  (say/ne "~&Setting printers: ~A~%" *printers*))



(define-command ("o?")
  ":o?            Show available optimizers."
  (if *emacs-mode*
      (notify-optimizers (dynamic *optimizers*))
      (begin
	(print-file "$HASKELL/emacs-tools/optimizer-help.txt")
	(say "~&Active optimizers: ~A~%" (dynamic *optimizers*)))
    ))

(define-command ("o=" . optimizers)
  ":o= optimizer1 optimizer2 ...  Set optimizers."
  (setf *optimizers* (set-optimizers optimizers '=))
  (say/ne "~&Setting optimizers: ~A~%" *optimizers*))

(define-command ("o+" . optimizers)
  ":o+ optimizer1 optimizer2 ...  Add optimizers."
  (setf *optimizers* (set-optimizers optimizers '+))
  (say/ne "~&Setting optimizers: ~A~%" *optimizers*))

(define-command ("o-" . optimizers)
  ":o- optimizer1 optimizer2 ...  Turn off optimizers."
  (setf *optimizers* (set-optimizers optimizers '-))
  (say/ne "~&Setting optimizers: ~A~%" *optimizers*))


(define-command ("cd" d)
  ":cd directory   Change the current directory."
  (cd d)
  'OK)

(define-command ("Emacs" mode)
  ":Emacs on/off   Turn on or off emacs mode."
  (cond ((string=? mode "on")
	 (setf *emacs-mode* '#t))
	((string=? mode "off")
	 (setf *emacs-mode* '#f))
	(else
	 (say "~&Use on or off.~%"))))

(define-command ("file" name)
  ":file name"
  (setf *extension-file-name* name)
  'OK)
