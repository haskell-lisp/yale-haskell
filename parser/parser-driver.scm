
;;; This is the top level entry to the parse.  The input is a list of file
;;; names to be parsed and the output is a list of modules.  Interface files
;;; generate modules similar to ordinary files.  

(define (parse-files filenames)
  (let ((all-mods '()))
    (dolist (file filenames)
      (let* ((ext (filename-type file))
	     (mods (cond ((string=? ext ".hs")
			  (parse-single-file file))
			 ((string=? ext ".lhs")
			  (parse-single-file/literate file))
			 ((string=? ext ".hi")
			  (parse-single-file/interface file)))))
	   (setf all-mods (append all-mods mods))))
    all-mods))

(define (parse-single-file filename)
  (parse-single-file-1 filename '#f '#f))

(define (parse-single-file/literate filename)
  (parse-single-file-1 filename '#t '#f))

(define (parse-single-file/interface filename)
  (parse-single-file-1 filename '#f '#t))

(define (parse-single-file-1 filename literate? interface?)
  (when (memq 'reading *printers*)
      (format '#t "Reading Haskell source file ~s.~%" filename))
  (when (not (file-exists? filename))
    (signal-file-not-found filename))
  (dynamic-let ((*current-file* filename))
    (let ((mods '()))
      (call-with-input-file filename
        (lambda (port)
	  (let* ((tokens (lex-port port literate?))
		 (module-asts (if interface?
				  (parse-tokens/interface tokens)
				  (parse-tokens tokens))))
	    (setf mods module-asts))))
      (when (memq 'parse *printers*)
	(dolist (m mods)
	  (format '#t "~%")
	  (print-full-module m)))
      mods)))


