;;; This contains parser error handlers.  They, in turn, call the
;;; system error handlers.

(define (lexer-error id . msgs)
  (parser-error/common id 'recoverable msgs '#t)
  `#\?)

(define (parser-error id . msgs)
  (parser-error/common id 'phase msgs '#f)
  (if (null? *layout-stack*)
      (abort-compilation)
      (recover-to-next-decl *token-stream*)))

(define (parser-error/recoverable id . args)
  (parser-error/common id 'recoverable args '#f))

(define (parser-error/common id type msgs in-lexer?)
  (let ((place
	 (if in-lexer?
	     (list "Error occured at in file ~A at line ~A, column ~A."
		   *current-file* *current-line* *current-col*)
	     (list "Error occured at in file ~A at line ~A, token ~A."
		   *current-file* *current-line*
		   (cond ((null? *token-args*)
			  *token*)
			 ((null? (cdr *token-args*))
			  (car *token-args*))
			 (else *token-args*)))))) ; could be better
    (haskell-error id type (list place msgs))))

(define (recover-to-next-decl tokens)
  (cond ((null? tokens)
	 (abort-compilation))
	((eq? (car (car tokens)) 'line)
	 (search-layout-stack *layout-stack* tokens (caddr (car tokens))))
	(else (recover-to-next-decl (cdr tokens)))))

(define (search-layout-stack layouts tokens column)
  (cond ((null? layouts)
	 (abort-compilation))
	((> column (layout-col (car layouts)))
	 (recover-to-next-decl (cdr tokens)))
	((= column (layout-col (car layouts)))
	 (setf *current-col* column)
	 (setf *current-line* (cadr (car tokens)))
	 (setf *token-stream* (cdr tokens))
	 (advance-token)  ; loads up *token*
	 ;; *** layout-recovery-fn is not defined anywhere!
	 (funcall (layout-recovery-fn (car layouts))))
	(else
	 (setf *layout-stack* (cdr *layout-stack*))
	 (search-layout-stack (cdr layouts) tokens column))))


;;; Here are some very commonly used signalling functions.
;;; Other (more specific) signalling functions are defined near
;;; the places where they are called.


;;; This is used when a particular token isn't found.

(define (signal-missing-token what where)
  (parser-error 'missing-token
		"Missing ~a in ~a." what where))


;;; This is used to signal more complicated parse failures involving
;;; failure to match a nonterminal.

(define (signal-invalid-syntax where)
  (parser-error 'invalid-syntax
		"Invalid syntax appears where ~a is expected." where))


