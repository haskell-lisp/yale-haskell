;;; This file contains general error handling routines.

;;; This is the general error handler.  It has three arguments: an
;;; id, error type, and an error message.  The message is a list of
;;; format, arglist combinations.

;;; The error types are:
;;;   warning       -> control returns and compilation proceeds
;;;                    The message may be suppressed
;;;   recoverable   -> control returns and compilation proceeds
;;;   phase         -> control returns but compilation is aborted
;;;                         after the phase in *abort-point*.
;;;   fatal         -> control goes back to the top level
;;;   internal      -> enters the break loop or does a fatal error

;;; Two globals control error behavior:
;;;   *break-on-error?* enter the break loop on any error
;;;   *never-break?* never enter the break loop, even for internal errors.

;;; The global *error-output-port* controls where errors are printer.

;;; The strategy here is to first write a banner message based on the id and
;;; type, write out the messages, and then take action depending on the type.

(define *in-error-handler?* '#f)

(define (haskell-error id type messages)
  (format *error-output-port* "~&[~A] ~A in phase ~A:~%"
	  id (err-type->banner type) (dynamic *phase*))
  (dolist (m messages)
    (apply (function format) *error-output-port* m)
    (fresh-line *error-output-port*))
  (maybe-show-context (dynamic *context*))
  (if (dynamic *in-error-handler?*)
      (error "Recursive error in haskell-error.")
      (begin
        (dynamic-let ((*in-error-handler?*  '#t))
	  (cond (*break-on-error?*
		 (haskell-breakpoint))
		((eq? type 'internal)
		 (if *never-break?*
		     (abort-compilation)
		     (haskell-breakpoint)))
		((eq? type 'fatal)
		 (abort-compilation))
		((eq? type 'phase)
		 (halt-compilation))))
	(when (and (memq type '(recoverable phase))
		   (dynamic *recoverable-error-handler*))
	  (funcall (dynamic *recoverable-error-handler*)))
	'ok)))

(define (err-type->banner err-type)
  (cond ((eq? err-type 'warning)
	 "Warning")
	((eq? err-type 'recoverable)
	 "Recoverable error")
	((eq? err-type 'phase)
	 "Phase error")
	((eq? err-type 'fatal)
	 "Fatal error")	
	((eq? err-type 'internal)
	 "Internal-error")
	(else "???")))

(define (maybe-show-context context)
  (when context
    (with-slots source-pointer (line file) (ast-node-line-number context)
      (fresh-line *error-output-port*)
      (format *error-output-port* "Error occurred at line ~A in file ~A.~%"
         line (filename-name file)))))

;;; A few entry points into the error system.
;;; As a matter of convention, there should be a signaling function defined
;;; for each specific error condition that calls one of these functions.
;;; Error messages should be complete sentences with proper punctuation
;;; and capitalization.  The signaling function should use the message
;;; to report the error and not do any printing of its own.

(define (fatal-error id . msg)
 (haskell-error id 'fatal (list msg)))

(define (haskell-warning id . msg)
 (haskell-error id 'warning (list msg)))

(define (recoverable-error id . msg)
 (haskell-error id 'recoverable (list msg)))

(define (compiler-error id . msg)
 (haskell-error id 'internal (list msg)))

(define (phase-error id . msg)
 (haskell-error id 'phase (list msg)))

;;; This function puts the compiler into the lisp breakloop.  this may
;;; want to fiddle the programming envoronment someday.

(define (haskell-breakpoint)
 (error "Haskell breakpoint."))


;;; This deals with error at runtime

(define (haskell-runtime-error msg)
  (format '#t "~&Haskell runtime abort.~%~A~%" msg)
  (funcall (dynamic *runtime-abort*)))

;; Some common error handlers

(define (signal-unknown-file-type filename)
  (fatal-error 'unknown-file-type
    "The filename ~a has an unknown file type."
    filename))

(define (signal-file-not-found filename)
  (fatal-error 'file-not-found
    "The file ~a doesn't exist."
    filename))
                                                       
