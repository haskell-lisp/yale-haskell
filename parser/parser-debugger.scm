;;; These routines are strictly for debugging the parser.  They could
;;; be removed from the system later.

;;; define some debugging stuff
;;;  Here's the debugging control:
;;;  Capabilities:
;;;      record start (line,token,production,k)
;;;      record end (line,token,prodection,k)
;;;      print end (line,token,prodection,k,value)
;;;      break start
;;;      break end

(define *parser-debug-options* '())
(define *parser-debug-lines* '())
(define *parser-debug-id* 0)

(define (watch-lines . lines)
  (setf *parser-debug-lines* lines))

(define (watching-this-line?)
 (and (not (eq? *parser-debug-lines* 'none))
  (or (null? *parser-debug-lines*)
      (and (>= *current-line* (car *parser-debug-lines*))
	   (or (null? (cdr *parser-debug-lines*))
	       (<= *current-line* (cadr *parser-debug-lines*)))))))

(define (ptrace-print-obj x)
  (pprint x))

(define (ptrace-breakpoint)
  (error "Breakpoint~%"))

(define (parser-show-context id tag msg)
  (format '#t "~A parse of ~A(~A)  Line: ~A  Token: ~A"
	  msg tag id *current-line* *token*)
  (when (not (null? *token-args*))
     (format '#t " ~A" *token-args*))
  (format '#t "~%"))

(define (ptrace-clear)
  (setf *parser-debug-options* '()))

(define (ptrace-pop)
  (pop *parser-debug-options*))

(define (ptrace-watch . things)
  (dolist (x things)
     (push (cons x 'watch) *parser-debug-options*)))

(define (ptrace-show . things)
  (dolist (x things)
     (push (cons x 'show) *parser-debug-options*)))

(define (ptrace-break . things)
  (dolist (x things)
     (push (cons x 'break) *parser-debug-options*)))

;;; Routines called by the trace-parser macro

(define (tracing-parse/entry tag)
  (let ((all? (assq 'all *parser-debug-options*))
	(this? (assq tag *parser-debug-options*)))
    (cond ((or all? this?)
	   (incf *parser-debug-id*)
	   (parser-show-context *parser-debug-id* tag "Entering")
	   (when (and this? (eq? (cdr this?) 'break))
		 (ptrace-breakpoint))
	   *parser-debug-id*)
	  (else 0))))

(define (tracing-parse/exit tag id res)
  (let ((all? (assq 'all *parser-debug-options*))
	(this? (assq tag *parser-debug-options*)))
    (when (and (or all? this?) (not (eq? tag 0)))
      (setf (dynamic *returned-obj*) res)
      (parser-show-context id tag "Exiting")
      (when (and this? (eq? (cdr this?) 'show))
	    (ptrace-print-obj res))
      (when (and this? (eq? (cdr this?) 'break))
	    (ptrace-breakpoint)))))

