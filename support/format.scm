;;; format.scm -- format function for Scheme
;;;
;;; author :  Sandra Loosemore
;;; date   :  29 Oct 1991
;;;
;;;
;;; This code is adapted from the XP pretty printer originally written
;;; in Common Lisp by Dick Waters.  Here is the copyright notice attached
;;; to the original XP source file:
;;;
;;;------------------------------------------------------------------------
;;;
;;; Copyright 1989,1990 by the Massachusetts Institute of Technology,
;;; Cambridge, Massachusetts.
;;; 
;;; Permission to use, copy, modify, and distribute this software and its
;;; documentation for any purpose and without fee is hereby granted,
;;; provided that this copyright and permission notice appear in all
;;; copies and supporting documentation, and that the name of M.I.T. not
;;; be used in advertising or publicity pertaining to distribution of the
;;; software without specific, written prior permission. M.I.T. makes no
;;; representations about the suitability of this software for any
;;; purpose.  It is provided "as is" without express or implied warranty.
;;; 
;;;  M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
;;;  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
;;;  M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
;;;  ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
;;;  WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
;;;  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
;;;  SOFTWARE.
;;;
;;;------------------------------------------------------------------------
;;;


;;; The stream argument can be #f, in which case a string is returned.
;;; If the stream is #t, (current-output-port) is used.
;;; We compile a string argument into a function and call the function.
;;; The only exception is if the string doesn't contain any ~ escapes;
;;; then we can treat it as a literal and just write it to the stream.

(define (format stream string-or-fn . args)
  (cond ((not stream)
	 (call-with-output-string
	     (lambda (stream)
	       (apply (function format) stream string-or-fn args))))
	(else
	 (if (eq? stream '#t)
	     (setf stream (current-output-port)))
	 (when (string? string-or-fn)
	   (setf string-or-fn (xp.process-format-string string-or-fn)))
	 (if (string? string-or-fn)
	     (write-string string-or-fn stream)
	     (xp.maybe-initiate-xp-printing string-or-fn stream args))
	 '#f)))

(define xp.format-string-cache (make-table))

(define (xp.process-format-string string-or-fn)
  (cond ((not (string? string-or-fn)) string-or-fn)
	((not xp.format-string-cache)
	 (xp.maybe-compile-format-string string-or-fn))
	(else
	 (when (not (table? xp.format-string-cache))
	   (setf xp.format-string-cache (make-table)))
	 (let ((value
		   (table-entry xp.format-string-cache string-or-fn)))
	   (when (not value)
	     (setf value (xp.maybe-compile-format-string string-or-fn))
	     (setf (table-entry xp.format-string-cache string-or-fn)
		   value))
	   value))))


(define (xp.maybe-compile-format-string string)
  (let ((length  (string-length string)))
    (or (xp.simple-format-string? string 0 length)
	(let ((fn  (xp.parse-format-string string 0 length)))
	  (lambda (xp args)
	    (funcall fn xp args args))))))


;;; Try to detect format strings without fancy directives, that can be 
;;; written with a call to  write-string.
;;; Can do simple transformations e.g. ~% => newline, ~~ => ~, etc.

(define (xp.simple-format-string? s start end)
  (let ((twiddle  (string-position #\~ s start end)))
    (if (not twiddle)
	(if (eqv? start 0)
	    s
	    (substring s start end))
	(let ((char    (string-ref s (1+ twiddle))))
	  (cond ((eqv? char #\%)
		 (let ((tail (xp.simple-format-string? s (+ twiddle 2) end)))
		   (if tail
		       (string-append (substring s start twiddle)
				      (string #\newline)
				      tail)
		       '#f)))
		((eqv? char #\~)
		 (let ((tail (xp.simple-format-string? s (+ twiddle 2) end)))
		   (if tail
		       (string-append (substring s start (1+ twiddle))
				      tail)
		       '#f)))
		((eqv? char #\newline)
		 (let ((tail (xp.simple-format-string?
			         s
				 (xp.skip-whitespace s (+ twiddle 2) end)
				 end)))
		   (if tail
		       (string-append (substring s start twiddle)
				      tail)
		       '#f)))
		(else
		 '#f))))))

(define (warning string-or-fn . args)
  (internal-warning (apply (function format) '#f string-or-fn args)))

(define (error string-or-fn . args)
  (internal-error (apply (function format) '#f string-or-fn args)))


;;;=====================================================================
;;; Compiled format
;;;=====================================================================

;;; Note that compiled format strings always print through xp streams even if
;;; they don't have any xp directives in them.  As a result, the compiled code
;;; can depend on the fact that the stream being operated on is an xp
;;; stream not an ordinary one.


;;; Parse a format string, returning a function to do the printing.
;;; The function is called with three arguments
;;;    * the xp stream
;;;    * the original argument list
;;;    * the argument list tail
;;; It should return the list of leftover, unprocessed arguments.

(define (xp.parse-format-string string start end)
  (cond ((eqv? start end)
	 (function xp.format-finish))
	((eqv? (string-ref string start) #\~)
	 (xp.parse-format-string-dispatch string start end))
	(else
	 (let* ((next       (or (string-position #\~ string start end) end))
		(literal    (substring string start next))
		(count      (- next start))
		(continue   (xp.parse-format-string string next end))
		(newline?   (string-position #\newline literal 0 count)))
	   (if newline?
	       (lambda (xp args tail)
		 (xp.write-string+ literal xp 0 count)
		 (funcall continue xp args tail))
	       (lambda (xp args tail)
		 (xp.write-string++ literal xp 0 count)
		 (funcall continue xp args tail)))))
	))

(define (xp.format-finish xp args tail)
  (declare (ignore xp args))
  tail)


;;; Functions for handling individual format specifiers are installed
;;; in this table.  They are called with these arguments:
;;; * the format string
;;; * the index of the next character
;;; * the index of the end of the format string
;;; * the list of parameters for the format specification
;;; * a boolean indicating whether the colon modifier was present
;;; * a boolean indicating whether the atsign modifier was present
;;; The handler is responsible for calling xp.parse-format-string to parse 
;;; the rest of the format string, and returning a function.  (This has
;;; to be done by the individual handlers because some of them need to
;;; scan the format string for matching delimiters, etc.)

;;; *** This probably isn't right, we assume characters can be compared
;;; *** with EQ? and used as table keys.

(define xp.fn-table (make-table))

(define (define-format char function)
  (setf (table-entry xp.fn-table (char-upcase char)) function)
  (setf (table-entry xp.fn-table (char-downcase char)) function))

;;; Parse a ~ sequence from the format string and dispatch to the
;;; appropriate handler.  

(define (xp.parse-format-string-dispatch string start end)
  (multiple-value-bind (next params colon? atsign? char)
      (xp.parse-format-descriptor string start end)
    (let ((fn  (table-entry xp.fn-table char)))
      (if fn
	  (funcall fn string next end params colon? atsign?)
	  (error "Unrecognized format escape ~~~a." char)))))

(define (xp.parse-format-descriptor string start end)
  (multiple-value-bind (params start)
      (xp.parse-format-parameters string start end)
    (let ((colon?    '#f)
	  (atsign?   '#f)
	  (char      '#f))
      (block parse-format-descriptor
	(do ()
	    ((xp.check-for-incomplete-format-string string start end))
	    (setf char (string-ref string start))
	    (incf start)
	    (cond ((eqv? char #\:)
		   (setf colon? '#t))
		  ((eqv? char #\@)
		   (setf atsign? '#t))
		  (else
		   (return-from parse-format-descriptor
		     (values start params colon? atsign? char)))
		))))))


;;; *** The stuff for V and # format parameters is disabled because
;;; *** it makes the handler functions hairier.  It's rarely used anyway,
;;; *** and you can get the same effect by consing up a format string
;;; *** on the fly if you really need to.

(define (xp.parse-format-parameters string start end)
  (let ((params  '())
	(char    '#f))
    (incf start)  ; skip ~
    (block parse-format-parameters
      (do ()
	  ((xp.check-for-incomplete-format-string string start end))
	  (setf char (string-ref string start))
	  (cond ((char-numeric? char)
		 (multiple-value-bind (next value)
		     (xp.parse-format-number string start end 0)
		   (setf start next)
		   (push value params)))
		((eqv? char #\')
		 (push (string-ref string (1+ start)) params)
		 (setf start (+ start 2)))
		((or (eqv? char #\v) (eqv? char #\V))
		 (error "V format parameter not supported.")  ;***
		 (push 'value params)
		 (setf start (+ start 1)))
		((eqv? char #\#)
		 (error "# format parameter not supported.")  ;***
		 (push 'count params)
		 (setf start (+ start 1)))
		((eqv? char #\,)
		 (push '#f params))
		(else
		 (return-from parse-format-parameters
		   (values (nreverse params) start))))
	  (if (eqv? (string-ref string start) #\,)
	      (incf start))))))

(define (xp.parse-format-number string start end value)
  (xp.check-for-incomplete-format-string string start end)
  (let* ((char    (string-ref string start))
	 (weight  (string-position char "0123456789" 0 10)))
    (if weight
	(xp.parse-format-number string (1+ start) end (+ (* value 10) weight))
	(values start value))))

(define (xp.check-for-incomplete-format-string string start end)
  (if (eqv? start end)
      (error "Incomplete format string ~s." string)
      '#f))


;;; *** All of these format handlers probably ought to do more checking
;;; *** for the right number of parameters and not having colon? and
;;; *** atsign? supplied when they are not allowed.

;;; ~A and ~S are the basic format directives.

(define (xp.format-a string start end params colon? atsign?)
  (xp.format-a-s-helper string start end params colon? atsign? '#f))
(define-format #\a (function xp.format-a))

(define (xp.format-s string start end params colon? atsign?)
  (xp.format-a-s-helper string start end params colon? atsign? '#t))
(define-format #\s (function xp.format-s))

(define (xp.format-a-s-helper string start end params colon? atsign? escape?)
  (declare (ignore colon? atsign?))  ;***
  (let ((continuation  (xp.parse-format-string string start end)))
    (if (null? params)
	;; Do the simple, common case.
	(lambda (xp args tail)
	  (dynamic-let ((*print-escape*   escape?))
	    (xp.write+ (car tail) xp))
	  (funcall continuation xp args (cdr tail)))
	;; Do the hard case.
	(let* ((mincol   (or (and (not (null? params)) (pop params)) 0))
	       (colinc   (or (and (not (null? params)) (pop params)) 1))
	       (minpad   (or (and (not (null? params)) (pop params)) 0))
	       (padchar  (or (and (not (null? params)) (pop params)) #\space)))
	  (declare (ignore mincol colinc minpad padchar))  ;***
;;; *** I'm confused.  It seems like we have to print this to a string
;;; *** and then write the string to the XP stream along with the padding
;;; *** But won't switching to a new stream mess up circularity detection, 
;;; *** indentation, etc?
	  (error "Unimplemented format option ~s!" string))
      )))


;;; ~W -> write

(define (xp.format-w string start end params colon? atsign?)
  (declare (ignore params))
  (let ((continuation  (xp.parse-format-string string start end)))
    (cond ((and (not colon?) (not atsign?))
	   (lambda (xp args tail)
	     (xp.write+ (car tail) xp)
	     (funcall continuation xp args (cdr tail))))
	  ((and colon? (not atsign?))
	   (lambda (xp args tail)
	     (dynamic-let ((*print-pretty*  '#t))
	       (xp.write+ (car tail) xp))
	     (funcall continuation xp args (cdr tail))))
	  ((and (not colon?) atsign?)
	   (lambda (xp args tail)
	     (dynamic-let ((*print-level*  '#f)
			   (*print-length* '#f))
	       (xp.write+ (car tail) xp))
	     (funcall continuation xp args (cdr tail))))
	  ((and colon? atsign?)
	   (lambda (xp args tail)
	     (dynamic-let ((*print-level*  '#f)
			   (*print-length* '#f)
			   (*print-pretty* '#t))
	       (xp.write+ (car tail) xp))
	     (funcall continuation xp args (cdr tail))))
	  )))
(define-format #\w (function xp.format-w))


;;; Here are the directives for printing integers, ~D and friends.

(define (xp.format-d string start end params colon? atsign?)
  (xp.format-d-b-o-x-helper string start end params colon? atsign? 10))
(define-format #\d (function xp.format-d))

(define (xp.format-b string start end params colon? atsign?)
  (xp.format-d-b-o-x-helper string start end params colon? atsign? 2))
(define-format #\b (function xp.format-b))

(define (xp.format-o string start end params colon? atsign?)
  (xp.format-d-b-o-x-helper string start end params colon? atsign? 8))
(define-format #\o (function xp.format-o))

(define (xp.format-x string start end params colon? atsign?)
  (xp.format-d-b-o-x-helper string start end params colon? atsign? 16))
(define-format #\x (function xp.format-x))

(define (xp.format-d-b-o-x-helper string start end params colon? atsign? radix)
  (let ((continuation  (xp.parse-format-string string start end)))
    (if (and (null? params) (not colon?) (not atsign?))
	;; Do the simple, common case.
	(lambda (xp args tail)
	  (dynamic-let ((*print-escape*  '#f)
			(*print-radix*   '#f)
			(*print-base*    radix))
	    (xp.write+ (car tail) xp))
	  (funcall continuation xp args (cdr tail)))
	;; Do the hard case.
	(let* ((mincol    (or (and (not (null? params)) (pop params)) 0))
	       (padchar   (or (and (not (null? params)) (pop params)) #\space))
	       (commachar (or (and (not (null? params)) (pop params)) #\,))
	       (commaint  (or (and (not (null? params)) (pop params)) 3)))
	  (declare (ignore mincol padchar commachar commaint))  ;***
	  ;; *** I'm too lazy to do this right now.
	  (error "Unimplemented format option ~s!" string)))))


(define (xp.format-r string start end params colon? atsign?)
  (if (not (null? params))
      (xp.format-d-b-o-x-helper string start end (cdr params)
			     colon? atsign? (car params))
      ;; *** The colon? and atsign? modifiers do weird things like
      ;; *** printing roman numerals.  I'm too lazy to do this until/unless
      ;; *** we have a real need for it.
      (error "Unimplemented format option ~s!" string)))
(define-format #\r (function xp.format-r))


;;; ~P -> plurals

(define (xp.format-p string start end params colon? atsign?)
  (declare (ignore params))
  (let ((continuation  (xp.parse-format-string string start end)))
    (cond ((and (not colon?) (not atsign?))
	   (lambda (xp args tail)
	     (if (not (eqv? (car tail) 1))
		 (xp.write-char++ #\s xp))
	     (funcall continuation xp args (cdr tail))))
	  ((and colon? (not atsign?))
	   (lambda (xp args tail)
	     (setf tail (xp.back-up 1 args tail))
	     (if (not (eqv? (car tail) 1))
		 (xp.write-char++ #\s xp))
	     (funcall continuation xp args (cdr tail))))
	  ((and (not colon?) atsign?)
	   (lambda (xp args tail)
	     (if (eqv? (car tail) 1)
		 (xp.write-char++ #\y xp)
		 (begin
		   (xp.write-char++ #\i xp)
		   (xp.write-char++ #\e xp)
		   (xp.write-char++ #\s xp)))
	     (funcall continuation xp args (cdr tail))))
	  ((and colon? atsign?)
	   (lambda (xp args tail)
	     (setf tail (xp.back-up 1 args tail))
	     (if (eqv? (car tail) 1)
		 (xp.write-char++ #\y xp)
		 (begin
		   (xp.write-char++ #\i xp)
		   (xp.write-char++ #\e xp)
		   (xp.write-char++ #\s xp)))
	     (funcall continuation xp args (cdr tail)))))))
(define-format #\p (function xp.format-p))


;;; ~C -> character

(define (xp.format-c string start end params colon? atsign?)
  (declare (ignore params))
  (let ((continuation  (xp.parse-format-string string start end)))
    (cond ((and (not colon?) (not atsign?))
	   (lambda (xp args tail)
	     (xp.write-char++ (car tail) xp)
	     (funcall continuation xp args (cdr tail))))
	  ((and (not colon?) atsign?)
	   (lambda (xp args tail)
	     (dynamic-let ((*print-escape*  '#t))
	       (xp.write+ (car tail) xp)
	       (funcall continuation xp args (cdr tail)))))
	  (else
	   ;; *** I don't know how to get at the character names.
	   (error "Unimplemented format option ~s!" string)))))
(define-format #\c (function xp.format-c))



;;; Newline directives, ~% and ~&

(define (xp.format-percent string start end params colon? atsign?)
  (xp.format-newline-helper string start end params colon? atsign?
			 'unconditional))
(define-format #\% (function xp.format-percent))

(define (xp.format-ampersand string start end params colon? atsign?)
  (xp.format-newline-helper string start end params colon? atsign?
			 'fresh))
(define-format #\& (function xp.format-ampersand))

(define (xp.format-newline-helper string start end params colon? atsign? kind)
  (declare (ignore colon? atsign?))
  (let ((continuation (xp.parse-format-string string start end))
	(n            (or (and (not (null? params)) (pop params)) 1)))
    (if (eqv? n 1)
	(lambda (xp args tail)
	  (xp.pprint-newline+ kind xp)
	  (funcall continuation xp args tail))
	(lambda (xp args tail)
	  (xp.pprint-newline+ kind xp)
	  (dotimes (i (1- n))
	    (xp.pprint-newline+ 'unconditional xp))
	  (funcall continuation xp args tail))
      )))


;;; ~_, Conditional newline

(define (xp.format-underbar string start end params colon? atsign?)
  (declare (ignore params))
  (let ((continuation  (xp.parse-format-string string start end))
	(kind          (if colon?
			   (if atsign? 'mandatory 'fill)
			   (if atsign? 'miser 'linear))))
    (lambda (xp args tail)
      (xp.pprint-newline+ kind xp)
      (funcall continuation xp args tail))))
(define-format #\_ (function xp.format-underbar))


;;; Random character printing directives, ~| and ~~

;;; *** commented out because #\page is not standard scheme
; (define (xp.format-bar string start end params colon? atsign?)
;  (xp.format-char-helper string start end params colon? atsign? #\page))
; (define-format #\| (function xp.format-bar))

(define (xp.format-twiddle string start end params colon? atsign?)
  (xp.format-char-helper string start end params colon? atsign? #\~))
(define-format #\~ (function xp.format-twiddle))

(define (xp.format-char-helper string start end params colon? atsign? char)
  (declare (ignore colon? atsign?))
  (let ((continuation  (xp.parse-format-string string start end))
	(n             (or (and (not (null? params)) (pop params)) 1)))
    (if (eqv? n 1)
	(lambda (xp args tail)
	  (xp.write-char++ char xp)
	  (funcall continuation xp args tail))
	(lambda (xp args tail)
	  (dotimes (i n)
	    (xp.write-char++ char xp))
	  (funcall continuation xp args tail)))))



;;; ~<newline> directive (ignore whitespace in format string)

(define (xp.format-newline string start end params colon? atsign?)
  (declare (ignore params))
  (let ((newline?   '#f)
	(skip?      '#f))
    (cond ((and (not colon?) (not atsign?))  ; skip both newline and whitespace
	   (setf skip? '#t))
	  ((and colon? (not atsign?)))  ; skip newline, leave whitespace
	  ((and (not colon?) atsign?)   ; do newline, skip whitespace
	   (setf newline? '#t)
	   (setf skip? '#t))
	  (else
	   (error "~:@<newline> not allowed.")))
    (if skip?
	(setf start (xp.skip-whitespace string start end)))
    (let ((continuation  (xp.parse-format-string string start end)))
      (if newline?
	  (lambda (xp args tail)
	    (xp.pprint-newline+ 'unconditional xp)
	    (funcall continuation xp args tail))
	  continuation))))
(define-format #\newline (function xp.format-newline))

(define (xp.skip-whitespace string start end)
  (if (eqv? start end)
      start
      (let ((char  (string-ref string start)))
	(if (and (char-whitespace? char)
		 (not (eqv? char #\newline)))
	    (xp.skip-whitespace string (1+ start) end)
	    start))))



;;; ~T -> tab

(define (xp.format-t string start end params colon? atsign?)
  (let* ((continuation  (xp.parse-format-string string start end))
	 (colnum        (or (and (not (null? params)) (pop params)) 1))
	 (colinc        (or (and (not (null? params)) (pop params)) 1))
	 (kind          (if colon?
			    (if atsign? 'section-relative 'section)
			    (if atsign? 'line-relative 'line))))
    (lambda (xp args tail)
      (xp.pprint-tab+ kind colnum colinc xp)
      (funcall continuation xp args tail))))
(define-format #\t (function xp.format-t))


;;; ~I -> indent

(define (xp.format-i string start end params colon? atsign?)
  (declare (ignore atsign?))
  (let ((continuation  (xp.parse-format-string string start end))
	(kind          (if colon? 'current 'block))
	(n             (or (and (not (null? params)) (pop params)) 0)))
    (lambda (xp args tail)
      (pprint-indent kind n)
      (funcall continuation xp args tail))))
(define-format #\i (function xp.format-i))


;;; ~* -> skip or back up over arguments

(define (xp.format-star string start end params colon? atsign?)
  (let ((continuation  (xp.parse-format-string string start end))
	(n             (or (and (not (null? params)) (pop params)) 1)))
    (cond ((and (not colon?) (not atsign?))
	   (lambda (xp args tail)
	     (funcall continuation xp args (list-tail tail n))))
	  ((and colon? (not atsign?))
	   (lambda (xp args tail)
	     (funcall continuation xp args (xp.back-up n args tail))))
	  ((and (not colon?) atsign?)
	   (lambda (xp args tail)
	     (declare (ignore tail))
	     (funcall continuation xp args (list-tail args n))))
	  (else
	   (error "~:@* not allowed.")))))
(define-format #\* (function xp.format-star))

(define (xp.back-up n head tail)
  (if (eq? (list-tail head n) tail)
      head
      (xp.back-up n (cdr head) tail)))


;;; ~? -> indirection
;;; Normally uses two arguments, a string and a list.
;;; With @, only uses a string, takes arguments from the tail.

(define (xp.format-question string start end params colon? atsign?)
  (declare (ignore params colon?))
  (let ((continuation  (xp.parse-format-string string start end)))
    (if atsign?
	(lambda (xp args tail)
	  (setf tail (apply (function format) xp (car tail) (cdr tail)))
	  (funcall continuation xp args tail))
	(lambda (xp args tail)
	  (apply (function format) xp (car tail) (cadr tail))
	  (funcall continuation xp args (cddr tail))))))
(define-format #\? (function xp.format-question))


;;; ~(...~) -> case conversion.

(define *xp.format-paren-next* '#f)

(define (xp.format-paren string start end params colon? atsign?)
  (declare (ignore params))
  (let* ((handler      (dynamic-let ((*xp.format-paren-next* '#t))
			 (let ((result (xp.parse-format-string
					   string start end)))
			   (if (eq? (dynamic *xp.format-paren-next*) '#t)
			       (error "~( directive has no matching ~)."))
			   (setf start (dynamic *xp.format-paren-next*))
			   result)))
	 (continuation (xp.parse-format-string string start end))
	 (mode         (if colon?
			   (if atsign? 'up 'cap1)
			   (if atsign? 'cap0 'down))))
    (lambda (xp args tail)
      (xp.push-char-mode xp mode)
      (setf tail (funcall handler xp args tail))
      (xp.pop-char-mode xp)
      (funcall continuation xp args tail))))
(define-format #\( (function xp.format-paren))

(define (xp.format-paren-end string start end params colon? atsign?)
  (declare (ignore string end params colon? atsign?))
  (if (not (dynamic *xp.format-paren-next*))
      (error "~) directive has no matching ~(."))
  (setf (dynamic *xp.format-paren-next*) start)
  (function xp.format-finish))
(define-format #\) (function xp.format-paren-end))

;;; ~F      -> fixed-width      *** unimplemented
;;; ~E      -> e-notation       *** unimplemented
;;; ~G      -> general float    *** unimplemented
;;; ~$      -> dollars float    *** unimplemented
;;; ~[...~] -> conditional      *** unimplemented
;;; ~{...~} -> iteration        *** unimplemented
;;; ~<...~> -> justification    *** unimplemented
;;; ~;      -> clause seperator *** unimplemented
;;; ~^      -> up and out       *** unimplemented
;;; ~/.../  -> hook             *** unimplemented

(define (xp.unimplemented-format string start end params colon? atsign?)
  (declare (ignore start end params colon? atsign?))
  (error "Unimplemented format directive in ~s." string))

(define-format #\f (function xp.unimplemented-format))
(define-format #\e (function xp.unimplemented-format))
(define-format #\g (function xp.unimplemented-format))
(define-format #\$ (function xp.unimplemented-format))
(define-format #\[ (function xp.unimplemented-format))
(define-format #\] (function xp.unimplemented-format))
(define-format #\{ (function xp.unimplemented-format))
(define-format #\} (function xp.unimplemented-format))
(define-format #\< (function xp.unimplemented-format))
(define-format #\> (function xp.unimplemented-format))
(define-format #\; (function xp.unimplemented-format))
(define-format #\^ (function xp.unimplemented-format))
(define-format #\/ (function xp.unimplemented-format))

