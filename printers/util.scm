;;; util.scm -- utilities for printing AST structures
;;;
;;; author :  Sandra Loosemore
;;; date   :  15 Jan 1992
;;;
;;;


;;; The AST syntax printers are only used if this variable is true.

(define *print-ast-syntax* '#t)


;;; Here's a macro for defining AST printers.

(define-syntax (define-ast-printer type lambda-list . body)
  (let ((printer  (symbol-append 'write- type)))
    `(begin
       (define (,printer ,@lambda-list) ,@body)
       (define-struct-printer ,type ,printer))
    ))


;;; This variable controls how much indentation to perform on block
;;; bodies.

(define *print-ast-indent* 2)


;;; Begin a logical block with the default indentation.

(define-syntax (with-ast-block xp-stuff . body)
  (let ((xp  (car xp-stuff)))
    `(pprint-logical-block (,xp '() "" "")
       (pprint-indent 'block (dynamic *print-ast-indent*) ,xp)
       (pprint-pop)  ; prevents unused variable warning
       ,@body)))


;;; Write a space and maybe a fill line break.

(define (write-whitespace xp)
  (write-char #\space xp)
  (pprint-newline 'fill xp))


;;; Write a space and maybe a mandatory line break.

(define (write-newline xp)
  (write-char #\space xp)
  (pprint-newline 'mandatory xp))



;;; Write a list of things separated by delimiters and maybe
;;; surrounded by delimiters.

(define (write-delimited-list objects xp fn delim prefix suffix)
  (pprint-logical-block (xp '() prefix suffix)
    (do ((objects objects (cdr objects)))
	((null? objects) '#f)
	(pprint-pop)
	(funcall fn (car objects) xp)
	(when (cdr objects)
	  (write-string delim xp)
	  (write-whitespace xp)))))


;;; Here's a couple common special cases of the above.

(define (write-commaized-list objects xp)
  (write-delimited-list objects xp (function write) "," "(" ")"))

(define (write-ordinary-list objects xp)
  (write-delimited-list objects xp (function write) "" "" ""))


;;; Here's another helper function that's used to implement the layout
;;; rule.  The layout rule is only used to format output if *print-pretty*
;;; is true.
;;; *** should do pprint-indent here?

(define (write-layout-rule objects xp fn)
  (pprint-logical-block (xp '()
			    (if (dynamic *print-pretty*) "" "{")
			    (if (dynamic *print-pretty*) "" "}"))
    (do ((objects objects (cdr objects)))
	((null? objects) '#f)
	(pprint-pop)
	(funcall fn (car objects) xp)
	(when (cdr objects)
	  (if (dynamic *print-pretty*)
	      (pprint-newline 'mandatory xp)
	      (write-string "; " xp))))))


;;; This filters a list of decls, removing the recursive marker added by
;;; dependency analysis.

(define (remove-recursive-grouping decls)
  (cond ((null? decls) '())
	((is-type? 'recursive-decl-group (car decls))
	 (append (recursive-decl-group-decls (car decls))
		 (remove-recursive-grouping (cdr decls))))
	(else
	 (cons (car decls) (remove-recursive-grouping (cdr decls))))))

;;; Write where-decls, using the layout rule if appropriate.

(define (write-wheredecls decls xp)
  (when (not (null? decls))
    (write-whitespace xp)
    (write-string "where" xp)
    (write-whitespace xp)
    (write-layout-rule (remove-recursive-grouping decls) xp (function write))))


;;; Write an ordinary variable name.

(define (write-avarid name xp)
  (write-string (symbol->string name) xp))
  

;;; Constructor name symbols have a funny prefix attached; have to strip
;;; this off, so can't just print the symbol using write-avarid.

(define (write-aconid name xp)
  (let ((s  (symbol->string name)))
    (write-string (substring s 1 (string-length s)) xp)))


;;; There are a couple places where conids and varids are mixed up
;;; together.

(define (conid? name)
  (eqv? (string-ref (symbol->string name) 0) #\;))

(define (write-varop-conop name xp)
  (if (conid? name)
      (write-conop name xp)
      (write-varop name xp)))

(define (write-varid-conid name xp)
  (if (conid? name)
      (write-conid name xp)
      (write-varid name xp)))



;;; Stuff for writing a variable name as either an operator or an ordinary
;;; variable ID.  This is necessary because some kinds of symbol names
;;; default to being operators and others default to being ordinary names.
;;; Bleah....


(define (write-varop name xp)
  (if (avarid? name)
      (begin
        (write-char #\` xp)
	(write-avarid name xp)
	(write-char #\` xp))
      (write-avarid name xp)))

(define (write-varid name xp)
  (if (avarid? name)
      (write-avarid name xp)
      (begin
        (write-char #\( xp)
	(write-avarid name xp)
	(write-char #\) xp))))


;;; This tests for alphabetic rather than lower-case characters
;;; so that gensym'ed variables with uppercase names don't print funny.

(define (avarid? name)
  (let ((ch  (string-ref (symbol->string name) 0)))
    (char-alphabetic? ch)))


;;; Similar stuff for doing constructor names.  Moby bleah....

(define (write-conop name xp)
  (if (aconid? name)
      (begin
        (write-char #\` xp)
	(write-aconid name xp)
	(write-char #\` xp))
      (write-aconid name xp)))

(define (write-conid name xp)
  (if (aconid? name)
      (write-aconid name xp)
      (begin
        (write-char #\( xp)
	(write-aconid name xp)
	(write-char #\) xp))))

(define (aconid? name)
  (let ((ch  (string-ref (symbol->string name) 1)))
    (char-upper-case? ch)))


;;; These are officially aconid in the syntax, but they aren't
;;; prefixed so write them using write-avarid instead.  Barf.

(define (write-modid name xp)
  (write-avarid name xp))

(define (write-tyconid name xp)
  (write-avarid name xp))

(define (write-tyclsid name xp)
  (write-avarid name xp))
