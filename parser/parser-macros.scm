;;; Macro definitions for the parser & lexer.


;;; This macro allows debugging of the lexer.  Before releasing, this can
;;; be replaced by (begin ,@body) for faster code.

(define-syntax (trace-parser tag . body)
;  `(begin 
;     (let* ((k (tracing-parse/entry ',tag))
;	    (res (begin ,@body)))
;       (tracing-parse/exit ',tag k res)
;       res))
  (declare (ignore tag))
  `(begin ,@body)
  )

;;; Macros used by the lexer.

;;; The lexer used a macro, char-case, to dispatch on the syntactic catagory of
;;; a character.  These catagories (processed at compile time) are defined
;;; here.  Note that some of these definitions use the char-code
;;; directly and would need updating for different character sets.

(define *lex-definitions*
  '((vtab 11)  ; define by ascii code to avoid relying of the reader
    (formfeed 12) 
    (whitechar #\newline #\space #\tab formfeed vtab)
    (small #\a - #\z)
    (large #\A - #\Z)
    (digit #\0 - #\9)
    (symbol #\! #\# #\$ #\% #\& #\* #\+ #\. #\/ #\< #\= #\> #\? #\@
      #\\ #\^ #\|)
    (presymbol #\- #\~)
    (exponent #\e #\E)
    (graphic large small digit
             #\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\+
             #\, #\- #\. #\/ #\: #\; #\< #\= #\> #\? #\@
	     #\[ #\\ #\] #\^ #\_ #\` #\{ #\| #\} #\~)
    (charesc #\a #\b #\f #\n #\r #\t #\v #\\ #\" #\' #\&)
    (cntrl large #\@ #\[ #\\ #\] #\^ #\_)))

;;; The char-case macro is similar to case using characters to select.
;;; The following capabilities are added by char-case:
;;;   pre-defined constants are denoted by symbols (defined above)
;;;   ranges of characters are represented using -.  For example,
;;;     (#\a - #\z #\A - #\Z) denotes all alphabetics.
;;;   numbers refer to the char code of a character.
;;; The generated code is optimized somewhat to take advantage of
;;; consecutive character ranges.  With a little work, this could be
;;; implemented using jump tables someday.

(define-syntax (char-case exp . alts)
  (expand-char-case exp alts))

(define (expand-char-case exp alts)
  (let ((temp (gensym)))
    `(let ((,temp ,exp))
       ,(expand-char-case1 temp alts))))

(define (expand-char-case1 temp alts)
  (if (null? alts)
      '()
      (let* ((alt (car alts))
	     (test (car alt))
	     (body (cons 'begin (cdr alt)))
	     (rest (expand-char-case1 temp (cdr alts))))
	(cond ((eq? test 'else)
	       body)
	      (else
	       `(if (or ,@(gen-char-tests temp
			     (if (pair? test) test (list test))))
		    ,body
		    ,rest))))))

(define (gen-char-tests temp tests)
  (gen-char-tests-1 temp
	(sort-list (gather-char-tests tests) (function char<?))))

(define (gen-char-tests-1 temp chars)
  (cond ((null? chars)
	 '())
	((long-enough-run? chars 3)
	 (gen-range-check temp (car chars) (car chars) (cdr chars)))
	(else
	 `((char=? ,temp ',(car chars))
	   ,@(gen-char-tests-1 temp (cdr chars))))))

(define (gen-range-check temp first current chars)
  (if (and (pair? chars) (consec-chars? current (car chars)))
      (gen-range-check temp first (car chars) (cdr chars))
      `((and (char>=? ,temp ',first)
	     (char<=? ,temp ',current))
	,@(gen-char-tests-1 temp chars))))

(define (consec-chars? c1 c2)
  (eqv? (+ 1 (char->integer c1)) (char->integer c2)))

(define (long-enough-run? l n)
  (or (eqv? n 1)
      (and (pair? (cdr l))
	   (consec-chars? (car l) (cadr l))
	   (long-enough-run? (cdr l) (1- n)))))

(define (gather-char-tests tests)
  (cond ((null? tests)
	 '())
	((symbol? (car tests))
	 (let ((new-test (assq (car tests) *lex-definitions*)))
	   (if new-test
	       (gather-char-tests (append (cdr new-test) (cdr tests)))
	       (error "Unknown character class: ~A~%" (car tests)))))
	((integer? (car tests))
	 (cons (integer->char (car tests))
	       (gather-char-tests (cdr tests))))
	((and (pair? (cdr tests)) (eq? '- (cadr tests)))
	 (letrec ((fn (lambda (a z)
			(if (char>? a z)
			    (gather-char-tests (cdddr tests))
			    (cons a (funcall
				      fn (integer->char
					 (+ 1 (char->integer a))) z))))))
	   (funcall fn (car tests) (caddr tests))))
	((char? (car tests))
	 (cons (car tests) (gather-char-tests (cdr tests))))
	(else
	 (error "Invalid selector in char-case: ~A~%" (car tests)))))

;;; This macro scans a list of characters on a given syntaxtic catagory.
;;; The current character is always included in the resulting list.

(define-syntax (scan-list-of char-type)
 `(letrec ((test-next (lambda ()
		       (char-case *char*
			(,char-type
			 (let ((c *char*))
			   (advance-char)
			   (cons c (funcall test-next))))
			(else '())))))
    (let ((c *char*))
      (advance-char)
      (cons c (funcall test-next)))))

;;; This macro tests for string equality in which the strings are
;;; represented by lists of characters.  The comparisons are expanded
;;; inline (really just a little partial evaluation going on here!) for
;;; fast execution.  The tok argument evaluate to a list of chars.  The string
;;; argument must be a string constant, which is converted to characters
;;; as the macro expands.

(define-syntax (string=/list? tok string)
  (let ((temp (gensym)))
    `(let ((,temp ,tok))
       ,(expand-string=/list? temp (string->list string)))))

(define (expand-string=/list? var chars)
  (if (null? chars)
      `(null? ,var)
      (let ((new-temp (gensym)))
	`(and (pair? ,var)
	      (char=? (car ,var) ',(car chars))
	      (let ((,new-temp (cdr ,var)))
		,(expand-string=/list? new-temp (cdr chars)))))))

;;; This macro extends the string equality defined above to search a
;;; list of reserved words quickly for keywords.  It does this by a case
;;; dispatch on the first character of the string and then processing
;;; the remaining characters wirh string=/list.  This would go a little
;;; faster with recursive char-case statements, but I'm a little too
;;; lazy at for this at the moment.  If a keyword is found is emitted
;;; as a symbol.  If not, the token string is emitted with the token
;;; type indicated.  Assume the string being scanned is a list of
;;; chars assigned to a var.  (Yeah - I know - I should add a gensym
;;; var for this argument!!).

(define-syntax (parse-reserved var token-type . reserved-words)
 (let ((sorted-rws (sort-list reserved-words (function string<?))))
  `(let ((thunk (lambda () (emit-token/string ',token-type ,var))))
    (char-case (car ,var)
     ,@(expand-parse-reserved var
        (group-by-first-char (list (car sorted-rws)) (cdr sorted-rws)))
      (else (funcall thunk))))))

(define (group-by-first-char group rest)
  (cond ((null? rest)
	 (list group))
	((char=? (string-ref (car group) 0)
		 (string-ref (car rest) 0))
	 (group-by-first-char (append group (list (car rest))) (cdr rest)))
	(else
	 (cons group (group-by-first-char (list (car rest)) (cdr rest))))))

(define (expand-parse-reserved var groups)
  (if (null? groups)
      '()
      `((,(string-ref (caar groups) 0)
	 (cond ,@(expand-parse-reserved/group var (car groups))
	       (else (funcall thunk))))
	,@(expand-parse-reserved var (cdr groups)))))

(define (expand-parse-reserved/group var group)
  (if (null? group)
      '()
      `(((string=/list? (cdr ,var)
	     ,(substring (car group) 1 (string-length (car group))))
	 (emit-token ',(string->symbol (car group))))
	,@(expand-parse-reserved/group var (cdr group)))))


;;; The following macros are used by the parser.

;;; The primary macro used by the parser is token-case, which dispatches
;;; on the type of the current token (this is always *token* - unlike the
;;; lexer, no lookahead is provided; however, some of these dispatches are
;;; procedures that do a limited lookahead.  The problem with lookahead is that
;;; the layout rule adds tokens which are not visible looking into the
;;; token stream directly.

;;; Unlike char-case, the token is normally advanced unless the selector
;;; includes `no-advance'.  The final else also avoids advancing the token.

;;; In addition to raw token types, more complex types can be used.  These
;;; are defined here.  The construct `satisfies fn' calls the indicated
;;; function to determine whether the current token matches.

;;; If the token type to be matched is not a constant, the construct
;;; `unquote var' matches the current token against the type in the var.

(define *predefined-syntactic-catagories* '(
  (+ satisfies at-varsym/+?)
  (- satisfies at-varsym/-?)
  (tycon no-advance conid)
  (tyvar no-advance varid)
  (var no-advance varid satisfies at-varsym/paren?)
  (con no-advance conid satisfies at-consym/paren?)
  (name no-advance var con)
  (consym/paren no-advance satisfies at-consym/paren?)
  (varsym? no-advance varsym)
  (consym? no-advance consym)
  (varid? no-advance varid)
  (conid? no-advance conid)
  (op no-advance varsym consym \`)
  (varop no-advance varsym satisfies at-varid/quoted?)
  (conop no-advance consym satisfies at-conid/quoted?)
  (modid no-advance conid)
  (literal no-advance integer float char string)
  (numeric no-advance integer float)
  (k no-advance integer)
  (+k no-advance satisfies at-+k?)
  (-n no-advance satisfies at--n?)
  (apat-start no-advance varid conid literal _ \( \[ \~)
  (pat-start no-advance - apat-start)
  (atype-start no-advance tycon tyvar \( \[)
  (aexp-start no-advance varid conid \( \[ literal)
  ))

;;; The format of token-case is
;;;  (token-case
;;;    (sel1 . e1) (sel2 . e2) ... [(else . en)])
;;; If the sel is a symbol it is the same as a singleton list: (@ x) = ((@) x)

;;; Warning: this generates rather poor code!  Should be fixed up someday.

(define-syntax (token-case . alts)
  `(cond ,@(map (function gen-token-case-alt) alts)))

(define (gen-token-case-alt alt)
  (let ((test (car alt))
	(code (cdr alt)))
    (cond ((eq? test 'else)
	   `(else ,@code))
	  ((symbol? test)
	   (gen-token-case-alt-1 (expand-catagories (list test)) code))
	  (else
	   (gen-token-case-alt-1 (expand-catagories test) code)))))

(define (expand-catagories terms)
  (if (null? terms)
      terms
      (let ((a (assq (car terms) *predefined-syntactic-catagories*))
	    (r (expand-catagories (cdr terms))))
	(if (null? a)
	    (cons (car terms) r)
	    (expand-catagories (append (cdr a) r))))))

(define (gen-token-case-alt-1 test code)
  `((or ,@(gen-token-test test))
    ,@(if (memq 'no-advance test) '() '((advance-token)))
    ,@code))

(define (gen-token-test test)
  (cond ((null? test)
	 '())
	((eq? (car test) 'no-advance)
	 (gen-token-test (cdr test)))
	((eq? (car test) 'unquote)
	 (cons `(eq? *token* ,(cadr test)) (gen-token-test (cddr test))))
	((eq? (car test) 'satisfies)
	 (cons (list (cadr test)) (gen-token-test (cddr test))))
	(else
	 (cons `(eq? *token* ',(car test)) (gen-token-test (cdr test))))))

;;; require-tok requires a specific token to be at the scanner.  If it
;;; is found, the token is advanced over.  Otherwise, the error
;;; routine is called.

(define-syntax (require-token tok error-handler)
  `(token-case
    (,tok '())
    (else ,error-handler)))

;;; The save-parser-context macro captures the current line & file and
;;; attaches it to the ast node generated.

(define-syntax (save-parser-context . body)
  (let ((temp1 (gensym))
	(temp2 (gensym)))
    `(let ((,temp1 (capture-current-line))
	   (,temp2 (begin ,@body)))
       (setf (ast-node-line-number ,temp2) ,temp1)
       ,temp2)))

(define (capture-current-line)
  (make source-pointer (line *current-line*) (file *current-file*)))

(define-syntax (push-decl-list decl place)
  `(setf ,place (nconc ,place (list ,decl))))

