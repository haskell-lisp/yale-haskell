;;; File: parser/lexer    Author: John

;;; token data structure: a list with the token type in the
;;; car and other information in the rest of the list.  Symbols
;;; designate the token type.

;;; Reserved tokens use the name as the type and have no args.
;;; Reserved tokens:
;;;  case class data default deriving else hiding if import in infix
;;;  infixl infixr instance interface let module of renaming then to
;;;  type where .. :: => = @ \ | ~ <- -> `
;;; Other tokens:
;;;  (file string)
;;;  (newline line indent-column)
;;;  (conid string)
;;;  (varid string)
;;;  (consym string)
;;;  (varsym string)
;;;  (comment string) ;;; not used at the moment
;;;  (integer integer)
;;;  (float integer fraction exponent) 
;;;  (string string)
;;;  (eof)


;;; *** All of the stuff for lexing character and string literals is
;;; *** broken because it assumes that the host Lisp uses the ASCII
;;; *** encoding for characters and supports at least 255 characters.
;;; *** I have marked the specific places in the code where these
;;; *** assumptions are made, but fixing the problem will probably
;;; *** require more drastic changes anyway -- such as using integers
;;; *** instead of characters and vectors of integers instead of characters
;;; *** throughout the compiler.

(define *max-char* 255)  ; highest char-code allowed.

;;; This defines the long names of the control chars.  Note that some of
;;; this duplicates the table above & the reader.

(define *control-char-names* '(
  ("NUL" . 0) ("SOH" . 1) ("STX" . 2) ("ETX" . 3)
  ("EOT" . 4) ("ENQ" . 5) ("ACK" . 6) ("BEL" . 7)
  ("BS" . 8) ("HT" . 9) ("LF" . 10) ("VT" . 11)
  ("FF" . 12) ("CR" . 13) ("SO" . 14) ("SI" . 15)
  ("DLE" . 16) ("DC1" . 17) ("DC2" . 18) ("DC3" . 19)
  ("DC4" . 20) ("NAK" . 21) ("SYN" . 22) ("ETB" . 23)
  ("CAN" . 24) ("EM" . 25) ("SUB" . 26) ("ESC" . 27)
  ("FS" . 28) ("GS" . 29) ("RS" . 30) ("US" . 31)
  ("SP" . 32) ("DEL" . 127)))

;;; This defines the short names for a few control chars.  This
;;; is keyed off the previous table

(define *short-control-char-names* '(
   (#\a . "BEL")    (#\b . "BS")    (#\f . "FF")    (#\n . "LF")
   (#\r . "CR") (#\t . "HT") (#\v . "VT")))

;;; This is used in the ^X construct.  Assume that ^X = code for ^A + X-A
;;; *** This is an invalid assumption.

(define *control-A* 1)

;;; This function is the interface between the lexer and the rest
;;; of the system.  Note that the `file' reported in error messages
;;; must be bound in an outer context.


;;; *** I think this function should be binding these variables and not
;;; *** just assigning them.

(define (lex-port port literate?)
  (setf *lex-literate?* literate?)
  (setf *current-line* 1)
  (setf *current-col* 0)
  (setf *on-new-line?* '#t)
  (setf *save-col?* '#f)
  (setf *port* port)
  (setf *tokens* '())
  (setf *char* (read-char *port*))
  (setf *peek-char* (read-char *port*))
  (when (eof-object? *char*)
	(setf *char* '#\space))
  (when (eof-object? *peek-char*)
	(setf *peek-char* '#\space))
  (setf *at-eof/p?* '#f)
  (setf *at-eof?* '#f)
  (when *lex-literate?*
     (process-literate-comments '#t))
  (parse-till-eof)
  (nreverse *tokens*))

(define (parse-till-eof)
  (cond (*at-eof?*
	 (emit-token 'eof)
	 '())
	(else
	 (lex-one-token)
	 (parse-till-eof))))

;;; There is an assumption that the scanner never peeks beyond a newline.
;;; In literate mode, this may reveal the wrong thing.

(define (advance-char)
  (if (and *lex-literate?* (eqv? *char* #\newline))
      (process-literate-comments '#f)
      (advance-char-1)))

(define (advance-char-1)
  (cond ((eqv? *char* #\newline)
	 (setf *on-new-line?* '#t)
	 (incf (the fixnum *current-line*))
	 (setf *current-col* 0))
	((eqv? *char* #\tab)
	 (incf (the fixnum *current-col*) (- 8 (modulo *current-col* 8))))
	(else
	 (incf (the fixnum *current-col*))))
  (setf *char* *peek-char*)
  (setf *at-eof?* *at-eof/p?*)
  (setf *peek-char* (read-char *port*))
  (when (eof-object? *peek-char*)
     (setf *at-eof/p?* '#t)
     (setf *peek-char* '#\space))
  *char*)

(define (peek-char-2)
  (let ((ch (peek-char *port*)))
    (if (eof-object? ch)
	'#\space
	ch)))

(define (lex-one-token)
 (setf *start-line* *current-line*) ; capture the loc at the start of the token
 (setf *start-col* *current-col*)
 (unless *at-eof?*
  (char-case *char*
    (whitechar
     (advance-char)
     (lex-one-token))
    (#\- (char-case *peek-char*
	    (#\- (lex-comment))
	    (#\> (advance-char)
		 (advance-char)
		 (emit-token '\-\>))
	    (#\} (signal-missing-begin-comment)
		 (advance-char)
		 (advance-char)
		 (lex-one-token))
	    (else
	     (lex-varsym))))
    (#\{ (cond ((char=? *peek-char* '#\-)
		(advance-char)
		(advance-char)
		(cond ((char=? *char* '#\#)
		       (advance-char)
		       (emit-token 'begin-annotation))
		      (else
		       (lex-ncomment)
		       (lex-one-token))))
	       (else
		(advance-char)
		(emit-token '\{ ))))
    (small (lex-varid))
    (large (lex-conid))
    (#\( (advance-char)
	 (emit-token '\())
    (#\: (lex-consym))
    (#\` (advance-char)
	 (emit-token '\`))
    ((symbol presymbol) (lex-varsym))
    (digit (lex-numeric))
    (#\' (lex-char))
    (#\" (lex-string))
    (#\) (advance-char)
	 (emit-token '\)))
    (#\, (advance-char)
	 (emit-token '\,))
    (#\; (advance-char)
	 (emit-token '\;))
    (#\[ (advance-char)
	 (emit-token '\[))
    (#\] (advance-char)
	 (emit-token '\]))
    (#\_ (advance-char)
	 (emit-token '\_))
    (#\} (advance-char)
	 (emit-token '\}))
    (else
     (signal-invalid-character *char*)
     (advance-char)
     (lex-one-token)))))

(define (signal-missing-begin-comment)
  (lexer-error 'missing-begin-comment
	       "`-}' appears outside of a nested comment."))

(define (signal-invalid-character ch)
  (lexer-error 'invalid-character 
	       "Invalid character `~a' appears in source program." ch))

(define (advance-past-white)
  (unless *at-eof?*
    (char-case *char*
      (whitechar
        (advance-char)
	(advance-past-white))
      (else
       '()))))

(define (process-literate-comments at-start?)
  (unless at-start? (advance-char-1))
  (let ((l (classify-line)))
    (cond ((or *at-eof?* (eq? l 'program))
	   '())
	  ((eq? l 'blank)
	   (skip-literate-comment '#t))
	  (else
	   (when (not at-start?)
		 (lexer-error 'blank-line-needed
		    "Literate comments must be preceeded by a blank line"))
	   (skip-literate-comment '#f)))))

(define (skip-literate-comment prev-blank)
  (skip-past-line)
  (let ((l (classify-line)))
    (cond (*at-eof?*
	   '())
	  ((eq? l 'comment)
	   (skip-literate-comment '#f))
	  ((eq? l 'blank)
	   (skip-literate-comment '#t))
	  (else
	   (when (not prev-blank)
	     (lexer-error 'blank-line-needed
		  "Literate comments must be followed by a blank line"))))))
  
(define (classify-line)
  (if *at-eof?*
      'blank
      (char-case *char*
       (#\>
	(advance-char-1)
	'program)
       (#\newline 'blank)
       (whitechar
	(classify-line-1))
       (else 'comment))))

(define (classify-line-1)
  (advance-char-1)
  (char-case *char*
    (#\newline 'blank)
    (whitechar (classify-line-1))
    (else 'comment)))

(define (skip-past-line)
  (when (not *at-eof?*)
    (char-case *char*
      (#\newline
       (advance-char-1))
      (else
       (advance-char-1)
       (skip-past-line)))))
	  
(define (lex-comment)  ;; a -- style comment
  (advance-char)
  (cond (*at-eof?* (lexer-eof-in-comment *current-line*))
	((char=? *char* #\newline)
	 (lex-one-token))
	(else
	 (lex-comment))))

(define (lexer-eof-in-comment start-line)
  (signal-eof-in-comment start-line)
  (lex-one-token))  ; will return the eof token

(define (signal-eof-in-comment start-line)
  (lexer-error 'eof-in-comment
	       "End of file in comment starting at line ~A." start-line))

;;; Here *char* and *peek-char* are the first two chars on a line.

(define (scan-symbol)
  (scan-list-of (symbol #\:)))

(define (scan-var-con)
  (scan-list-of (large small digit #\' #\_)))

(define (lex-ncomment)
  (lex-ncomment-1 *current-line*))

(define (lex-ncomment-1 start-line)
 (if *at-eof?*
  (lexer-eof-in-comment start-line)
  (char-case *char*
    (#\- (cond ((char=? *peek-char* #\})
		(advance-char)
		(advance-char))
	       (else
		(advance-char)
		(lex-ncomment-1 start-line))))
    (#\{ (cond ((char=? *peek-char* #\-)
		(advance-char)
		(advance-char)
		(lex-ncomment)
		(lex-ncomment-1 start-line))
	       (else
		(advance-char)
		(lex-ncomment-1 start-line))))
    (else
     (advance-char)
     (lex-ncomment-1 start-line)))))

(define (lex-varid)
  (let ((sym (scan-var-con)))
    (parse-reserved sym varid
       "case" "class"
       "data" "default" "deriving"
       "else"
       "hiding"
       "if" "import" "in" "infix" "infixl" "infixr" "instance" "interface"
       "let"
       "module"
       "of"
       "renaming"
       "then" "to" "type"
       "where")))

(define (lex-conid)
  (let ((sym (scan-var-con)))
    (emit-token/string 'conid sym)))

(define (lex-consym)
  (let ((sym (scan-symbol)))
    (cond ((string=/list? (cdr sym) ":")
	   (emit-token '\:\:))
	  (else
	   (emit-token/string 'consym sym)))))

(define (lex-varsym)
  (let ((sym (scan-symbol)))
    (cond ((and (string=/list? sym "<") (char=? *char* #\-))
	   (advance-char)
	   (emit-token '\<\-))
	  ((and (string=/list? sym "#")
		(char=? *char* #\-)
		(char=? *peek-char* #\}))
	   (advance-char)
	   (advance-char)
	   (emit-token 'end-annotation))
	  (else
	   (parse-reserved sym varsym
	      ".."
	      "=>" "="
	      "@"
	      "\\"
	      "|"
	      "~")))))

(define (lex-integer radix)
  (lex-integer-1 radix 0))

(define (lex-integer-1 radix psum)
  (declare (type fixnum radix)
	   (type integer psum))
  (let ((d  (char->digit *char* radix)))
    (if d
	(begin
	  (advance-char)
	  (lex-integer-1 radix (+ (* psum radix) (the fixnum d))))
	psum)))

(define (lex-fraction int-part denominator)
  (declare (type integer int-part denominator))
  (let ((d  (char->digit *char* 10)))
    (if d
	(begin
	  (advance-char)
	  (lex-fraction
	    (+ (* int-part 10) (the fixnum d)) (* denominator 10)))
	(values int-part denominator))))

(define (lex-numeric)
  (let ((int-part (lex-integer 10)))
    (if (and (char=? *char* #\.)
	     (char->digit *peek-char* 10))
	(lex-float int-part)
	(emit-token 'integer int-part))))

(define (lex-float int-part)
  (advance-char)
  (multiple-value-bind (numerator denominator) (lex-fraction int-part 1)
    (let ((no-exponent
	   (lambda () (emit-token 'float numerator denominator 0))))
      (char-case *char*
	(exponent
	  (char-case *peek-char*
	    (digit
	     (advance-char)
	     (lex-float/exp numerator denominator 1))
	    ((#\+ #\-)
	     (cond ((char->digit (peek-char-2) 10)
		    (let ((sign (if (char=? *peek-char* '#\+) 1 -1)))
		      (advance-char)
		      (advance-char)
		    (lex-float/exp numerator denominator sign)))
		 (else
		  (funcall no-exponent))))
	  (else
	   (funcall no-exponent))))
       (else
	(emit-token 'float numerator denominator 0))))))

(define (lex-float/exp numerator denominator sign)
  (let ((exponent (lex-integer 10)))
    (emit-token 'float numerator denominator (* sign exponent))))

(define (lex-char)
  (advance-char)
  (let ((c
    (char-case *char*
      (#\' (signal-null-character)
	   '#\?)
      (#\\ (lex-escaped-char '#f))
      ((#\space graphic)
       (let ((ch *char*))
	 (advance-char)
	 ch))
      (else
       (signal-bad-character-constant *char*)
       (advance-char)
       `#\?))))
    (cond ((char=? *char* '#\')
	   (advance-char)
	   (emit-token 'char c))
	  (else
	   (signal-missing-char-quote)
	   (skip-to-quote-or-eol)))))

(define (signal-null-character)
  (lexer-error 'null-character
	       "Null character '' is illegal - use '\\'' for a quote."))

(define (signal-bad-character-constant ch)
  (lexer-error 'bad-character-constant
	       "The character `~a' may not appear in a character literal." ch))

(define (signal-missing-char-quote)
  (lexer-error 'missing-char-quote
	       "Character constant has more than one character."))
  

(define (skip-to-quote-or-eol)
  (if *at-eof?*
      (lex-one-token)
      (char-case *char*
	 (#\' (advance-char)
	      (lex-one-token))
	 (#\newline (advance-char)
		    (lex-one-token))
	 (else
	  (advance-char)
	  (skip-to-quote-or-eol)))))

(define (lex-string)
  (advance-char)
  (emit-token 'string (list->string (gather-string-chars))))

(define (gather-string-chars)
  (char-case *char*
    (#\\
      (let ((ch (lex-escaped-char '#t)))
	(if (eq? ch 'null)
	    (gather-string-chars)
	    (cons ch (gather-string-chars)))))
    (#\"
      (advance-char)
      '())
    ((graphic #\space)
     (let ((ch *char*))
       (advance-char)
       (cons ch (gather-string-chars))))
    (#\newline
     (signal-missing-string-quote)
     '())
    (else
     (signal-bad-string-constant *char*)
     (advance-char)
     (gather-string-chars))))

(define (signal-missing-string-quote)
  (lexer-error 'missing-string-quote
	       "String continued over end of line."))

(define (signal-bad-string-constant ch)
  (lexer-error 'bad-string-constant
	       "The character `~a' may not appear in a string literal." ch))


(define (convert-stupid-control-character-names)
  (let ((c1 *char*)
	(c2 *peek-char*))
    (advance-char)
    (advance-char)
    (let ((s2 (string c1 c2))
	  (s3 (string c1 c2 *char*)))
      (let ((srch3 (assoc s3 *control-char-names*)))
	(cond (srch3
	       (advance-char)
	       (integer->char (cdr srch3)))
	      (else
	       (let ((srch2 (assoc s2 *control-char-names*)))
		 (cond (srch2
			(integer->char (cdr srch2)))
		       (else
			(signal-bad-control-char s3)
			`#\?)))))))))

(define (signal-bad-control-char name)
  (lexer-error 'invalid-control-char
	       "`~a' is not a recognized control character name." name))


(define (lex-escaped-char in-string?)
  (advance-char)
  (char-case *char*
    ((#\a #\b #\f #\n #\r #\t #\v)
     (let* ((ccode (cdr (assoc *char* *short-control-char-names*)))
	    (ccode1 (cdr (assoc ccode *control-char-names*))))
       (advance-char)
       (integer->char ccode1)))
    ((#\\ #\' #\")
     (let ((ch *char*))
       (advance-char)
       ch))
    (#\&
     (advance-char)
     (cond (in-string? 'null)
	   (else
	    (signal-bad-&-escape)
	    '#\?)))
    (#\^
     ;; *** This code is problematic because it assumes
     ;; *** (1) that you can do the arithmetic on the character codes
     ;; *** (2) that the resulting integer can actually be coerced to
     ;; ***     the right character object in the host Lisp.
     (advance-char)
     (char-case *char*
       ((large #\@ #\[ #\\ #\] #\^ #\_)
	(let ((code (+ (- (char->integer *char*)
			  (char->integer '#\A))
		       *control-A*)))
	  (advance-char)
	  (integer->char code)))
       (else
	(signal-bad-^-escape *char*)
	'#\?)))
    (large
     (convert-stupid-control-character-names))
    (digit
     (convert-num-to-char (lex-integer 10)))
    (#\o
     (advance-char)
     (cond ((char->digit *char* 8)
	    (convert-num-to-char (lex-integer 8)))
	   (else
	    (signal-missing-octal-digits)
	    '#\?)))
    (#\x
     (advance-char)
     (cond ((char->digit *char* 16)
	    (convert-num-to-char (lex-integer 16)))
	   (else
	    (signal-missing-hex-digits)
	    `#\?)))
    (whitechar
     (cond (in-string?
	    (lex-gap))
	   (else
	    (signal-bad-gap)
	    `#\?)))
    (else
     (signal-bad-escape *char*)
     `#\?)))

(define (signal-bad-&-escape)
  (lexer-error 'bad-&-escape
	       "The escape `\\&' is not allowed inside a character literal."))

(define (signal-bad-^-escape ch)
  (lexer-error 'bad-^-escape
	       "The escape `\\^~a' is not recognized." ch))

(define (signal-missing-octal-digits)
  (lexer-error 'missing-octal-digits
	       "No digits provided for `\\o' escape."))

(define (signal-missing-hex-digits)
  (lexer-error 'missing-hex-digits
	       "No digits provided for `\\x' escape."))

(define (signal-bad-gap)
  (lexer-error 'invalid-gap
	       "Gaps are not allowed inside character literals."))

(define (signal-bad-escape ch)
  (lexer-error 'bad-escape
	       "The escape `\\~a' is not recognized." ch))



;;; *** This code is problematic because it assumes that integers
;;; *** between 0 and 255 map on to characters with the corresponding
;;; *** ASCII encoding in the host Lisp, and that the host Lisp actually
;;; *** supports 255 characters.

(define (convert-num-to-char num)
  (cond ((and (>= num 0) (>= *max-char* num))
	 (integer->char num))
	(else
	 (signal-char-out-of-range num)
	 '#\?)))

(define (signal-char-out-of-range num)
  (lexer-error 'char-out-of-range
	       "There is no character corresponding to code ~s." num))


(define (lex-gap)
  (cond (*at-eof?*
	 (signal-eof-in-gap)
	 'null)
	(else
	 (char-case *char*
	   (whitechar
	    (advance-char)
	    (lex-gap))
	   (#\\
	    (advance-char)
	    'null)
	   (else
	    (signal-missing-gap)
	    'null)))))
  
      
(define (signal-eof-in-gap)
  (lexer-error 'eof-in-gap
	       "End of file encountered inside gap."))

(define (signal-missing-gap)
  (lexer-error 'missing-gap
	       "Missing gap delimiter, or junk inside gap."))
