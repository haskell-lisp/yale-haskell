;;; print-exps.scm -- print expression AST structures
;;;
;;; author :  Sandra Loosemore
;;; date   :  10 Jan 1992
;;;
;;; This file corresponds to ast/exp-structs.scm.
;;;

(define-ast-printer lambda (object xp)
  (with-ast-block (xp)
    (write-string "\\ " xp)
    (write-delimited-list
      (lambda-pats object) xp (function write-apat) "" "" "")
    (write-string " ->" xp)
    (write-whitespace xp)
    (write (lambda-body object) xp)))

(define-ast-printer let (object xp)
  (write-lets-body "let " (let-decls object) (let-body object) xp))

(define (write-lets-body let-name decls body xp)
  (pprint-logical-block (xp '() "" "")  ; no extra indentation
    (write-string let-name xp)
    (write-layout-rule (remove-recursive-grouping decls) xp (function write))
    (write-whitespace xp)
    (write-string "in " xp)
    (write body xp)))

(define-ast-printer if (object xp)
  (with-ast-block (xp)
    (write-string "if " xp)
    (write (if-test-exp object) xp)
    (write-whitespace xp)
    (with-ast-block (xp)
      (write-string "then" xp)
      (write-whitespace xp)
      (write (if-then-exp object) xp))
    (write-whitespace xp)
    (with-ast-block (xp)
      (write-string "else" xp)
      (write-whitespace xp)
      (write (if-else-exp object) xp))))

(define-ast-printer case (object xp)
  (with-ast-block (xp)
    (write-string "case " xp)
    (write (case-exp object) xp)
    (write-string " of" xp)
    (write-whitespace xp)
    (write-layout-rule (case-alts object) xp (function write))))

(define-ast-printer alt (object xp)
  (with-ast-block (xp)
    (write (alt-pat object) xp)
    (dolist (r (alt-rhs-list object))
      (write-whitespace xp)
      (unless (is-type? 'omitted-guard (guarded-rhs-guard r))
	(write-string "| " xp)
	(write (guarded-rhs-guard r) xp))
      (write-string " -> " xp)
      (write (guarded-rhs-rhs r) xp))
    (write-wheredecls (alt-where-decls object) xp)))

(define-ast-printer exp-sign (object xp)
  (with-ast-block (xp)
    (write (exp-sign-exp object) xp)
    (write-string " ::" xp)
    (write-whitespace xp)
    (write (exp-sign-signature object) xp)))

;;; Have to look for application of special-case constructors before
;;; doing the normal prefix/infix cases.

(define-ast-printer app (object xp)
  (let* ((fn          (app-fn object))
	 (arg         (app-arg object)))
    (multiple-value-bind (con args) (extract-constructor fn (list arg))
      (cond ;; ((eq? con (core-symbol "UnitConstructor"))
	    ;;  *** Does this ever happen?
	    ;;  (write-string "()" xp))
	    ((and con (is-tuple-constructor? con))
	     (write-commaized-list args xp))
	    (else
	     (multiple-value-bind (fixity op arg1) (extract-infix-operator fn)
	       (if fixity
		   (write-infix-application fixity op arg1 arg xp)
		   (write-prefix-application fn arg xp))))
	    ))))


(define (write-infix-application fixity op arg1 arg2 xp)
  (let ((precedence      (fixity-precedence fixity))
	(associativity   (fixity-associativity fixity)))
    (with-ast-block (xp)
      (write-exp-with-precedence
        arg1 (1+ precedence) (if (eq? associativity 'l) 'l '#f) xp)
      (write-whitespace xp)
      (write op xp)
      (write-whitespace xp)
      (write-exp-with-precedence
        arg2 (1+ precedence) (if (eq? associativity 'r) 'r '#f) xp))))

(define (write-prefix-application fn arg xp)      
  (with-ast-block (xp)
    (write-exp-with-precedence fn 10 '#f xp)
    (write-whitespace xp)
    (write-aexp arg xp)))


;;; Write an expression with at least the given precedence.  If the
;;; actual precedence is lower, put parens around it.

(define *print-exp-parens* '#f)

(define (write-exp-with-precedence exp precedence associativity xp)
  (if *print-exp-parens*
      (write-aexp exp xp)
      (if (< (precedence-of-exp exp associativity) precedence)
	  (begin
	    (write-char #\( xp)
	    (write exp xp)
	    (write-char #\) xp))
	  (write exp xp))))


;;; Similar to the above: write an aexp.

(define *print-original-code* '#t)

(define (write-aexp object xp)
  (if (is-type? 'save-old-exp object)
      (write-aexp (if *print-original-code*
		      (save-old-exp-old-exp object)
		      (save-old-exp-new-exp object))
		  xp)
      (if (or (is-type? 'aexp object)
	      (pp-exp-list-section? object)
	      (is-type? 'negate object))
	  (write object xp)
	  (begin
	    (write-char #\( xp)
	    (write object xp)
	    (write-char #\) xp)))))


;;; The infix? slot on var-ref and con-ref structs refers to whether
;;; the thing appears as an infix operator or not, not whether the name
;;; has operator or identifier syntax.

(define-ast-printer var-ref (object xp)
  (let ((name  (var-ref-name object)))
    (if (var-ref-infix? object)
	(write-varop name xp)
	(write-varid name xp))))

(define-ast-printer con-ref (object xp)
  (if (eq? (con-ref-con object) (core-symbol "UnitConstructor"))
      (write-string "()" xp)
      (let ((name  (con-ref-name object)))
	(if (con-ref-infix? object)
	    (write-conop name xp)
	    (write-conid name xp)))))


(define-ast-printer integer-const (object xp)
  (write (integer-const-value object) xp))

(define-ast-printer float-const (object xp)
  (let* ((numerator   (float-const-numerator object))
	 (denominator (float-const-denominator object))
	 (exponent    (float-const-exponent object))
	 (whole       (quotient numerator denominator))
	 (fraction    (remainder numerator denominator)))
    (write whole xp)
    (write-char #\. xp)
    (write-precision-integer fraction denominator xp)
    (unless (zero? exponent)
      (write-char #\E xp)
      (write exponent xp))))

(define (write-precision-integer fraction denominator xp)
  (cond ((eqv? denominator 1)
	 ; no fraction
	 )
	((eqv? denominator 10)
	 (write-digit fraction xp))
	(else
	 (write-digit (quotient fraction 10) xp)
	 (write-precision-integer (remainder fraction 10)
				  (quotient denominator 10)
				  xp))
	))

(define (write-digit n xp)
  (write-char (string-ref "0123456789" n) xp))


;;; Character and string printers need to handle weird escapes.
;;; Fortunately we can just choose one canonical style for printing
;;; unprintable characters.

(define-ast-printer char-const (object xp)
  (write-char #\' xp)
  (write-char-literal (char-const-value object) xp #\')
  (write-char #\' xp))

(define-ast-printer string-const (object xp)
  (write-char #\" xp)
  (let ((s  (string-const-value object)))
    (dotimes (i (string-length s))
      (write-char-literal (string-ref s i) xp #\")))
  (write-char #\" xp))

(define (write-char-literal c xp special)
  (cond ((eqv? c special)
	 (write-char #\\ xp)
	 (write c xp))
	((eqv? c #\newline)
	 (write-char #\\ xp)
	 (write-char #\n xp))
	(else
	 (let ((code  (char->integer c)))
	   (if (and (>= code 32) (< code 128))
	       ;; printing ascii characters
	       (write-char c xp)
	       ;; "control" characters print in \ddd notation
	       (begin
		 (write-char #\\ xp)
		 (write code xp)))))
	))

(define-ast-printer list-exp (object xp)
  (write-delimited-list
    (list-exp-exps object) xp (function write) "," "[" "]"))

(define-ast-printer sequence (object xp)
  (with-ast-block (xp)
    (write-string "[" xp)
    (write (sequence-from object) xp)
    (write-string "..]" xp)))

(define-ast-printer sequence-to (object xp)
  (with-ast-block (xp)
    (write-string "[" xp)
    (write (sequence-to-from object) xp)
    (write-string " .." xp)
    (write-whitespace xp)
    (write (sequence-to-to object) xp)
    (write-string "]" xp)))

(define-ast-printer sequence-then (object xp)
  (with-ast-block (xp)
    (write-string "[" xp)			
    (write (sequence-then-from object) xp)
    (write-string "," xp)
    (write-whitespace xp)
    (write (sequence-then-then object) xp)
    (write-string "..]" xp)))

(define-ast-printer sequence-then-to (object xp)
  (with-ast-block (xp)
    (write-string "[" xp)
    (write (sequence-then-to-from object) xp)
    (write-string "," xp)
    (write-whitespace xp)
    (write (sequence-then-to-then object) xp)
    (write-string " .." xp)
    (write-whitespace xp)
    (write (sequence-then-to-to object) xp)
    (write-string "]" xp)))

(define-ast-printer list-comp (object xp)
  (with-ast-block (xp)
    (write-string "[" xp)
    (write (list-comp-exp object) xp)
    (write-string " |" xp)
    (write-whitespace xp)
    (write-delimited-list
      (list-comp-quals object) xp (function write) "," "" "")
    (write-string "]" xp)))


(define-ast-printer section-l (object xp)
  (let* ((exp           (section-l-exp object))
	 (op            (section-l-op object))
	 (fixity        (operator-fixity op))
	 (precedence    (fixity-precedence fixity)))
    (with-ast-block (xp)
      (write-string "(" xp)
      (write op xp)
      (write-whitespace xp)
      (write-exp-with-precedence exp (1+ precedence) '#f xp)
      (write-string ")" xp))))

(define-ast-printer section-r (object xp)
  (let* ((exp           (section-r-exp object))
	 (op            (section-r-op object))
	 (fixity        (operator-fixity op))
	 (precedence    (fixity-precedence fixity)))
    (with-ast-block (xp)
      (write-string "(" xp)
      (write-exp-with-precedence exp (1+ precedence) '#f xp)
      (write-whitespace xp)
      (write op xp)
      (write-string ")" xp))))

(define-ast-printer qual-generator (object xp)
  (with-ast-block (xp)
    (write (qual-generator-pat object) xp)
    (write-string " <-" xp)
    (write-whitespace xp)
    (write (qual-generator-exp object) xp)))

(define-ast-printer qual-filter (object xp)
  (write (qual-filter-exp object) xp))


;;; A pp-exp-list with an op as the first or last element is really
;;; a section.  These always get parens and are treated like aexps.
;;; Other pp-exp-lists are treated as exps with precedence 0.
;;; Bleah...  Seems like the parser ought to recognize this up front....
;;;                                                     Yeah but I'm lazy ...

(define-ast-printer pp-exp-list (object xp)
  (let ((section?  (pp-exp-list-section? object)))
    (if section? (write-char #\( xp))
    (write-delimited-list
      (pp-exp-list-exps object) xp (function write-aexp) "" "" "")
    (if section? (write-char #\) xp))))

(define-ast-printer negate (object xp)
  (declare (ignore object))
  (write-string "-" xp))

(define-ast-printer def (object xp)
  (write-string (symbol->string (def-name object)) xp))

(define-ast-printer con (object xp)
  (write-string (remove-con-prefix (symbol->string (def-name object))) xp))

(define-ast-printer con-number (object xp)
  (with-ast-block (xp)
    (write-string "con-number/" xp)
    (write (con-number-type object) xp)
    (write-whitespace xp)
    (write-aexp (con-number-value object) xp)))

(define-ast-printer sel (object xp)
  (with-ast-block (xp)
    (write-string "sel/" xp)
    (write (sel-constructor object) xp)
    (write-whitespace xp)
    (write (sel-slot object) xp)
    (write-whitespace xp)
    (write-aexp (sel-value object) xp)))

(define-ast-printer is-constructor (object xp)
(with-ast-block (xp)
    (write-string "is-constructor/" xp)
    (write (is-constructor-constructor object) xp)
    (write-whitespace xp)
    (write-aexp (is-constructor-value object) xp)))
  
(define-ast-printer void (object xp)
  (declare (ignore object))
  (write-string "Void" xp))

;;; Special cfn constructs

(define-ast-printer case-block (object xp)
  (with-ast-block (xp)
    (write-string "case-block " xp)
    (write (case-block-block-name object) xp)
    (write-whitespace xp)
    (write-layout-rule (case-block-exps object) xp (function write))))

(define-ast-printer return-from (object xp)
  (with-ast-block (xp)
    (write-string "return-from " xp)
    (write (return-from-block-name object) xp)
    (write-whitespace xp)
    (write (return-from-exp object) xp)))

(define-ast-printer and-exp (object xp)
  (with-ast-block (xp)
    (write-string "and " xp)
    (write-layout-rule (and-exp-exps object) xp (function write))))

;;; Expression types used by the type checker.

(define-ast-printer dict-placeholder (object xp)
  (cond ((not (eq? (dict-placeholder-exp object) '#f))
	 (write (dict-placeholder-exp object) xp))
	(else
	 (write-string "%" xp)
	 (write-string (symbol->string
			(def-name (dict-placeholder-class object))) xp))))

(define-ast-printer recursive-placeholder (object xp)
  (cond ((not (eq? (recursive-placeholder-exp object) '#f))
	 (write (recursive-placeholder-exp object) xp))
	(else
	 (write-varid (def-name (recursive-placeholder-var object)) xp))))

;;; This should probably have a flag to allow the dictionary converted code
;;; to be printed during debugging.

(define-ast-printer save-old-exp (object xp)
  (write (save-old-exp-old-exp object) xp))

