;;; print-valdefs.scm -- print AST structures for local declarations
;;;
;;; author :  Sandra Loosemore
;;; date   :  14 Jan 1992
;;;
;;; This file corresponds to ast/valdef-structs.scm.
;;;
;;;



(define-ast-printer signdecl (object xp)
  (with-ast-block (xp)
    (write-delimited-list (signdecl-vars object) xp (function write) "," "" "")
    (write-string " ::" xp)
    (write-whitespace xp)
    (write (signdecl-signature object) xp)))


;;; This interacts with the layout rule stuff.  See util.scm.

(define-ast-printer valdef (object xp)
  (let ((lhs         (valdef-lhs object))
	(definitions (valdef-definitions object)))
    (write-definition lhs (car definitions) xp)
    (dolist (d (cdr definitions))
      (if (dynamic *print-pretty*)
	  (pprint-newline 'mandatory xp)
	  (write-string "; " xp))
      (write-definition lhs d xp))))


(define (write-definition lhs d xp)
  (with-ast-block (xp)
    (let ((args        (single-fun-def-args d))
	  (rhs-list    (single-fun-def-rhs-list d))
	  (where-decls (single-fun-def-where-decls d))
	  (infix?      (single-fun-def-infix? d)))
      (write-lhs lhs args infix? xp)
      (write-rhs rhs-list xp)
      (write-wheredecls where-decls xp)
      )))
				
(define (write-lhs lhs args infix? xp)       
  (cond ((null? args)
	 ;; pattern definition
	 (write-apat lhs xp)
	 )
        ;; If there are args, the lhs is always a var-pat pointing to a 
        ;; var-ref. The infix? slot from the single-fun-def must override
	;; the slot on the var-ref, since there can be a mixture of
	;; infix and prefix definitions for the same lhs.
	(infix?
	 ;; operator definition
	 (when (not (null? (cddr args)))
	   (write-char #\( xp))
	 (write-apat (car args) xp)
	 (write-whitespace xp)
	 (write-varop (var-ref-name (var-pat-var lhs)) xp)
	 (write-whitespace xp)
	 (write-apat (cadr args) xp)
	 (when (not (null? (cddr args)))
	   (write-char #\) xp)
	   (write-whitespace xp)
	   (write-delimited-list (cddr args) xp (function write-apat)
				 "" "" "")))
	(else
	 ;; normal prefix function definition
	 (write-varid (var-ref-name (var-pat-var lhs)) xp)
	 (write-whitespace xp)
	 (write-delimited-list args xp (function write-apat) "" "" ""))
	))

(define (write-rhs rhs-list xp)
  (let ((guard   (guarded-rhs-guard (car rhs-list)))
	(rhs     (guarded-rhs-rhs   (car rhs-list))))
    (when (not (is-type? 'omitted-guard guard))
      (write-string " | " xp)
      (write guard xp))
    (write-string " =" xp)
    (write-whitespace xp)
    (write rhs xp)
    (when (not (null? (cdr rhs-list)))
      (write-newline xp)
      (write-rhs (cdr rhs-list) xp))))


;;; Pattern printers


;;; As per jcp suggestion, don't put whitespace after @; line break comes
;;; before, not after (as is the case for other infix-style punctuation).
    
(define-ast-printer as-pat (object xp)
  (with-ast-block (xp)
    (write (as-pat-var object) xp)
    (write-whitespace xp)
    (write-string "@" xp)
    (write-apat (as-pat-pattern object) xp)))

(define (write-apat pat xp)
  (if (or (is-type? 'apat pat)
	  (is-type? 'pp-pat-plus pat)  ; hack per jcp
	  (and (is-type? 'pcon pat)
	       (or (null? (pcon-pats pat))
		   (eq? (pcon-con pat) (core-symbol "UnitConstructor"))
		   (is-tuple-constructor? (pcon-con pat)))))
      (write pat xp)
      (begin
        (write-char #\( xp)
        (write pat xp)
        (write-char #\) xp))))

(define-ast-printer irr-pat (object xp)
  (write-string "~" xp)
  (write-apat (irr-pat-pattern object) xp))

(define-ast-printer var-pat (object xp)
  (write (var-pat-var object) xp))

(define-ast-printer wildcard-pat (object xp)
  (declare (ignore object))
  (write-char #\_ xp))

(define-ast-printer const-pat (object xp)
  (write (const-pat-value object) xp))

(define-ast-printer plus-pat (object xp)
  (write (plus-pat-pattern object) xp)
  (write-string " + " xp)
  (write (plus-pat-k object) xp))



(define-ast-printer pcon (object xp)
  (let ((name    (pcon-name object))
	(pats    (pcon-pats object))
	(infix?  (pcon-infix? object))
	(def     (pcon-con object)))
    (cond ((eq? def (core-symbol "UnitConstructor"))
	   (write-string "()" xp))
	  ((is-tuple-constructor? def)
	   (write-commaized-list pats xp))
          ((null? pats)
	   (if infix?
	       ;; infix pcon with no arguments can happen inside pp-pat-list
	       ;; before precedence parsing happens.
	       (write-conop name xp)
	       (write-conid name xp)))
	  (infix?
	   ;; This could be smarter about dealing with precedence of patterns.
	   (with-ast-block (xp)
	     (write-apat (car pats) xp)
	     (write-whitespace xp)
	     (write-conop name xp)
	     (write-whitespace xp)
	     (write-apat (cadr pats) xp)))
	  (else
	   (with-ast-block (xp)
	     (write-conid name xp)
	     (write-whitespace xp)
	     (write-delimited-list pats xp (function write-apat) "" "" "")))
	  )))

(define-ast-printer list-pat (object xp)
  (write-delimited-list
    (list-pat-pats object) xp (function write) "," "[" "]"))

(define-ast-printer pp-pat-list (object xp)
  (write-delimited-list (pp-pat-list-pats object) xp (function write-apat)
			"" "" ""))

(define-ast-printer pp-pat-plus (object xp)
  (declare (ignore object))
  (write-string "+ " xp))

(define-ast-printer pp-pat-negated (object xp)
  (declare (ignore object))
  (write-string "-" xp))

