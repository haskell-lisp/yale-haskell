;;;  File: pattern-parser        Author: John

;;; This parses the pattern syntax except for the parts which need to be
;;; resolved by precedence parsing.

;;; This parses a list of alternating pats & conops.

(define (parse-pat)
 (trace-parser pat
   (let ((res (parse-pat/list)))
     (if (null? (cdr res))
	 (car res)
	 (make pp-pat-list (pats res))))))

;;; This parses a list of patterns with intervening conops and + patterns

(define (parse-pat/list)
  (token-case
    (con (let ((pcon (pcon->ast)))
	   (setf (pcon-pats pcon) (parse-apat-list))
	   (cons pcon (parse-pat/tail))))
    (-n
     (advance-token) ; past -
     (token-case
      (numeric (let ((val (literal->ast)))
		 (cons (make pp-pat-negated)
		       (cons (make const-pat (value val))
			     (parse-pat/tail)))))
      (else
       (signal-missing-token "<number>" "negative literal pattern"))))
    (var
     (let ((var (var->ast)))
       (token-case
	(+k (cons (make var-pat (var var))
		  (parse-+k-pat)))
	(@  (let ((pattern (parse-apat)))
	      (cons (make as-pat (var var) (pattern pattern))
		    (parse-pat/tail))))
	(else (cons (make var-pat (var var)) (parse-pat/tail))))))
    (_
     (let ((pat (make wildcard-pat)))
       (token-case
	(+k (cons pat (parse-+k-pat)))
	(else (cons pat (parse-pat/tail))))))
    (else (let ((apat (parse-apat)))
	    (cons apat (parse-pat/tail))))))


(define (parse-+k-pat)
  (advance-token)  ; past +
  (token-case
   (k (let ((val (literal->ast)))
	(cons (make pp-pat-plus)
	      (cons (make const-pat (value val))
		    (parse-pat/tail)))))
   (else (signal-missing-token "<integer>" "successor pattern"))))

(define (parse-pat/tail)
   (token-case
     (conop
      (let ((con (pconop->ast)))
	(cons con (parse-pat/list))))
     (else '())))

(define (parse-apat)
 (trace-parser apat
   (token-case
     (var (let ((var (var->ast)))
	    (token-case
	     (@
	      (let ((pattern (parse-apat)))
		(make as-pat (var var) (pattern pattern))))
	     (else (make var-pat (var var))))))
     (con (pcon->ast))
     (literal (let ((value (literal->ast)))
		(make const-pat (value value))))
     (_ (make wildcard-pat))
     (\( (token-case
           (\) (**pcon/def (core-symbol "UnitConstructor") '()))
	   (else
	    (let ((pat (parse-pat)))
	      (token-case
		(\, (**pcon/tuple (cons pat (parse-pat-list '\)))))
		(\) pat)
		(else
		 (signal-missing-token "`)' or `,'" "pattern")))))))
     (\[ (token-case
	  (\] (make list-pat (pats '())))
	  (else (make list-pat (pats (parse-pat-list '\]))))))
     (\~ (let ((pattern (parse-apat)))
	   (make irr-pat (pattern pattern))))
     (else
      (signal-invalid-syntax "an apat")))))

(define (parse-pat-list term)  ;; , separated
  (let ((pat (parse-pat)))
    (token-case
     (\, (cons pat (parse-pat-list term)))
     ((unquote term) (list pat))
     (else
      (signal-missing-token
        (if (eq? term '\)) "`)'" "`]'")
	"pattern")))))

(define (parse-apat-list)  ;; space separated
  (token-case
    (apat-start
     (let ((pat (parse-apat)))
       (cons pat (parse-apat-list))))
    (else
     '())))

;;; The following routine scans patterns without creating ast structure.
;;; They return #t or #f depending on whether a valid pattern was encountered.
;;; The leave the scanner pointing to the next token after the pattern.

(define (scan-pat)  ; same as parse-pat/list
  (and
   (token-case
    (con (scan-con)
	 (scan-apat-list))
    (-n (advance-token)
	(token-case
	 (numeric (advance-token)
		  '#t)
	 (else '#f)))
    (var (and (scan-var)
	      (token-case
	       (@ (scan-apat))
	       (+k (scan-+k))
	       (else '#t))))
    (_ (scan-+k))
    (else (scan-apat)))
   (scan-pat/tail)))

(define (scan-pat/tail)
  (token-case
   (conop (and (scan-conop)
	       (scan-pat)))
   (else '#t)))

(define (scan-apat)
  (token-case
   (var (scan-var)
	(token-case
	 (@ (scan-apat))
	 (else '#t)))
   (con (scan-con))
   (literal (advance-token)
	    '#t)
   (_ '#t)
   (\( (token-case
	(\) '#t)
	(else
	 (and (scan-pat)
	      (token-case
	       (\, (scan-pat-list '\)))
	       (\) '#t)
	       (else '#f))))))
   (\[ (token-case
	(\] '#t)
	(else (scan-pat-list '\]))))
   (\~ (scan-apat))
   (else '#f)))

(define (scan-pat-list term)
  (and (scan-pat)
       (token-case
	(\, (scan-pat-list term))
	((unquote term) '#t)
	(else '#f))))

(define (scan-apat-list)
  (token-case
   (apat-start
    (and (scan-apat)
	 (scan-apat-list)))
   (else '#t)))

(define (scan-var)
  (token-case
   (varid '#t)
   (\( (token-case
	(varsym
	 (token-case
	  (\) '#t)
	  (else '#f)))
	(else '#f)))
   (else '#f)))

(define (scan-con)
  (token-case
   (conid '#t)
   (\( (token-case
	(consym
	 (token-case
	  (\) '#t)
	  (else '#f)))
	(else '#f)))
   (else '#f)))

(define (scan-conop)
  (token-case
   (consym '#t)
   (\` (token-case
	(conid
	 (token-case
	  (\` '#t)
	  (else '#f)))
	(else '#f)))
   (else '#f)))

(define (scan-+k)
  (token-case
   (+k (advance-token)  ; past the +
       (token-case
	(integer '#t)
	(else '#f)))
   (else '#t)))

