;;;  This file abstracts the representation of tokens.  It is used by both
;;;  the lexer & parser.  This also contains routines for converting
;;;  individual tokens to ast structure.  Routines used by the
;;;  token-case macro in `satisfies' clauses are here too.

;;; Lexer routines for emitting tokens:

(define (emit-token type . args)
  (cond (*on-new-line?*
	 (push (list 'line *start-line* *start-col*) *tokens*))
	(*save-col?*
	 (push (list 'col *start-col*) *tokens*)))
  (push (cons type args) *tokens*)
  (setf *on-new-line?* '#f)
  (setf *save-col?* (memq type '(|where| |of| |let|))))

(define (emit-token/string type string-as-list)
  (emit-token type (list->string string-as-list)))

;;; Parser routines:

;;;  These routines take care of the token stream in the parser.  They
;;;  maintain globals for the current token and its location.  

;;;  Globals used:
;;;   *token-stream*   remaining tokens to be parsed
;;;   *token*          current token type
;;;   *token-args*     current token arguments
;;;   *layout-stack*   columns at which layout is being done
;;;   *current-line*   current line the scanner is on
;;;   *current-col*    current col; valid at start of line & after where,let,of
;;;   *current-file*

(define (init-token-stream tokens)
  (setf *token-stream* tokens)
  (setf *layout-stack* '())
  (advance-token))

(define (advance-token)
  (cond ((null? *token-stream*)
	 (setf *token* 'eof))
	(else
	 (let* ((token (car *token-stream*)))
	   (setf *token-stream* (cdr *token-stream*))
	   (advance-token-1 (car token) (cdr token))))))

(define (advance-token-1 type args)
  (cond ((eq? type 'file)
	 (setf *current-file* (car args))
	 (advance-token))
	((eq? type 'col)
	 (setf *current-col* (car args))
	 (advance-token))
	((eq? type 'line)  ;; assume blank lines have been removed
	 (let ((line (car args))
	       (col (cadr args)))
	   (setf *current-line* line)
	   (setf *current-col* col)
	   (setf *token-stream*
		 (resolve-layout *token-stream* *layout-stack*)))
	 (advance-token))
	(else
	 (setf *token* type)
	 (setf *token-args* args)
	 type)))

(define (insert-extra-token tok-type stream) ; used by layout
  (cons (list tok-type) stream))

;;; This looks for the { to decide of layout will apply.  If so, the layout
;;; stack is pushed.  The body function, fn, is called with a boolean which
;;; tells it the whether layout rule is in force.

;;; *** The CMU CL compiler barfs with some kind of internal error
;;; *** on this function.  See the revised definition below.

;(define (start-layout fn)
;  (token-case
;   (\{ (funcall fn '#f))
;   (else
;    (let/cc recovery-fn
;      (push (cons *current-col* (lambda ()
;				  (let ((res (funcall fn '#t)))
;				    (funcall recovery-fn res))))
;	    *layout-stack*)
;      (funcall fn '#t)))))

(define (start-layout fn)
  (token-case
   (\{ (funcall fn '#f))
   (else
    (let/cc recovery-fn
      (start-layout-1 fn recovery-fn)))))

(define (start-layout-1 fn recovery-fn)
  (push (cons *current-col*
	      (lambda ()
		(let ((res (funcall fn '#t)))
		  (funcall recovery-fn res))))
	*layout-stack*)
  (funcall fn '#t))

(define (layout-col x)
  (car x))

(define (layout-recovery-fn x)
  (cdr x))

(define (close-layout in-layout?)
  (cond (in-layout?
	 (setf *layout-stack* (cdr *layout-stack*))
	 (token-case
	  ($\} '())   ; the advance-token routine may have inserted this
	  (else '())))
	(else
	 (token-case
	  (\} '())
	  (else
	   (signal-missing-brace))))))

(define (signal-missing-brace)
  (parser-error 'missing-brace
		"Missing `}'."))

(define (resolve-layout stream layout-stack)
  (if (null? layout-stack)
      stream
      (let ((col  (layout-col (car layout-stack))))
	(declare (type fixnum col))
	(cond ((= (the fixnum *current-col*) col)
	       (insert-extra-token '\; stream))
	      ((< (the fixnum *current-col*) col)
	       (insert-extra-token
	         '$\} (resolve-layout stream (cdr layout-stack))))
	      (else
	       stream)
	      ))))
	

;;; The following routines are used for backtracking.  This is a bit of
;;; a hack at the moment.

(define (save-scanner-state)
  (vector *token* *token-args* *token-stream* *layout-stack* *current-line*
	  *current-col*))

(define (restore-excursion state)
  (setf *token* (vector-ref state 0))
  (setf *token-args* (vector-ref state 1))
  (setf *token-stream* (vector-ref state 2))
  (setf *layout-stack* (vector-ref state 3))
  (setf *current-line* (vector-ref state 4))
  (setf *current-col* (vector-ref state 5)))

(define (eq-token? type)
  (eq? type *token*))

(define (eq-token-arg? str)
  (string=? str (car *token-args*)))

;;; lookahead into the token stream

(define (peek-1-type)
  (peek-toks 0 *token-stream*))

(define (peek-2-type)
  (peek-toks 1 *token-stream*))

;;; This is a Q&D way of looking ahead.  It does not expand the layout
;;; as it goes so there may be missing } and ;.  This should not matter
;;; in the places where this is used since these would be invalid anyway.
;;; To be safe, token types are rechecked while advancing to verify the
;;; lookahead.

(define (peek-toks n toks)
  (declare (type fixnum n))
  (cond ((null? toks)
	 'eof)
	((memq (caar toks) '(col line))
	 (peek-toks n (cdr toks)))
	((eqv? n 0)
	 (caar toks))
	(else (peek-toks (1- n) (cdr toks)))))

;; These routines handle the `satisfies' clauses used in token-case.

(define (at-varsym/+?)
  (and (eq? *token* 'varsym)
       (string=? (car *token-args*) "+")))

(define (at-varsym/-?)
  (and (eq? *token* 'varsym)
       (string=? (car *token-args*) "-")))

(define (at-varsym/paren?)
  (and (eq? *token* '\()
       (eq? (peek-1-type) 'varsym)
       (eq? (peek-2-type) '\))))

(define (at-consym/paren?)
  (and (eq? *token* '\()
       (eq? (peek-1-type) 'consym)
       (eq? (peek-2-type) '\))))

(define (at-varid/quoted?)
  (and (eq? *token* '\`)
       (eq? (peek-1-type) 'varid)))

(define (at-conid/quoted?)
  (and (eq? *token* '\`)
       (eq? (peek-1-type) 'conid)))

(define (at-+k?)
  (and (at-varsym/+?)
       (eq? (peek-1-type) 'integer)))

(define (at--n?)
  (and (at-varsym/-?)
       (memq (peek-1-type) '(integer float))))

;;;  The following routines convert the simplest tokens to AST structure.

(define-local-syntax (return+advance x)
  `(let ((x ,x))
     (advance-token)
     x))

(define (token->symbol)
 (return+advance
  (string->symbol (car *token-args*))))

(define (token->symbol/con)  ; for conid, aconid
 (return+advance
  (string->symbol (add-con-prefix (car *token-args*)))))

(define (var->symbol)
  (token-case
   (\( (token-case
	(varsym?
	 (let ((res (token->symbol)))
	   (token-case
	    (\) res)
	    (else (signal-missing-token "`)'" "var")))))
	(else (signal-missing-token "<varsym>" "var"))))
   (varid? (token->symbol))))

(define (var->ast)
  (let ((vname (var->symbol)))
    (make var-ref (name vname) (infix? '#f) (var *undefined-def*))))

(define (var->entity) 
  (let ((vname (var->symbol)))
    (make entity-var (name vname))))

(define (con->symbol)
  (token-case
   (\( (token-case
	(consym?
	 (let ((res (token->symbol/con)))
	   (token-case
	    (\) res)
	    (else (signal-missing-token "`)'" "con")))))
	(else (signal-missing-token "<consym>" "con"))))
   (conid? (token->symbol/con))))

(define (varop->symbol)
  (token-case
   (\` (token-case
	(varid?
	 (let ((res (token->symbol)))
	   (token-case
	    (\` res)
	    (else (signal-missing-token "``'" "varop")))))
	(else (signal-missing-token "<varid>" "varop"))))
   (varsym? (token->symbol))))

(define (varop->ast)
  (let ((varop-name (varop->symbol)))
    (make var-ref (name varop-name) (infix? '#t) (var *undefined-def*))))

(define (conop->symbol)
  (token-case
   (\` (token-case
	(conid?
	 (let ((res (token->symbol/con)))
	   (token-case
	    (\` res)
	    (else (signal-missing-token "``'" "conop")))))
	(else (signal-missing-token "<conid>" "conop"))))
   (consym? (token->symbol/con))))

(define (conop->ast)
  (let ((conop-name (conop->symbol)))
    (make con-ref (name conop-name) (infix? '#t) (con *undefined-def*))))

(define (op->symbol)
  (token-case
   (\` (token-case
	(conid?
	 (let ((res (token->symbol/con)))
	   (token-case
	    (\` res)
	    (else (signal-missing-token "``'" "op")))))
	(varid?
	 (let ((res (token->symbol)))
	   (token-case
	    (\` res)
	    (else (signal-missing-token "``'" "op")))))
	(else (signal-missing-token "<conid> or <varid>" "op"))))
   (consym? (token->symbol/con))
   (varsym? (token->symbol))))

(define (con->ast)  ; for conid, aconid
  (let ((name (con->symbol)))
    (make con-ref (name name) (con *undefined-def*) (infix? '#f))))

(define (pcon->ast) ; for aconid, conid
  (let ((name (con->symbol)))
    (make pcon (name name) (con *undefined-def*) (pats '()) (infix? '#f))))

(define (pconop->ast) ; for aconop, conop
  (let ((name (conop->symbol)))
    (make pcon (name name) (con *undefined-def*) (pats '()) (infix? '#t))))

(define (tycon->ast) ; for aconid
  (let ((name (token->symbol)))
    (make tycon (name name) (def *undefined-def*) (args '()))))

(define (class->ast) ; for aconid
  (let ((name (token->symbol)))
    (make class-ref (name name) (class *undefined-def*))))

(define (tyvar->ast) ; for avarid
  (let ((name (token->symbol)))
    (make tyvar (name name))))

(define (token->integer) ; for integer
 (return+advance
  (car *token-args*)))

(define (integer->ast) ; for integer
 (return+advance
  (make integer-const (value (car *token-args*)))))

(define (float->ast)
 (return+advance
  (make float-const (numerator (car *token-args*))
	            (denominator (cadr *token-args*))
	            (exponent (caddr *token-args*)))))

(define (string->ast)
 (return+advance
  (make string-const (value (car *token-args*)))))

(define (char->ast)
 (return+advance
  (make char-const (value (car *token-args*)))))

(define (literal->ast)
  (token-case
    ((no-advance integer) (integer->ast))
    ((no-advance float) (float->ast))
    ((no-advance string) (string->ast))
    ((no-advance char) (char->ast))))
