;;; print-flic.scm -- printers for FLIC structures
;;;
;;; author :  Sandra Loosemore
;;; date   :  30 Mar 1992
;;;
;;;


;;; For now, printing of FLIC structures is controlled by the same
;;; *print-ast-syntax* variable as for AST structures.
;;; Maybe eventually this should use its own variable.

(define-syntax (define-flic-printer type lambda-list . body)
  `(define-ast-printer ,type ,lambda-list ,@body))

(define-flic-printer flic-lambda (object xp)
  (with-ast-block (xp)
    (write-string "\\ " xp)
    (write-ordinary-list (flic-lambda-vars object) xp)
    (write-string " ->" xp)
    (write-whitespace xp)
    (write (flic-lambda-body object) xp)))

(define-flic-printer flic-let (object xp)
  (pprint-logical-block (xp '() "" "")  ; no extra indentation
    (write-string "let " xp)
    (write-layout-rule (flic-let-bindings object) xp
		       (lambda (v xp)
		         (with-ast-block (xp)
		           (write v xp)
			   (write-string " =" xp)
			   (write-whitespace xp)
			   (write (var-value v) xp))))
    (write-whitespace xp)
    (write-string "in " xp)
    (write (flic-let-body object) xp)))

(define-flic-printer flic-app (object xp)
  (with-ast-block (xp)
    (maybe-paren-flic-object (flic-app-fn object) xp)
    (write-whitespace xp)
    (write-flic-list (flic-app-args object) xp)))

(define (maybe-paren-flic-object object xp)
  (cond ((or (flic-ref? object)
	     (flic-const? object)
	     (flic-pack? object))
	 (write object xp))
	(else
	 (write-char #\( xp)
	 (write object xp)
	 (write-char #\) xp))))

(define (write-flic-list objects xp)
  (write-delimited-list objects xp (function maybe-paren-flic-object) "" "" ""))

(define-flic-printer flic-ref (object xp)
  (write (flic-ref-var object) xp))

(define-flic-printer flic-const (object xp)
  (write (flic-const-value object) xp))

(define-flic-printer flic-pack (object xp)
  (write-string "pack/" xp)
  (write (flic-pack-con object) xp))

(define-flic-printer flic-case-block (object xp)
  (with-ast-block (xp)
    (write-string "case-block " xp)
    (write (flic-case-block-block-name object) xp)
    (write-whitespace xp)
    (write-layout-rule (flic-case-block-exps object) xp (function write))))

(define-flic-printer flic-return-from (object xp)
  (with-ast-block (xp)
    (write-string "return-from " xp)
    (write (flic-return-from-block-name object) xp)
    (write-whitespace xp)
    (write (flic-return-from-exp object) xp)))

(define-flic-printer flic-and (object xp)
  (with-ast-block (xp)
    (write-string "and " xp)
    (write-layout-rule (flic-and-exps object) xp (function write))))

(define-flic-printer flic-if (object xp)
  (with-ast-block (xp)
    (write-string "if " xp)
    (write (flic-if-test-exp object) xp)
    (write-whitespace xp)
    (with-ast-block (xp)
      (write-string "then" xp)
      (write-whitespace xp)
      (write (flic-if-then-exp object) xp))
    (write-whitespace xp)
    (with-ast-block (xp)
      (write-string "else" xp)
      (write-whitespace xp)
      (write (flic-if-else-exp object) xp))
    ))


(define-flic-printer flic-sel (object xp)
  (with-ast-block (xp)
    (write-string "sel/" xp)
    (write (flic-sel-con object) xp)
    (write-char #\/ xp)
    (write (flic-sel-i object) xp)
    (write-whitespace xp)
    (write (flic-sel-exp object) xp)))

(define-flic-printer flic-is-constructor (object xp)
  (with-ast-block (xp)
    (write-string "is-constructor/" xp)
    (write (flic-is-constructor-con object) xp)
    (write-whitespace xp)
    (write (flic-is-constructor-exp object) xp)))

(define-flic-printer flic-con-number (object xp)
  (with-ast-block (xp)
    (write-string "con/" xp)
    (write (flic-con-number-type object) xp)
    (write-whitespace xp)
    (write (flic-con-number-exp object) xp)))

(define-flic-printer flic-void (object xp)
  (declare (ignore object))
  (write-string "Void" xp))

  