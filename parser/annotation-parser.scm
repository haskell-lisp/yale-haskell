
(define *annotation-escape* '())

(define (parse-annotations)
 (let ((save-layout (dynamic *layout-stack*)))
  (setf (dynamic *layout-stack*) '())
  (advance-token)
  (let/cc annotation-escape
   (setf *annotation-escape* (lambda () 
			       (setf (dynamic *layout-stack*) save-layout)
			       (advance-to-annotation-end)
			       (funcall annotation-escape '())))
   (let ((res (start-layout (function parse-annotation-list-1))))
    (setf (dynamic *layout-stack*) save-layout)
    (token-case
     (end-annotation res)
     (else (signal-annotation-error)))))))

(define (parse-annotation-list-1 in-layout?)
  (let ((kind (get-annotation-kind)))
    (cond ((eq? kind 'decl)
	   (let ((d (parse-annotation-decl)))
	     (token-case
	      (\; (cons d (parse-annotation-list-1 in-layout?)))
	      (else (close-layout in-layout?)
		    (list d)))))
	  ((eq? kind 'value)
	   (let ((d (parse-annotation-value)))
	     (token-case
	      (\; (cons d (parse-annotation-list-1 in-layout?)))
	      (else (close-layout in-layout?)
		    (list d)))))
	  (else
	   (close-layout in-layout?)
	   '()))))

(define (get-annotation-kind)
  (token-case
   ((no-advance end-annotation) 'end)
   ((no-advance \() 'decl)
   ((var con)
    (let ((next (peek-1-type)))
      (cond ((eq? next '|,|)
	     'decl)
	    ((eq? next '|::|)
	     'decl)
	    (else
	     'value))))
   (else 'error)))

(define (parse-annotation-decl)
  (let* ((names (parse-aname-list))
	 (decls (parse-aval-list)))
    (make annotation-decl (names names) (annotations decls))))

(define (parse-aname-list)
 (let ((name 'foo))
  (token-case
   (var
    (setf name (var->symbol)))
   (con
    (setf name (con->symbol)))
   (else (signal-annotation-error)))
  (token-case (\, (cons name (parse-aname-list)))
	      (|::| (list name))
	      (else (signal-annotation-error)))))


(define (parse-aval-list)
  (let ((ann (parse-annotation-value)))
    (token-case (\, (cons ann (parse-aval-list)))
		(else (list ann)))))

(define (parse-annotation-value)
  (token-case
   (name (let* ((name (token->symbol))
		(args (parse-annotation-args name)))
	   (make annotation-value (name name) (args args))))))

(define (parse-annotation-args name)
  (token-case
   (\( (parse-annotation-args-1 name 0))
   (else '())))

;;; This routine can invoke special parsers for the arguments

(define (parse-annotation-args-1 name i)
  (let* ((argtype (get-annotation-arg-description name i))
	 (arg (parse-annotation-arg argtype)))
    (token-case
     (\) (list arg))
     (\, (cons arg (parse-annotation-args-1 name (1+ i))))
     (else (signal-annotation-error)))))

(define (parse-annotation-arg type)
  (cond ((eq? type 'string)
	 (token-case
	  ((string no-advance)
	   (let ((res (car *token-args*)))
	     (advance-token)
	     res))
	  (else (signal-annotation-error))))
	;; The following is for a datatype import/export.  It is
	;; Type(Con1(strs),Con2(strs),...)
	((eq? type 'integer)
	 (token-case
	  ((integer no-advance) (token->integer))
	  (else (signal-annotation-error))))
	((eq? type 'constr-list)
	 (parse-annotation-constr-list))
	(else
	 (signal-annotation-error))))
	   
(define (signal-annotation-error)
  (parser-error/recoverable 'annotation-error "Error in annotation syntax")
  (funcall *annotation-escape*))

(define (parse-annotation-constr-list)
  (token-case
   (tycon (let ((type-name (token->symbol)))
	    (token-case (\( (let* ((args (parse-acl1))
				   (res (tuple type-name args)))
			      (token-case  ; leave the ) to end the args
			       ((no-advance \)) (list res))
			       (\, (cons res (parse-annotation-constr-list)))
			       (else (signal-annotation-error)))))
			(else (signal-annotation-error)))))
   (else (signal-annotation-error))))

(define (parse-acl1)
  (token-case
   (con (let ((con-name (con->symbol)))
	  (token-case (\( (let ((str-args (parse-string-list)))
			    (token-case
			     (\, (cons (tuple con-name str-args)
				       (parse-acl1)))
			     (\) (list (tuple con-name str-args)))
			     (else (signal-annotation-error)))))
		      (else (signal-annotation-error)))))
   (else (signal-annotation-error))))

(define (parse-string-list)
  (token-case
   ((string no-advance)
    (let ((res (read-lisp-object (car *token-args*))))
      (advance-token)
      (token-case
       (\) (list res))
       (\, (cons res (parse-string-list)))
       (else (signal-annotation-error)))))
   (else (signal-annotation-error))))

(define (advance-to-annotation-end)
  (token-case
   (eof '())
   (end-annotation
     (advance-token))
   (else
    (advance-token)
    (advance-to-annotation-end))))
  
(define *known-annotations* '(
  (|LispName| string)
  (|Prelude|)
  (|Strictness| string)
  (|Strict|)
  (|NoConversion|)
  (|Inline|)
  (|STRICT|)
  (|ImportLispType| constr-list)
  (|ExportLispType| constr-list)
  (|Complexity| integer)
  ))

(define (get-annotation-arg-description annotation i)
  (let ((s (assq annotation *known-annotations*)))
    (cond ((eq? s '#f)
	   (parser-error/recoverable 'unknown-annotation
             "Annotation ~A is not defined in this system - ignored."
	     annotation)
	   'unknown)
	  ((>= i (length s))
	   'error)
	  (else (list-ref s (1+ i))))))
