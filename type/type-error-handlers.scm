;;; This file contains error handlers for the type checker.

(define (type-error msg . args)
  (apply (function phase-error) `(type-error ,msg ,@args))
  (report-non-local-type-error)
  (continue-from-type-error))

(define (report-non-local-type-error)
  (when (pair? (dynamic *type-error-handlers*))
     (funcall (car (dynamic *type-error-handlers*)))))

(define (continue-from-type-error)
  (funcall (car (dynamic *type-error-recovery*))))

(define (type-mismatch/fixed object msg type)
  (format '#t "While typing ~A:~%~A~%Type: ~A~%" object msg type))

(define (type-mismatch object msg type1 type2)
  (format '#t "While type checking~%~A~%~A~%Types: ~A~%       ~A~%"
	  object msg type1 type2))

(define (type-mismatch/list types object msg)
  (format '#t "While typing ~A:~%~A~%Types: ~%" object msg)
  (dolist (type types)
     (format '#t "~A~%" type)))

;;; Error handlers

(define (signature-mismatch var)
  (format '#t
      "Signature mismatch for ~A~%Inferred type: ~A~%Declared type: ~A~%"
      var
      (remove-type-wrapper (ntype->gtype (var-type var)))
      (var-signature var)))

(define (remove-type-wrapper ty)
  (if (recursive-type? ty) (recursive-type-type ty) ty))


	