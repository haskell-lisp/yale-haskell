;;; flic-walker.scm -- macros for defining code walkers for flic
;;;
;;; author :  Sandra Loosemore
;;; date   :  7 May 1992
;;;


;;; (define-flic-walker foo (object))
;;; creates a macro (define-foo type (object) . body)
;;; and a function (foo object) that dispatches on the type of object.

(define-syntax (define-flic-walker name args)
  (let ((accessor-name (symbol-append 'flic-td- name '-walker))
	(definer-name  (symbol-append 'define- name)))
    `(begin
       (define-walker ,name ,accessor-name)
       (define-local-syntax (,definer-name type args . body)
	 `(define-walker-method ,',name ,type ,args ,@body))
       (define (,name ,@args)
	 (call-walker ,name ,@args)))))

