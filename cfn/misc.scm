;;; misc.scm -- random other transformations done during CFN processing
;;;
;;; author :  Sandra Loosemore
;;; date   :  27 Feb 1992
;;;
;;; This file contains specialized CFN walkers that implement rewrite rules
;;; for list-exp, sequence-xxx, list-comp, section-l, and section-r.


;;; Turn list-exps into cons chains.

(define-walker-method cfn list-exp (object)
  (do-cfn-list-exp (list-exp-exps object)))

(define (do-cfn-list-exp exps)
  (if (null? exps)
      ;; Make a con-ref for []
      (**con/def (core-symbol "Nil"))
      ;; Otherwise make an app of :
      (let ((first  (cfn-ast-1 (car exps)))
	    (rest   (do-cfn-list-exp (cdr exps))))
	(**app (**con/def (core-symbol ":")) first rest))))
	

;;; Sections get turned into lambda expressions.

(define-walker-method cfn section-l (object)
  (let ((def   (create-temp-var 'section-arg)))
    (**lambda/pat
      (list (**var-pat/def def))
      (**app (cfn-ast-1 (section-l-op object))
             (**var/def def)
	     (cfn-ast-1 (section-l-exp object))))))

(define-walker-method cfn section-r (object)
  (let ((def   (create-temp-var 'section-arg)))
    (**lambda/pat
      (list (**var-pat/def def))
      (**app (cfn-ast-1 (section-r-op object))
	     (cfn-ast-1 (section-r-exp object))
             (**var/def def)))))



;;; Do list comprehensions.
;;; rewrite in terms of build and foldr so that we can apply
;;; deforestation techniques later.

(define-walker-method cfn list-comp (object)
  (let ((c   (create-temp-var 'c))
	(n   (create-temp-var 'n)))
    (cfn-ast-1
      (**app (**var/def (core-symbol "build"))
	     (**lambda/pat
	       (list (**var-pat/def c) (**var-pat/def n))
	       (do-cfn-list-comp
		 (list-comp-exp object) (list-comp-quals object) c n))))))

(define (do-cfn-list-comp exp quals c n)
  (if (null? quals)
      (**app (**var/def c) exp (**var/def n))
      (let ((qual  (car quals)))
	(if (is-type? 'qual-generator qual)
	    (do-cfn-list-comp-generator exp qual (cdr quals) c n)
	    (do-cfn-list-comp-filter exp qual (cdr quals) c n)))))

(define (do-cfn-list-comp-filter exp qual quals c n)
  (**if (qual-filter-exp qual)
	(do-cfn-list-comp exp quals c n)
	(**var/def n)))

(define (do-cfn-list-comp-generator exp qual quals c n)
  (let ((gen-pat  (qual-generator-pat qual))
	(gen-exp  (qual-generator-exp qual))
	(l        (create-temp-var 'list))
	(b        (create-temp-var 'rest)))
    (**app (**var/def (core-symbol "foldr"))
	   (**lambda/pat
	     (list (**var-pat/def l) (**var-pat/def b))
	     (**case (**var/def l)
		     (list (**alt/simple
			    gen-pat
			    (do-cfn-list-comp exp quals c b))
			   (**alt/simple
			    (**wildcard-pat)
			    (**var/def b)))))
	   (**var/def n)
	   gen-exp)))

;;; Placeholders just get eliminated

(define-walker-method cfn dict-placeholder (object)
  (if (eq? (dict-placeholder-exp object) '#f)
      (error "Type checker screwed a dict placeholder object ~s." object)
      (cfn-ast-1 (dict-placeholder-exp object))))

(define-walker-method cfn method-placeholder (object)
  (if (eq? (method-placeholder-exp object) '#f)
      (error "Type checker screwed a method placeholder object ~s." object)
      (cfn-ast-1 (method-placeholder-exp object))))

(define-walker-method cfn recursive-placeholder (object)
  (if (eq? (recursive-placeholder-exp object) '#f)
      (error "Type checker screwed a recursive placeholder object ~s." object)
      (cfn-ast-1 (recursive-placeholder-exp object))))

(define-walker-method cfn cast (object)
  (cfn-ast-1 (cast-exp object)))

;;; Eliminate saved old expressions

(define-walker-method cfn save-old-exp (object)
  (cfn-ast-1 (save-old-exp-new-exp object)))
