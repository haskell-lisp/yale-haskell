;;; box.scm -- determine which expressions need to be boxed
;;;
;;; author  :  Sandra Loosemore
;;; date    :  03 Apr 1993
;;;
;;; 
;;; This phase determines whether expressions need to be boxed or unboxed.
;;;
;;; In the case of an expression that needs to be boxed, it determines 
;;; whether it can be evaluated eagerly and boxed or whether a delay
;;; must be constructed.
;;;
;;; In the case of an expression that needs to be unboxed, it determines
;;; whether it is already known to have been evaluated and can simply
;;; be unboxed instead of checking for a delay that must be forced.
;;;
;;; This phase may mark previously non-strict variables as strict if their
;;; initializers can be evaluated eagerly.  However, doing this evaluation
;;; eagerly never causes any other non-strict variables to be forced,
;;; so there is no need to propagate this strictness information backwards
;;; (as happens in the var-strictness-walk pass).


;;;======================================================================
;;; Top-level function
;;;======================================================================


;;; Complexity computation

(define-integrable delay-complexity 10)
(define-integrable unbox-complexity 1)
(define-integrable box-complexity 2)
(define-integrable sel-complexity 1)
(define-integrable is-constructor-complexity 1)
(define-integrable pack-complexity 2)
(define-integrable con-number-complexity 1)

(define (add-complexity c1 c2)
  (cond ((not c1)
	 '#f)
	((not c2)
	 '#f)
	(else
	 ;; *** We might want to establish an arbitrary cutoff here.
	 ;; *** e.g., if complexity > N then set it to '#f.
	 (the fixnum (+ (the fixnum c1) (the fixnum c2))))))



;;; The second argument to the walker is a list of things
;;; that are known to have been forced already.
;;; The third argument is a list of variables that have not yet
;;; been initialized.
;;; Walkers return two values:  a new value for already-forced and
;;; the complexity of the expression.

;;; This helper function sets the unboxed? and cheap? bits for the
;;; code generator, and adjusts the basic complexity to account for
;;; forces, boxes, and delays.
;;;
;;; The basic decision tree for the code generator should be:
;;; if unboxed?
;;;    then if strict-result?
;;;            then generate x                                   (1)
;;;            else if cheap?
;;;                    then generate (unbox x)                   (2)
;;;                    else generate (force x)                   (3)
;;;    else if strict-result?
;;;            then if cheap?
;;;                    then generate (box x)                     (4)
;;;                    else generate (delay x)                   (5)
;;;            else if cheap?
;;;                    then generate x                           (6)
;;;                    then generate (delay (force x))           (7)
;;; See function do-codegen in codegen.scm.


(define (do-box-analysis object already-forced uninitialized unboxed?)
  (setf (flic-exp-unboxed? object) unboxed?)
  (multiple-value-bind (result complexity)
      (box-analysis object already-forced uninitialized)
    (setf complexity
	  (if unboxed?
	      ;; If the expression returns a boxed value and we want
	      ;; an unboxed one, we may need to do a force.
	      (if (flic-exp-strict-result? object)
		  (begin                                       ; case (1)
		    ;; this flic-exp-cheap? bit is used only by
		    ;; exp-would-be-cheap? below -- not by codegen
		    (setf (flic-exp-cheap? object)
			  (if complexity '#t '#f))
		    complexity)      
		  (if (already-forced? object already-forced)
		      (begin                                   ; case (2)
			(setf (flic-exp-cheap? object) '#t)
			(add-complexity complexity unbox-complexity))
		      (begin                                   ; case (3)
			(setf (flic-exp-cheap? object) '#f)
			'#f)))
	      ;; We want a boxed value.  If the expression already
	      ;; returns a boxed value, return its complexity directly;
	      ;; otherwise return the cost of either boxing or delaying it.
	      (if (flic-exp-strict-result? object)
		  (if complexity
		      (begin                                   ; case (4)
			(setf (flic-exp-cheap? object) '#t)
			(add-complexity complexity box-complexity))
		      (begin                                   ; case (5)
			(setf (flic-exp-cheap? object) '#f)
			delay-complexity))
		  (if complexity
		      (begin
			(setf (flic-exp-cheap? object) '#t)    ; case (6)
			complexity)
		      (begin                                   ; case (7)
		        (setf (flic-exp-cheap? object) '#f)
			delay-complexity)))
	    ))
    (values
      (if unboxed?
	  (note-already-forced object result)
	  result)
      complexity)))




;;;======================================================================
;;; Code walk
;;;======================================================================


(define *local-function-calls* '())

(define-flic-walker box-analysis (object already-forced uninitialized))

(define-box-analysis flic-lambda (object already-forced uninitialized)
  (do-box-analysis (flic-lambda-body object) already-forced uninitialized '#t)
  (values already-forced 0))

(define-box-analysis flic-let (object already-forced uninitialized)
  (let ((bindings    (flic-let-bindings object)))
    (dynamic-let ((*local-function-calls*  (dynamic *local-function-calls*)))
      (dolist (var bindings)
	;; Note local functions
	(when (and (not (var-toplevel? var))
		   (is-type? 'flic-lambda (var-value var))
		   (not (var-standard-refs? var)))
	  (push (cons var '()) (dynamic *local-function-calls*))))
      (multiple-value-bind (already-forced complexity)
	  (box-analysis-let-aux object already-forced uninitialized)
	(dolist (var bindings)
	  ;; Go back and reexamine local functions to see whether
	  ;; we can make more arguments strict, based on the values
	  ;; the function is actually called with.
	  (let ((stuff  (assq var (dynamic *local-function-calls*))))
	    (when stuff
	      (maybe-make-more-arguments-strict var (cdr stuff)))))
	(values already-forced complexity)))))

(define (box-analysis-let-aux object already-forced uninitialized)
  (let ((recursive?  (flic-let-recursive? object))
	(bindings    (flic-let-bindings object))
	(body        (flic-let-body object)))
    (when recursive? (setf uninitialized (append bindings uninitialized)))
    (dolist (var bindings)
      (let* ((value   (var-value var))
	     (strict? (var-strict? var))
	     (result  (do-box-analysis value already-forced uninitialized
				       strict?)))
	(cond (strict?
	       ;; Propagate information about things forced.
	       (setf already-forced result))
	      ((and (flic-exp-cheap? value)
		    (flic-exp-strict-result? value))
	       ;; The value expression is cheap unboxed value, so mark
	       ;; the variable as strict.
	       (setf (var-strict? var) '#t)
	       (setf (flic-exp-unboxed? value) '#t))))
      (when recursive? (pop uninitialized)))
    ;; *** Could be smarter about computing complexity.
    (values
      (do-box-analysis body already-forced uninitialized '#t)
      '#f)))

(define (maybe-make-more-arguments-strict var calls)
  (setf (var-strictness var)
	(maybe-make-more-arguments-strict-aux
	  (flic-lambda-vars (var-value var))
	  calls)))

(define (maybe-make-more-arguments-strict-aux vars calls)
  (if (null? vars)
      '()
      (let ((var  (car vars)))
	;; If the variable is not already strict, check to see
	;; whether it's always called with "cheap" arguments.
	(when (and (not (var-strict? var))
		   (every-1 (lambda (call)
			      (exp-would-be-cheap? (car call) var))
			    calls))
	  (setf (var-strict? var) '#t)
	  (dolist (call calls)
	    (setf (flic-exp-unboxed? (car call)) '#t)))
	(cons (var-strict? var)
	      (maybe-make-more-arguments-strict-aux
	       (cdr vars)
	       (map (function cdr) calls))))
    ))


;;; Look for one special fixed-point case: argument used as counter-type
;;; variable.  Otherwise ignore fixed points.

(define (exp-would-be-cheap? exp var)
  (or (and (flic-exp-cheap? exp)
	   (flic-exp-strict-result? exp))
      (and (is-type? 'flic-ref exp)
	   (eq? (flic-ref-var exp) var))
      (and (is-type? 'flic-app exp)
	   (is-type? 'flic-ref (flic-app-fn exp))
	   (var-complexity (flic-ref-var (flic-app-fn exp)))
	   (every-1 (lambda (a) (exp-would-be-cheap? a var))
		    (flic-app-args exp)))
      ))



(define-box-analysis flic-app (object already-forced uninitialized)
  (let ((fn         (flic-app-fn object))
	(args       (flic-app-args object))
	(saturated? (flic-app-saturated? object)))
    (cond ((and saturated? (is-type? 'flic-ref fn))
	   (let* ((var    (flic-ref-var fn))
		  (stuff  (assq var (dynamic *local-function-calls*))))
	     (when stuff
	       (push args (cdr stuff)))
	     (box-analysis-app-aux
	       (var-strictness var) (var-complexity var)
	       args already-forced uninitialized)))
	  ((and saturated? (is-type? 'flic-pack fn))
	   (box-analysis-app-aux
	     (con-slot-strict? (flic-pack-con fn)) pack-complexity
	     args already-forced uninitialized))
	  (else
	   ;; The function is going to be forced but all the arguments
	   ;; are non-strict.
	   (dolist (a args)
	     (do-box-analysis a already-forced uninitialized '#f))
	   (values 
	     (do-box-analysis fn already-forced uninitialized '#t)
	     '#f))
	  )))
	  


;;; Propagation of already-forced information depends on whether or
;;; not the implementation evaluates function arguments in left-to-right
;;; order.  If not, we can still propagate this information upwards.

(define (box-analysis-app-aux
	   strictness complexity args already-forced uninitialized)
  (let ((result   already-forced))
    (dolist (a args)
      (let ((strict?  (pop strictness)))
	(multiple-value-bind (new-result new-complexity)
	    (do-box-analysis a already-forced uninitialized strict?)
	  (when strict?
	    (setf result
		  (if left-to-right-evaluation
		      (setf already-forced new-result)
		      (union-already-forced
		        new-result already-forced result))))
	  (setf complexity (add-complexity complexity new-complexity)))))
    (values result complexity)))


(define-box-analysis flic-ref (object already-forced uninitialized)
  (values
    already-forced
    (if (memq (flic-ref-var object) uninitialized)
	'#f
	0)))

(define-box-analysis flic-const (object already-forced uninitialized)
  (declare (ignore object uninitialized))
  (values already-forced 0))

(define-box-analysis flic-pack (object already-forced uninitialized)
  (declare (ignore object uninitialized))
  (values already-forced 0))


;;; For case-block and and, already-forced information can be propagated 
;;; sequentially in the clauses.  But only the first expression is 
;;; guaranteed to be evaluated, so only it can propagate the information
;;; outwards.

(define-box-analysis flic-case-block (object already-forced uninitialized)
  (values
    (box-analysis-sequence
      (flic-case-block-exps object) already-forced uninitialized)
    '#f))

(define-box-analysis flic-and (object already-forced uninitialized)
  (values
    (box-analysis-sequence
      (flic-and-exps object) already-forced uninitialized)
    '#f))

(define (box-analysis-sequence exps already-forced uninitialized)
  (let ((result
	  (setf already-forced
		(do-box-analysis
		  (car exps) already-forced uninitialized '#t))))
    (dolist (e (cdr exps))
      (setf already-forced
	    (do-box-analysis e already-forced uninitialized '#t)))
    (values result already-forced)))


(define-box-analysis flic-return-from (object already-forced uninitialized)
  (values
    (do-box-analysis
      (flic-return-from-exp object) already-forced uninitialized '#t)
    '#f))


;;; For if, the test propagates to both branches and the result.
;;; Look for an important optimization:
;;; in (if (and e1 e2 ...) e-then e-else),
;;; e-then can inherit already-forced information from all of the ei
;;; rather than only from e1.
;;; *** Could be smarter about the complexity, I suppose....
;;; *** Also could intersect already-forced results from both
;;; *** branches.

(define-box-analysis flic-if (object already-forced uninitialized)
  (if (is-type? 'flic-and (flic-if-test-exp object))
      (box-analysis-if-and-aux object already-forced uninitialized)
      (box-analysis-if-other-aux object already-forced uninitialized)))

(define (box-analysis-if-other-aux object already-forced uninitialized)
  (setf already-forced
	(do-box-analysis
	  (flic-if-test-exp object) already-forced uninitialized '#t))
  (do-box-analysis (flic-if-then-exp object) already-forced uninitialized '#t)
  (do-box-analysis (flic-if-else-exp object) already-forced uninitialized '#t)
  (values already-forced '#f))

(define (box-analysis-if-and-aux object already-forced uninitialized)
  (let* ((test-exp  (flic-if-test-exp object))
	 (subexps   (flic-and-exps test-exp))
	 (then-exp  (flic-if-then-exp object))
	 (else-exp  (flic-if-else-exp object)))
    (setf (flic-exp-unboxed? test-exp) '#t)
    (multiple-value-bind (result1 resultn)
	(box-analysis-sequence subexps already-forced uninitialized)
      (do-box-analysis then-exp resultn uninitialized '#t)
      (do-box-analysis else-exp result1 uninitialized '#t)
      (values result1 '#f))))


(define-box-analysis flic-sel (object already-forced uninitialized)
  (multiple-value-bind (result complexity)
      (do-box-analysis
        (flic-sel-exp object) already-forced uninitialized '#t)
    (values result (add-complexity sel-complexity complexity))))

(define-box-analysis flic-is-constructor (object already-forced uninitialized)
  (multiple-value-bind (result complexity)
      (do-box-analysis
        (flic-is-constructor-exp object) already-forced uninitialized '#t)
    (values result (add-complexity is-constructor-complexity complexity))))

(define-box-analysis flic-con-number (object already-forced uninitialized)
  (multiple-value-bind (result complexity)
      (do-box-analysis
        (flic-con-number-exp object) already-forced uninitialized '#t)
    (values result (add-complexity con-number-complexity complexity))))

(define-box-analysis flic-void (object already-forced uninitialized)
  (declare (ignore object uninitialized))
  (values already-forced 0))




;;;======================================================================
;;; Already-forced bookkeeping
;;;======================================================================


;;; For now, we only keep track of variables that have been forced,
;;; and not data structure accesses.

(define (already-forced? object already-forced)
  (and (is-type? 'flic-ref object)
       (memq (flic-ref-var object) already-forced)))

(define (note-already-forced object already-forced)
  (if (is-type? 'flic-ref object)
      (cons (flic-ref-var object) already-forced)
      already-forced))

(define (union-already-forced new tail result)
  (cond ((eq? new tail)
	 result)
	((memq (car new) result)
	 (union-already-forced (cdr new) tail result))
	(else
	 (union-already-forced (cdr new) tail (cons (car new) result)))
	))

				      

