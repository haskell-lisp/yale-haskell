
;;; This is the top-level phase structure of the compiler.

;;; Compilation phase support

(define *phase* '#f)
(define *abort-phase* '#f)         ; abort when this phase completes
(define *abort-compilation*
  (lambda ()
    (error "No error continuation defined here!")))

(define *module-asts* '())   ; a global only for debugging purposes

;;; Later add the printing and timing stuff here

(define-local-syntax (phase-body phase-name body printer)
  `(dynamic-let ((*phase*       ',phase-name))
     (when (memq ',phase-name (dynamic *printers*))
       (format '#t "~%Phase ~a:~%" ',phase-name)
       (force-output))
     (let* ((phase-start-time (get-run-time))
	    (result ,body)
	    (current-time  (get-run-time)))
       (when (eq? (dynamic *abort-phase*) ',phase-name)
	 (abort-compilation))
       ,@(if (eq? printer '#f)
	     '()
	     `((when (memq ',phase-name (dynamic *printers*))
		 (funcall ,printer result)
		 (force-output))))
       (when (memq 'phase-time *printers*)
	 (let ((elapsed-time (- current-time phase-start-time)))
	   (format '#t "~&~A complete: ~A seconds~%"
		   ',phase-name elapsed-time)
	   (force-output)))
       result)))



;;; Returns 2 values: module ast's and lisp code.

(define (compile-haskell-files files)
  (dynamic-let ((*abort-phase*                '#f))
     (let ((all-mods       (haskell-parse-files files))
	   (interface-mods '())
	   (regular-mods   '()))
       (dolist (m all-mods)
	 (if (eq? (module-type m) 'interface)
	     (push m interface-mods)
	     (push m regular-mods)))
       (dynamic-let ((*unit*  (module-name (car all-mods))))
	 (values
	   all-mods
	   `(begin
	      ,(if interface-mods
		   (compile-interface-modules (nreverse interface-mods))
		   '#f)
	      ,(if regular-mods
		   (compile-modules (nreverse regular-mods))
		   '#f))
	   )))))



(define (compile-modules mods)
  (dynamic-let ((*context*                    '#f)
		(*recoverable-error-handler*  '#f)
		(*abort-phase*                '#f)
		(*unique-name-counter*        1)
		(*suffix-table*               (make-table)))
  	  (haskell-import-export mods '#f)
	  (haskell-process-type-declarations mods)
	  (haskell-scope mods)
	  (let ((big-let (haskell-dependency-analysis mods)))
	    (cond ((not (void? big-let))
		   (haskell-type-check big-let mods)
		   (setf big-let (haskell-cfn big-let))
		   (setf big-let (haskell-dependency-reanalysis big-let))
		   (setf big-let (haskell-ast-to-flic big-let))
		   (setf big-let (haskell-optimize big-let))
		   (setf big-let (haskell-strictness big-let))
		   (haskell-codegen big-let mods))
		  (else
		   ''#f)
		  ))))


(define (modules->lisp-code modules)
  (dynamic-let ((*unit* (module-name (car modules))))
    (compile-modules modules)))


(predefine (notify-error))  ; in command-interface/command-utils.scm

(define (abort-compilation)
  (notify-error)
  (funcall (dynamic *abort-compilation*)))

(define (halt-compilation)
  (setf (dynamic *abort-phase*) (dynamic *phase*)))


;;; Here are the actual phase bodies

(predefine (parse-files files))

(define (haskell-parse-files filenames)
  (phase-body parse
    (let ((mods (parse-files filenames)))
      mods)
    #f))

(predefine (import-export modules))  ; in import-export/import-export.scm
(predefine (import-export/interface modules))

(define (haskell-import-export modules interface?)
  (phase-body import
    (if interface?
	(import-export/interface modules)
	(import-export modules))
    #f))


(predefine (process-type-declarations modules)) 
    ; in tdecl/type-declaration-analysis.scm

(define (haskell-process-type-declarations modules)
  (phase-body type-decl
    (begin
      (process-type-declarations modules))
    #f))


(predefine (scope-modules x))  ; in prec/scope.scm
(predefine (print-full-module x . maybe-stream)) ; in the printers

(define (haskell-scope modules)
  (phase-body scope
    (scope-modules modules)
    (lambda (result)
      (declare (ignore result))
      (dolist (m modules) (print-full-module m)))
    ))


(predefine (do-dependency-analysis x))  ; in depend/dependency-analysis.scm

(define (haskell-dependency-analysis modules)
  (phase-body depend
    (do-dependency-analysis modules)
    (function pprint*)))


(predefine (do-haskell-type-check big-let mods))

(define (haskell-type-check big-let modules)
  (phase-body type
    (do-haskell-type-check big-let modules)
    #f))

(predefine (cfn-ast x))  ; in cfn/main.scm

(define (haskell-cfn big-let)
  (phase-body cfn
    (cfn-ast big-let)
    (function pprint*)))


(predefine (analyze-dependency-top x))  ; in depend/dependency-analysis.scm

(define (haskell-dependency-reanalysis big-let)
  (phase-body depend2
    (begin
      (analyze-dependency-top big-let)
      big-let)
    (function pprint*)))


(predefine (ast-to-flic x))		; in flic/ast-to-flic.scm

(define (haskell-ast-to-flic big-let)
  (phase-body flic
    (ast-to-flic big-let)
    (function pprint*)))


(predefine (optimize-top x))  ; in backend/optimize.scm

(define (haskell-optimize big-let)
  (phase-body optimize
    (optimize-top big-let)
    (function pprint*)))

(predefine (strictness-analysis-top x)) ; in backend/strictness.scm
(predefine (strictness-analysis-printer x))

(define (haskell-strictness big-let)
  (phase-body strictness
    (strictness-analysis-top big-let)
    (function strictness-analysis-printer)))


(predefine (codegen-top x))  ; in backend/codegen.scm
(predefine (codegen-exported-types x)) ; "
(predefine (codegen-prim-entries x))  ; ditto

(define (haskell-codegen big-let mods)
  (phase-body codegen
    `(begin
       ,(codegen-exported-types mods)
       ,(codegen-top big-let))
    #f))

	       
;;; This is for interface modules.

(predefine (haskell-codegen/interface mods))

(define (compile-interface-modules mods)
 (dynamic-let ((*context*                    '#f)
	       (*recoverable-error-handler*  '#f)
	       (*abort-phase*                '#f))
     (haskell-import-export mods '#t)
     (haskell-process-type-declarations mods)
     (haskell-scope mods)
     (haskell-codegen/interface mods)))
