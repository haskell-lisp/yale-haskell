(in-package "LISP")


;;; The default version of this function has a bug with relative
;;; pathnames.

(defun pathname->string (p)
  (let ((dirlist (pathname-directory p)))
    (format nil "~A~{~A/~}~A~A~A"
            (case (car dirlist)
              (:absolute "/")
              (:relative "./")
              (:up "../")
              (t ""))
            (cdr dirlist)
            (nil->empty-string (pathname-name p))
            (if (null (pathname-type p)) "" ".")
            (nil->empty-string (pathname-type p)))))


;;; The default version of this function defaults the C file to the
;;; wrong directory -- LOAD can't find it.

(defun my-comf (file &key
                  (output-file (merge-pathnames ".o" file))
                  (c-file (merge-pathnames ".c" output-file))
                  (verbose *compile-verbose*)
                  (print *compile-print*)
                  (config *config*)
                  (pic? *pic?*)
                  only-to-c?)
  (old-comf file
	    :output-file output-file
	    :c-file c-file
	    :verbose verbose
	    :print print
	    :config config
	    :pic? pic?
	    :only-to-c? only-to-c?))

(when (not (fboundp 'old-comf))
  (setf (symbol-function 'old-comf) #'comf)
  (setf (symbol-function 'comf) #'my-comf))


;;; WCL's evaluator tries to macroexpand everything before executing
;;; anything.  Unfortunately, this does the wrong thing with
;;; top-level PROGN's -- it tries to expand macros in subforms before
;;; executing earlier subforms that set up stuff required to do the
;;; the expansion properly.

(defun eval-1 (form venv fenv tenv benv)
  (let ((new-form  (macroexpand form *eval-macro-env*)))
    (if (and (consp new-form)
	     (eq (car new-form) 'progn))
	(do ((forms (cdr new-form) (cdr forms)))
	    ((null (cdr forms)) (eval-1 (car forms) venv fenv tenv benv))
	    (eval-1 (car forms) venv fenv tenv benv))
	(let ((expansion (expand new-form)))
	  (when (and (listp expansion)
		     (eq (car expansion) 'define-function))
	    (setf (get (second (second expansion))
		       :function-definition)
		  form))
	  (eval/5 expansion venv fenv tenv benv))
      )))


