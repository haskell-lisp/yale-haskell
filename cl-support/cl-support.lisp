;;; cl-support.lisp -- compile-time support for building mumble
;;;
;;; author :  Sandra Loosemore
;;; date   :  10 Oct 1991
;;;
;;; This file must be loaded before compiling the cl-definitions file.
;;; However, it is not needed when loading the compiled file.

(in-package "MUMBLE-IMPLEMENTATION")


;;; Use this macro for defining an exported mumble function.

(defmacro define-mumble-function (name &rest stuff)
  `(progn
     (eval-when (eval compile load) (export (list ',name) "MUMBLE"))
     (defun ,name ,@stuff)))


;;; This is similar, but also does some stuff to try to inline the
;;; function definition.  

(defmacro define-mumble-function-inline (name &rest stuff)
  `(progn
     (eval-when (eval compile load) (export (list ',name) "MUMBLE"))
#+lcl
     (lcl:defsubst ,name ,@stuff)
#-lcl
     (progn
       (proclaim '(inline ,name))
       (defun ,name ,@stuff))
     ',name))


;;; Use this macro for defining an exported mumble macro.

(defmacro define-mumble-macro (name &rest stuff)
  `(progn
     (eval-when (eval compile load) (export (list ',name) "MUMBLE"))
     (defmacro ,name ,@stuff)))


;;; Use this macro for importing a random symbol into the MUMBLE
;;; package.  This is useful for things that can share directly with
;;; built-in Common Lisp definitions.

(defmacro define-mumble-import (name)
  `(progn
     (eval-when (eval compile load) (import (list ',name) "MUMBLE"))
     (eval-when (eval compile load) (export (list ',name) "MUMBLE"))
     ',name))


;;; Use this macro for defining a function in the MUMBLE package that
;;; is a synonym for some Common Lisp function.  Try to do some stuff
;;; to make the function compile inline.

(defmacro define-mumble-synonym (name cl-name)
  `(progn
     (eval-when (eval compile load) (export (list ',name) "MUMBLE"))
     (setf (symbol-function ',name) (symbol-function ',cl-name))
#+lcl
     (lcl:def-compiler-macro ,name (&rest args)
       (cons ',cl-name args))
     ',name))



;;; Use this macro to define a type synonym.

(defmacro define-mumble-type (name &rest stuff)
  `(progn
     (eval-when (eval compile load) (export (list ',name) "MUMBLE"))
     (deftype ,name ,@stuff)))


;;; This macro is used to signal a compile-time error in situations
;;; where an implementation-specific definition is missing.

(defmacro missing-mumble-definition (name)
  (error "No definition has been provided for ~s." name))





