;;; cl-structs.lisp -- extended structure definitions
;;;
;;; author :  Sandra Loosemore
;;; date   :  19 Aug 1992
;;;


;;;====================================================================
;;; Basic structure types
;;;====================================================================


;;; Use this hash table for mapping names -> type descriptors

(defvar *struct-lookup-table* (make-hash-table :test #'eq))

(defmacro lookup-type (name)
  `(gethash ,name *struct-lookup-table*))


;;; Do NOT add or remove slots from these DEFSTRUCTS without also
;;; changing the bootstrap code below!!!
;;; Do NOT try to give these structs complicated defaulting behavior!!!

;;; All of our objects are subtypes of STRUCT.


(mumble::predefine (mumble::write object . maybe-stream))

(defun print-struct-object (object stream depth)
  (declare (ignore depth))
  (mumble::write object stream)
;  (format stream "#<Struct ~a>" (td-name (struct-type-descriptor object)))
  )


;;; Note that non-exported slots are prefixed with % to prevent
;;; accidental slot name collisions.

(defstruct (struct
	     (:print-function print-struct-object)
	     (:predicate      struct?)
	     (:constructor    nil)   ; never instantiated directly
	     (:copier         nil))
  (type-descriptor nil :type t)
  (%bits 0 :type fixnum)
  )


(defstruct (type-descriptor
	     (:include struct
		       (type-descriptor (lookup-type 'type-descriptor)))
	     (:conc-name td-)
	     (:constructor create-type-descriptor ())
	     (:predicate nil)
	     (:copier nil))
  (name nil :type symbol)
  (slots nil :type list)         ; all slots, including inherited
  (parent-type nil :type t)
  (printer nil :type t)
  (%local-slots nil :type list)   ; "real" structure slots
  (%bits-used 0 :type fixnum)
  (%constructor nil :type symbol)
  )

(defstruct (slot-descriptor
	     (:include struct
		       (type-descriptor (lookup-type 'slot-descriptor)))
	     (:conc-name sd-)
	     (:constructor create-slot-descriptor ())
	     (:predicate nil)
	     (:copier nil))
  (name nil :type symbol)
  (type nil :type t)
  (default nil :type t)
  (getter nil :type symbol)
  (%bit nil :type (mumble::maybe fixnum))
  (%read-only? nil :type mumble::bool)
  (%required? nil :type mumble::bool)
  (%uninitialized? nil :type mumble::bool))


;;; Helper function for bootstrapping.

(defun create-slot-simple (prefix name type default
			    &optional read-only? required? uninitialized?)
  (let ((sd  (create-slot-descriptor)))
    (setf (sd-name sd) name)
    (setf (sd-type sd) type)
    (setf (sd-default sd) default)
    (setf (sd-getter sd) (symbol-append prefix name))
    (setf (sd-%read-only? sd) read-only?)
    (setf (sd-%required? sd) required?)
    (setf (sd-%uninitialized? sd) uninitialized?)
    sd))


;;; Initialize descriptors for the predefined struct types.

(let ((struct-td  (setf (lookup-type 'struct)
			(create-type-descriptor)))
      (type-td    (setf (lookup-type 'type-descriptor)
			(create-type-descriptor)))
      (slot-td    (setf (lookup-type 'slot-descriptor)
			(create-type-descriptor))))
  ;; struct
  (setf (td-type-descriptor struct-td) type-td)
  (setf (td-name struct-td) 'struct)
  (setf (td-%bits-used struct-td) 0)
  ;; type-descriptor
  (setf (td-type-descriptor type-td) type-td)
  (setf (td-name type-td) 'type-descriptor)
  (setf (td-%local-slots type-td)
	(list (create-slot-simple 'td- 'name 'symbol nil)
	      (create-slot-simple 'td- 'slots 'list nil)
	      (create-slot-simple 'td- 'parent-type 't nil)
	      (create-slot-simple 'td- 'printer 't nil)
	      (create-slot-simple 'td- '%local-slots 'list nil)
	      (create-slot-simple 'td- '%bits-used 'fixnum 0)
	      (create-slot-simple 'td- '%constructor 'symbol nil)
	      ))
  (setf (td-slots type-td) (td-%local-slots type-td))
  (setf (td-%bits-used type-td) 0)
  (setf (td-%constructor type-td) 'create-type-descriptor)
  (setf (td-parent-type type-td) struct-td)
  ;; slot-descriptor
  (setf (td-type-descriptor slot-td) type-td)
  (setf (td-name slot-td) 'slot-descriptor)
  (setf (td-%local-slots slot-td)
	(list (create-slot-simple 'sd- 'name 'symbol nil)
	      (create-slot-simple 'sd- 'type 't nil)
	      (create-slot-simple 'sd- 'default 't nil)
	      (create-slot-simple 'sd- 'getter 'symbol nil)
	      (create-slot-simple 'sd- '%bit '(mumble::maybe fixnum) nil)
	      (create-slot-simple 'sd- '%read-only? 'mumble::bool nil)
	      (create-slot-simple 'sd- '%required? 'mumble::bool nil)
	      (create-slot-simple 'sd- '%uninitialized? 'mumble::bool nil)
	      ))
  (setf (td-slots slot-td) (td-%local-slots slot-td))
  (setf (td-%bits-used slot-td) 0)
  (setf (td-%constructor slot-td) 'create-slot-descriptor)
  (setf (td-parent-type type-td) struct-td)
  )



;;;=====================================================================
;;; Support for bit slots
;;;=====================================================================

(eval-when (eval compile load)
  (defconstant max-bits (integer-length most-positive-fixnum)))

(defvar *bit-slot-getters* (make-array max-bits))
(defvar *bit-slot-setters* (make-array max-bits))

(defmacro bit-slot-getter (i) `(svref *bit-slot-getters* ,i))
(defmacro bit-slot-setter (i) `(svref *bit-slot-setters* ,i))

(defmacro define-bit-accessors ()
  (let ((results  nil))
    (dotimes (i max-bits)
      (let ((getter   (intern (format nil "GET-BIT-~a" i)))
	    (setter   (intern (format nil "SET-BIT-~a" i)))
	    (mask     (ash 1 i)))
	(push
	  `(progn
	     (mumble::define-integrable (,getter x)
               (not (eql (the fixnum
			      (logand (the fixnum (struct-%bits x))
				      (the fixnum ,mask)))
			 0)))
	     (mumble::define-integrable (,setter v x)
	       (setf (struct-%bits x)
		     (if v
			 (the fixnum
			      (logior (the fixnum (struct-%bits x))
				      (the fixnum ,mask)))
			 (the fixnum
			      (logandc2 (the fixnum (struct-%bits x))
					(the fixnum ,mask)))))
	       v)
	     (setf (bit-slot-getter ,i) ',getter)
	     (setf (bit-slot-setter ,i) ',setter))
	  results)))
    `(progn ,@results)))

(define-bit-accessors)




;;;=====================================================================
;;; Random helper functions
;;;=====================================================================

(defun quoted? (x)
  (and (consp x) (eq (car x) 'quote)))

(defun quoted-value (x)
  (cadr x))

(defun unknown-type-error (type)
  (error "Struct type ~s has not been defined." type))

(defun unknown-slot-error (type slot)
  (error "Struct type ~s has no slot named ~s." type slot))

(defun lookup-type-descriptor (type)
  (or (lookup-type type)
      (unknown-type-error type)))

(defun lookup-slot-descriptor (type slot)
  (let ((td  (lookup-type-descriptor type)))
    (or (find slot (td-slots td) :key #'sd-name)
	(unknown-slot-error type slot))))

(defun slot-getter-name (type slot)
  (sd-getter (lookup-slot-descriptor type slot)))

(defun sd-getter-function (sd)
  (symbol-function (sd-getter sd)))
  


;;;=====================================================================
;;; Struct-slot macro
;;;=====================================================================

;;; Note that this can be SETF'ed only if type and slot are quoted.

(defmacro struct-slot (type slot object)
  (if (and (quoted? type) (quoted? slot))
      (struct-slot-compiletime (quoted-value type) (quoted-value slot) object)
      (progn
	(warn "Type and/or slot argument to STRUCT-SLOT not constant.")
	`(struct-slot-runtime ,type ,slot ,object))))

(defun struct-slot-compiletime (type slot object)
  (let ((sd  (lookup-slot-descriptor type slot)))
    `(the ,(sd-type sd) (,(sd-getter sd) (the ,type ,object)))))

(defun struct-slot-runtime (type slot object)
  (let ((sd  (lookup-slot-descriptor type slot)))
    ;; *** Could insert explicit type checks here.
    (funcall (sd-getter-function sd) object)))
  

;;;=====================================================================
;;; Make macro and support
;;;=====================================================================

(defmacro make (type . inits)
  (make-aux type inits))

;;; Turn the call to MAKE into a call to the boa constructor.
;;; The arguments to the BOA constructor are those slots that have
;;; the required? flag set to true.  If initializers for other slots
;;; are provided, turn these into SETFs.  Bit attributes are always 
;;; handled via SETF.

(defun make-aux (type inits)
  (let* ((td           (lookup-type-descriptor type))
	 (boa          (td-%constructor td))
	 (slots        (td-slots td))
	 (tempvar      (gensym))
	 (setfs        '())
	 (bits-inits   '())
	 (slot-inits   '()))
    (check-slot-inits type inits)
    (dolist (s slots)
      (let* ((name           (sd-name s))
	     (supplied?      (mumble::assq name inits))
	     (required?      (sd-%required? s))
	     (uninitialized? (sd-%uninitialized? s))
	     (init           (if supplied?
				 (progn
				   ;; *** Maybe want to suppress this warning.
				   ;;(when (not required?)
				   ;;  (override-slot-init-warning type name))
				   (cadr supplied?))
				 (progn
				   ;; *** Maybe want to suppress this warning.
				   (when (and required? (not uninitialized?))
				     (missing-slot-init-warning type name))
				   (sd-default s)))))
	(cond ((sd-%bit s)
	       (cond ((or (eq init 'nil) (equal init '(quote nil)))
		      ;; do nothing, bit already defaults to 0
		      )
		     ((and uninitialized? (not supplied?) required?)
		      ;; no default or init supplied, leave uninitialized
		      )
		     ((constantp init)
		      ;; it must be a non-false constant, set bit to 1
		      (push (ash 1 (sd-%bit s)) bits-inits))
		     (t
		      ;; have to do runtime test
		      (push `(the fixnum (if ,init ,(ash 1 (sd-%bit s)) 0))
			    bits-inits))))
	      ((and required? (not uninitialized?))
	       ;; The constructor takes the value as a positional argument.
	       (push init slot-inits))
	      (supplied?
	       ;; Make a setf.  
	       ;; No point in putting the same value in twice.
	       (unless (and (constantp init) (equal init (sd-default s)))
		 (push `(setf (,(sd-getter s) ,tempvar) ,init) setfs)))
	      (t nil))))
    (unless (null bits-inits)
      (push `(setf (struct-%bits ,tempvar)
		   ,(cond ((null (cdr bits-inits))
			   (car bits-inits))
			  ((every #'constantp bits-inits)
			   (apply #'logior bits-inits))
			  (t
			   `(the fixnum (logior ,@(nreverse bits-inits))))))
	    setfs))
    (if (null setfs)
	`(,boa ,@(nreverse slot-inits))
	`(let ((,tempvar  (,boa ,@(nreverse slot-inits))))
	   ,@(nreverse setfs)
	   ,tempvar))))

(defun override-slot-init-warning (type name)
  (warn "Overriding default for slot ~s in MAKE ~s."
	name type))

(defun missing-slot-init-warning (type name)
  (warn "No initializer or default for slot ~s in MAKE ~s."
	name type))

(defun check-slot-inits (type inits)
  (dolist (i inits)
    (lookup-slot-descriptor type (car i))))



;;;====================================================================
;;; Update-slots macro
;;;====================================================================

;;; Note that type is a literal here.
;;; *** Could be smarter about merging setters for bit slots.

(defmacro update-slots (type exp . inits)
  (let ((temp  (gensym)))
    `(let ((,temp  ,exp))
       ,@(mapcar #'(lambda (i)
		     `(setf (struct-slot ',type ',(car i) ,temp) ,(cadr i)))
		 inits))))



;;;====================================================================
;;; With-slots macro
;;;====================================================================

;;; Note that type is a literal here.
;;; ***Could be smarter about merging accesses for bit slots.

(defmacro mumble::with-slots (type slots exp . body)
  (let ((temp  (gensym)))
    `(let* ((,temp  ,exp)
	    ,@(mapcar #'(lambda (s)
			  `(,s  (struct-slot ',type ',s ,temp)))
		      slots))
       ,@body)))


;;;====================================================================
;;; Define-struct macro
;;;====================================================================


;;; The rather strange division here is so that the call to MAKE
;;; works right.
;;; All INSTALL-STRUCT-TYPE does is fill in and install the type
;;; descriptor object.

(defmacro define-struct (name . fields)
  (multiple-value-bind (include type-template slots prefix predicate)
      (parse-struct-fields name fields)
    `(progn
       (eval-when (eval compile load)
	 (install-struct-type
	   ',name
	   ',include
	   ',prefix
	   (make ,type-template)
	   ',slots))
       (define-struct-aux ,name ,include ,prefix ,predicate))))


;;; This is the macro that actually creates the DEFSTRUCT expansion.

(defmacro define-struct-aux (name include prefix predicate)
  (let* ((td           (lookup-type name))
	 (slots        (td-slots td))
	 (local-slots  (td-%local-slots td))
	 (bit-slots    (remove-if-not #'sd-%bit slots)))
    `(progn
       ;; Make the struct definition.
       ;; *** could put the type descriptor for the default in a
       ;; *** global variable; it might speed up reference.
       (defstruct (,name
		    (:include ,include
			      (type-descriptor (lookup-type ',name)))
		    (:conc-name ,prefix)
		    ;; Disable the default keyword constructor.
		    ;; If you do this in AKCL, it will complain about
		    ;; the BOA constructor.  Bogus!!!
		    ;; If you do this in WCL, it will just quietly ignore
		    ;; the BOA.
		    #-(or akcl wcl) (:constructor nil)
		    (:constructor ,(td-%constructor td) ,(make-boa-args slots))
		    (:predicate ,predicate)
		    (:copier    nil))
	 ,@(mapcar
	    #'(lambda (s)
		`(,(sd-name s) ,(sd-default s)
		  ;; CMU common lisp initializes &aux boa constructor
		  ;; slots to NIL instead of leaving them uninitialized,
		  ;; and then complains if this doesn't match the declared
		  ;; slot type.  I think this is a bug, not a feature, but
		  ;; here's a workaround for it.
		  :type
		  #+cmu ,(if (sd-%uninitialized? s)
			     `(or ,(sd-type s) null)
			     (sd-type s))
		  #-cmu ,(sd-type s)
	          ;; Can make slots read-only only if a setf-er is not 
		  ;; required by MAKE.
		  :read-only ,(and (sd-%read-only? s) (sd-%required? s))))
	    local-slots))
       ;; Make accessor functions for bit slots.
       ,@(mapcar
	  #'(lambda (s)
	      (let ((place  (symbol-append prefix (sd-name s)))
		    (getter (bit-slot-getter (sd-%bit s)))
		    (setter (bit-slot-setter (sd-%bit s))))
		`(progn
		   (mumble::define-integrable (,place x) (,getter x))
		   ,@(unless (sd-%read-only? s)
		       `((mumble::define-setf ,place ,setter))))
		))
	  bit-slots)
	 ',name)
      ))



;;; Determine which arguments to make explicit to the boa constructor.
;;; Basically, expect an explicit initializer for any slot that does not 
;;; have a default supplied.
;;; Supplying slot names as &aux parameters to a boa constructor is
;;; supposed to suppress initialization.

(defun make-boa-args (slots)
  (let ((required-args      '())
	(uninitialized-args '()))
    (dolist (s slots)
      (when (and (sd-%required? s) (not (sd-%bit s)))
	(if (sd-%uninitialized? s)
	    (push (sd-name s) uninitialized-args)
	    (push (sd-name s) required-args))))
    ;; Gag.  AKCL does the wrong thing with &AUX arguments; defstruct sticks
    ;; another &AUX at the end of the lambda list.  Looks like it will do
    ;; the right thing if you just omit the uninitialized arguments from
    ;; the boa arglist entirely.
    #+akcl (nreverse required-args)
    #-akcl   
    (if (null uninitialized-args)
	(nreverse required-args)
	`(,@(nreverse required-args) &aux ,@(nreverse uninitialized-args)))
    ))


;;; Install the type descriptor, filling in all the slots.

(defun install-struct-type (name include prefix td slots)
  (let* ((parent-type  (lookup-type-descriptor include))
	 (bits-used    (td-%bits-used parent-type))
	 (local-slots  '())
	 (all-slots    '()))
    (dolist (s slots)
      (multiple-value-bind
	  (slot-name type default bit read-only? required? uninitialized?)
	  (parse-slot-fields name s)
	(let ((sd   (create-slot-simple
		      prefix slot-name type default
		      read-only? required? uninitialized?)))
	  (push sd all-slots)
	  (cond (bit
		 (if (eql bits-used max-bits)
		     (error "Too many bit slots in DEFINE-STRUCT ~s." name))
		 (setf (sd-%bit sd) bits-used)
		 (incf bits-used))
		(t
		 (push sd local-slots))))))
    (setf local-slots (nreverse local-slots))
    (setf (td-name td) name)
    (setf (td-slots td) (append (td-slots parent-type) (nreverse all-slots)))
    (setf (td-%local-slots td) local-slots)
    (setf (td-%bits-used td) bits-used)
    (setf (td-%constructor td) (symbol-append '%create- name))
    (setf (td-parent-type td) parent-type)
    (setf (lookup-type name) td)))


;;; Struct field parsing.

(defun parse-struct-fields (name fields)
  (when (not (symbolp name))
    (error "Structure name ~s is not a symbol." name))
  (let ((include         nil)
	(type-template   nil)
	(slots           nil)
	(prefix          nil)
	(predicate       nil))
    (dolist (f fields)
      (cond ((not (consp f))
	     (unknown-field-error f name))
	    ((eq (car f) 'include)
	     (if include
		 (duplicate-field-error 'include name)
		 (setf include (cadr f))))
	    ((eq (car f) 'type-template)
	     (if type-template
		 (duplicate-field-error 'type-template name)
		 (setf type-template (cadr f))))
	    ((eq (car f) 'slots)
	     (if slots
		 (duplicate-field-error 'slots name)
		 (setf slots (cdr f))))
	    ((eq (car f) 'prefix)
	     (if prefix
		 (duplicate-field-error 'prefix name)
		 (setf prefix (cadr f))))
	    ((eq (car f) 'predicate)
	     (if predicate
		 (duplicate-field-error 'predicate name)
		 (setf predicate (cadr f))))
	    (t
	     (unknown-field-error f name))))
    (values
      (or include 'struct)
      (or type-template
	  (and include
	       (td-name (td-type-descriptor (lookup-type-descriptor include))))
	  'type-descriptor)
      (or slots '())
      (or prefix (symbol-append name '-))
      predicate)))

(defun unknown-field-error (f name)	     
  (error "Unknown field ~s in DEFINE-STRUCT ~s." f name))

(defun duplicate-field-error (f name)
  (error "Field ~s appears more than once in DEFINE-STRUCT ~s." f name))



;;; Parsing for slot specifications.

(defun parse-slot-fields (struct-name slot)
  (let ((name           nil)
	(type           t)
	(default        '*default-slot-default*)
	(bit            nil)
	(read-only?     nil)
	(required?      t)
	(uninitialized? nil))
    (if (or (not (consp slot))
	    (not (symbolp (setf name (car slot)))))
	(invalid-slot-error slot struct-name))
    (dolist (junk (cdr slot))
      (cond ((eq (car junk) 'type)
	     (setf type (cadr junk)))
	    ((eq (car junk) 'default)
	     (setf default (cadr junk))
	     (setf required? nil))
	    ((eq (car junk) 'bit)
	     (setf bit (cadr junk)))
	    ((eq (car junk) 'read-only?)
	     (setf read-only? (cadr junk)))
	    ((eq (car junk) 'uninitialized?)
	     (setf uninitialized? (cadr junk)))
	    (t
	     (invalid-slot-error slot struct-name))))
    (values
      name
      type
      default
      bit
      read-only?
      required?
      uninitialized?
      )))

;;; Some implementations of DEFSTRUCT complain if the default value
;;; for a slot doesn't match the declared type of that slot, even if
;;; the default is never used.
;;; Using this variable as the default init form for such slots should
;;; suppress such warnings.

(defvar *default-slot-default* nil)

(defun invalid-slot-error (slot struct-name)
  (error "Invalid slot syntax ~s in DEFINE-STRUCT ~s." slot struct-name))



;;;=====================================================================
;;; Printer hooks
;;;=====================================================================

;;; Here is the macro for associating a printer with a structure type.

(defmacro define-struct-printer (type function)
  `(define-struct-printer-aux ',type (function ,function)))

(defun define-struct-printer-aux (type function)
  (let ((td  (lookup-type-descriptor type)))
    (setf (td-printer td) function)
    type))


;;;=====================================================================
;;; Imports
;;;=====================================================================


;;; Generic stuff

(define-mumble-import struct)
(define-mumble-import struct?)
(define-mumble-import struct-type-descriptor)


;;; Predefined types, slots, and accessors
;;; Note:  not all slots are exported.

(define-mumble-import type-descriptor)
(define-mumble-import name)
(define-mumble-import slots)
(define-mumble-import parent-type)
(define-mumble-import printer)
(define-mumble-import td-name)
(define-mumble-import td-slots)
(define-mumble-import td-parent-type)
(define-mumble-import td-printer)

(define-mumble-import slot-descriptor)
(define-mumble-import name)
(define-mumble-import type)
(define-mumble-import default)
(define-mumble-import getter)
(define-mumble-import sd-name)
(define-mumble-import sd-type)
(define-mumble-import sd-default)
(define-mumble-import sd-getter)


;;; Utility functions

(define-mumble-import lookup-type-descriptor)
(define-mumble-import lookup-slot-descriptor)
(define-mumble-import sd-getter-function)


;;; Macros

(define-mumble-import make)
(define-mumble-import struct-slot)
(define-mumble-import define-struct)
(define-mumble-import mumble::with-slots)
(define-mumble-import update-slots)
(define-mumble-import define-struct-printer)


;;; Field names for define-struct

(define-mumble-import include)
(define-mumble-import type-template)
(define-mumble-import slots)
(define-mumble-import prefix)
(define-mumble-import predicate)


;;; Field names for slot options

(define-mumble-import type)
(define-mumble-import default)
(define-mumble-import bit)
(define-mumble-import read-only?)
(define-mumble-import uninitialized?)


