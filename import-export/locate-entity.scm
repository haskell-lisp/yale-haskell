;;; This file deals with entities in import / export lists

;;; This resolves an entity with the export table of a
;;; module.  It returns either a group, the symbol 'error, or the symbol
;;; 'not-found.  When force-error? is true, signal an error when
;;; the module is not found & return 'error.

(define (locate-entity/export-table entity mod force-error?)
  (let* ((name (entity-name entity))
	 (group (table-entry (module-export-table mod) name)))
    (if (eq? group '#f)
	(if (not force-error?)
	    'not-found
	    (signal-entity-not-found name (module-name mod)))
	(let ((def (group-definition group)))
	  (cond ((is-type? 'entity-var entity)
		 group)
		((is-type? 'entity-con entity)
		 (cond ((algdata? def)
			(strip-constructors group))
		       ((synonym? def)
			(signal-synonym-needs-dots name (module-name mod)))
		       (else
			(signal-wrong-definition
			  "type constructor" name (module-name mod)))))
		((is-type? 'entity-abbreviated entity)
		 (cond ((algdata? def)
			(cond ((hidden-constructors? group)
			       (if force-error?
				   (signal-abstract-type
				     name (module-name mod))
				   'not-found))
			      (else
			       group)))
		       ((or (class? def) (synonym? def))
			group)
		       (else
			(signal-wrong-definition
			  "class or datatype" name (module-name mod)))))
		((is-type? 'entity-class entity)
		 (if (class? def)
		     (match-constituents group (entity-class-methods entity)
					 entity "method")
		     (signal-wrong-definition "class" name (module-name mod))))
		((is-type? 'entity-datatype entity)
		 (if (algdata? def)
		     (match-constituents group
					 (entity-datatype-constructors entity)
					 entity "constructor")
		     (signal-wrong-definition
		       "data type" name (module-name mod))))
		(else
		 (error "Bad entity ~s." entity))
		)))))

(define (match-constituents group names entity what)
  (check-duplicates names entity)
  (dolist (n-d (cdr group))
    (when (not (memq (tuple-2-1 n-d) names))
      (signal-extra-constituent entity (tuple-2-1 n-d) what)))
  (dolist (name names)
    (when (not (assq name (cdr group)))
      (signal-missing-constituent entity name what)))
  group)


;;; The following routine locates an entity in the current module.
;;; It may return 'error, 'not-found, or a group.

(define (locate-entity entity)
  (let* ((name (entity-name entity))
	 (def (resolve-toplevel-name name)))
    (cond ((eq? def '#f)
	   'not-found)
	  ((is-type? 'entity-var entity)
	   (if (method-var? def)
	       (signal-export-method-var name)
	       (make-group name def)))
	  ((is-type? 'entity-con entity)
	   (cond ((algdata? def)
		  (make-group name def))
		 ((synonym? def)
		  (signal-synonym-needs-dots name *module-name*))
		 (else
		  (signal-wrong-definition
		    "type constructor" name *module-name*))))
	  ((is-type? 'entity-abbreviated entity)
	   (cond ((algdata? def)
		  (require-complete-algdata
		   (gather-algdata-group name def)))
		 ((synonym? def)
		  (make-group name def))
		 ((class? def)
		  (gather-class-group name def))
		 (else
		  (signal-wrong-definition
		    "type constructor or class" name *module-name*))))
	  ((is-type? 'entity-class entity)
	   (if (class? def)
	       (match-group-names
		 (gather-class-group name def)
		 (entity-class-methods entity)
		 entity
		 "method")
	       (signal-wrong-definition "class" name *module-name*)))
	  ((is-type? 'entity-datatype entity)
	   (if (algdata? def)
	       (match-group-names
		 (require-complete-algdata (gather-algdata-group name def))
		 (entity-datatype-constructors entity)
		 entity "constructor")
	       (signal-wrong-definition "data type" name *module-name*)))
	  (else
	   (error "Bad entity ~s." entity)))))

(define (require-complete-algdata group)
  (if (hidden-constructors? group)
      'not-found
      group))

(define (match-group-names group names entity what)
  (when (not (eq? group 'not-found))
    (match-constituents group names entity what))
  group)


