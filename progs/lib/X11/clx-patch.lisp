(lisp:in-package 'xlib)
(defmacro generate-lookup-functions (useless-name &body types)
            `(within-definition (,useless-name generate-lookup-functions)
               ,@(mapcar
                   #'(lambda (type)
                       `(defun ,(xintern 'lookup- type)
                               (display id)
                          (declare (type display display)
                                   (type resource-id id))
                          (declare (values ,type))
                          ,(if (member type *clx-cached-types*)
                               `(let ((,type (lookup-resource-id display id)))
                                  (cond ((null ,type) ;; Not found, create and s
ave it.
                                         (setq ,type (,(xintern 'make- type)
                                                      :display display :id id))
                                         (save-id display id ,type))
                                        ;; Found.  Check the type
                                        ,(cond ((null '()) ;*type-check?*)
                                                `(t ,type))
                                               ((member type '(window pixmap))
                                                `((type? ,type 'drawable) ,type)
)
                                               (t `((type? ,type ',type) ,type))
)
                                        ,@(when '() ;*type-check?*
                                            `((t (x-error 'lookup-error
                                                          :id id
                                                          :display display
                                                          :type ',type
                                                          :object ,type))))))
                               ;; Not being cached.  Create a new one each time.
                               `(,(xintern 'make- type)
                                 :display display :id id))))
                   types)))
(macroexpand 
  (generate-lookup-functions ignore
    window))

