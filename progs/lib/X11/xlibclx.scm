;;; xlibclx.scm -- Lisp support for Haskell/CLX interface

;; general

(define-syntax (nth-value n form)
  (cond ((eqv? n 0)
	 `(values ,form))
	((number? n)
	 (let ((temps  '()))
	   (dotimes (i n)
	     (declare (ignorable i))
	     (push (gensym) temps))
	   `(multiple-value-bind ,(reverse temps) ,form
	      (declare (ignore ,@(reverse (cdr temps))))
	      ,(car temps))))
	(else
	 `(lisp:nth ,n (lisp:multiple-value-list ,form)))
	))


(define-local-syntax (keywordify string)
  `(lisp:intern ,string (lisp:find-package "KEYWORD")))

(define-local-syntax (xlibify string)
  `(lisp:intern ,string (lisp:find-package "XLIB")))



;;; This is stuff to support slots that consist of a keyword/value
;;; pair.  Note that the value is always unboxed.

(define-syntax (make-keyword key value)
  `(cons ,key ,value))

(define-syntax (is-keyword? x key)
  `(eq? (car ,x) ,key))

(define-syntax (keyword-key x) `(car ,x))
(define-syntax (keyword-val x) `(cdr ,x))

(define-syntax (define-keyword-constructor name)
  (let* ((name-str (symbol->string name))
	 (key      (keywordify name-str))
	 (is-name  (string->symbol (string-append "IS-" name-str)))
	 (mk-name  (string->symbol (string-append "MK-" name-str))))
    `(begin
       (define (,mk-name x) (make-keyword ,key x))
       (define (,is-name x) (is-keyword? x ,key)))
    ))

(define-syntax (define-event-slot-finder slot)
  (let* ((slot-str (symbol->string slot))
	 (slot-key (keywordify slot-str))
	 (fun      (string->symbol (string-append "X-EVENT-" slot-str))))
    `(define (,fun event) (lookup-event-slot (cdr event) ,slot-key))))    
    
(define (lookup-event-slot event key)
  (if (null? event)
      (error "non-existent event slot: ~A" key)
      (if (eq? key (car event))
	  (cadr event)
	  (lookup-event-slot (cddr event) key))))


(define-syntax (define-attribute-setter entity attribute)
  (let* ((entity-attr (string-append (symbol->string entity)
				     "-"
				     (symbol->string attribute)))
	 (fun-name    (string->symbol (string-append "X-SET-" entity-attr)))
	 (xfun-name   (xlibify entity-attr)))
    `(define (,fun-name ,entity ,attribute)
       (setf (,xfun-name ,entity) ,attribute))))

(define-syntax (make-h-tuple . args)
  (let ((nargs (map (lambda (arg) `(box ,arg)) args)))
    `(make-tuple ,@nargs)))

;; type XError

(define (cons-xerror x)
  (declare (ignore x))
  (error "can't construct XError"))

(define (x-error-string c)
  (make-haskell-string (format '#f "~A" c)))


;;; The forces here are necessary because the thing being funcalled
;;; returns a data structure of type (IO a), and we need to do
;;; an  IO a -> a transformation.

#+lucid
(define (x-handle-error handler body)
  (lisp:catch 'x-error-handle
	      (lcl:handler-bind ((lisp:error (mk-handler handler)))
				(force (funcall body (box 'state))))))

#+(or cmu allegro lispworks)
(define (x-handle-error handler body)
  (lisp:catch 'x-error-handle
	      (lisp:handler-bind ((lisp:error (mk-handler handler)))
				 (force (funcall body (box 'state))))))

#+akcl
(define (x-handle-error handler body)
  (error "AKCL does not support HANDLER-BIND!"))

(define (mk-handler handler)
  (lambda (c) 
    (lisp:throw 'x-error-handle 
		(force (funcall handler 
				(box c)
				(box 'state))))))

;; for type XMaybe

(define (not-null? x) (not (null? x)))


;; For Bitmap, Pixarray, KeysymTable

(define (array2->haskell-list a)
  (let* ((dims    (lisp:array-dimensions a))
	 (i1max   (car dims))
	 (i2max   (cadr dims)))
    (declare (type fixnum i1max i2max))
    (do ((i1     (the fixnum (1- i1max)) (the fixnum (1- i1)))
	 (outer  '()))
	((< i1 0) outer)
	(declare (type fixnum i1))
	(setf outer
	      (cons
	        (box
		  (do ((i2    (the fixnum (1- i2max)) (the fixnum (1- i2)))
		       (inner '()))
		      ((< i2 0) inner)
		      (declare (type fixnum i2))
		      (setf inner
			    (cons (box (lisp:aref a i1 i2))
				  (box inner)))))
		(box outer))))
    ))


;; Bitmap

(define (mk-bitmap ll)
  (let ((l (haskell-list->list #'haskell-list->list/identity ll)))
    (lisp:make-array `(,(length l) , (length (car l))) 
		     :element-type 'lisp:bit
		     :initial-contents l)))

(define (sel-bitmap l)
  (array2->haskell-list l))


;; XKeysymTable

(define (mk-keysym-table ll)
  (let ((l (haskell-list->list #'haskell-list->list/identity ll)))
    (lisp:make-array `(,(length l) , (length (car l))) 
		     :element-type 'xlib:card32
		     :initial-contents l)))

(define (sel-keysym-table l)
  (array2->haskell-list l))

;; XPixarray

(define (mk-pixarray ll)
  (let ((l (haskell-list->list #'haskell-list->list/identity ll)))
    (let* ((max-num  (find-max l))
	   (pix-type (cond ((<= max-num 1) 'lisp:bit)
			   ((<= max-num 15) '(lisp:unsigned-byte 4))
			   ((<= max-num 255) 'xlib:card8)
			   ((<= max-num 65535) 'xlib:card16)
			   (else 'xlib:card32))))
      (declare (type integer max-num))
      (lisp:make-array `(,(length l) , (length (car l)))
		       :element-type pix-type
		       :initial-contents l))))

(define (find-max l)
  (let ((max  0))
    (dolist (ll l)
      (dolist (lll ll)
	(when (> (the integer lll) (the integer max))
	  (setf max lll))))
    max))

(define (sel-pixarray l)
  (array2->haskell-list l))




;;; Can't use mumble vector primitives on arrays of specialized types!

(define (array1->haskell-list a)
  (declare (type lisp:vector a))
  (let ((imax  (lisp:length a)))
    (declare (type fixnum imax))
    (do ((i      (the fixnum (1- imax)) (the fixnum (1- i)))
	 (result '()))
	((< i 0) result)
	(declare (type fixnum i))
	(setf result
	      (cons (box (lisp:aref a i))
		    (box result))))))

;; BitVec

(define (mk-bitvec ll)
  (let ((l (haskell-list->list/identity ll)))
    (lisp:make-array `(,(length l)) :element-type 'lisp:bit
		     :initial-contents l)))

(define (sel-bitvec l)
  (array1->haskell-list l))

;; ByteVec

(define (mk-bytevec ll)
  (let ((l (haskell-list->list/identity ll)))
    (lisp:make-array `(,(length l)) :element-type 'xlib:card8
		     :initial-contents l)))

(define (sel-bytevec l)
  (array1->haskell-list l))


;; XAtom
(define (mk-atom name)
  (keywordify (haskell-string->string name)))

(define (sel-atom atom)
  (make-haskell-string (symbol->string atom)))

;; XProperty
;;; watch out for name conflict with :property keyword stuff
(define (mk-xproperty d ty f) (list (haskell-list->list/identity d) ty f))
(define (sel-xproperty-data p) (list->haskell-list/identity (car p)))
(define (sel-xproperty-type p) (cadr p))
(define (sel-xproperty-format p) (caddr p))

(define (mk-event type slots)
  (cons type (slots->keywords (haskell-list->list/identity slots))))

(define (sel-event-type event) (car event))

(define (sel-event-slots event) 
  (list->haskell-list/identity (keywords->slots (car event) (cdr event) event)))

;; XEventSlot

(define-keyword-constructor window)
(define-keyword-constructor event-window)
(define-keyword-constructor code)
(define-keyword-constructor pos)
(define-keyword-constructor state)
(define-keyword-constructor time)
(define-keyword-constructor root)
(define-keyword-constructor root-pos)
(define-keyword-constructor child)
(define-keyword-constructor same-screen-p)
(define-keyword-constructor hint-p)
(define-keyword-constructor mode)
(define-keyword-constructor kind)
(define-keyword-constructor focus-p)
(define-keyword-constructor keymap)
(define-keyword-constructor request)
(define-keyword-constructor start)
(define-keyword-constructor count)
(define-keyword-constructor rect)
(define-keyword-constructor drawable)
(define-keyword-constructor graph-fun)
(define-keyword-constructor place)
(define-keyword-constructor border-width)
(define-keyword-constructor above-sibling)
(define-keyword-constructor override-redirect-p)
(define-keyword-constructor parent)
(define-keyword-constructor configure-p)
(define-keyword-constructor visibility)
(define-keyword-constructor new-p)
(define-keyword-constructor installed-p)
(define-keyword-constructor stack-mode)
(define-keyword-constructor value-mask)
(define-keyword-constructor size)
(define-keyword-constructor message)
(define-keyword-constructor property-state)
(define-keyword-constructor atom)
(define-keyword-constructor selection)
(define-keyword-constructor target)
(define-keyword-constructor property)
(define-keyword-constructor requestor)

(define-event-slot-finder window)
(define-event-slot-finder event-window)
(define-event-slot-finder code)
(define-event-slot-finder x)
(define-event-slot-finder y)
(define-event-slot-finder state)
(define-event-slot-finder time)
(define-event-slot-finder root)
(define-event-slot-finder root-x)
(define-event-slot-finder root-y)
(define-event-slot-finder child)
(define-event-slot-finder same-screen-p)
(define-event-slot-finder hint-p)
(define-event-slot-finder mode)
(define-event-slot-finder kind)
(define-event-slot-finder focus-p)
(define-event-slot-finder keymap)
(define-event-slot-finder request)
(define-event-slot-finder start)
(define-event-slot-finder count)
(define-event-slot-finder width)
(define-event-slot-finder height)
(define-event-slot-finder drawable)
(define-event-slot-finder major)
(define-event-slot-finder minor)
(define-event-slot-finder place)
(define-event-slot-finder border-width)
(define-event-slot-finder above-sibling)
(define-event-slot-finder override-redirect-p)
(define-event-slot-finder parent)
(define-event-slot-finder configure-p)
(define-event-slot-finder new-p)
(define-event-slot-finder installed-p)
(define-event-slot-finder stack-mode)
(define-event-slot-finder value-mask)
(define-event-slot-finder data)
(define-event-slot-finder type)
(define-event-slot-finder format)
(define-event-slot-finder atom)
(define-event-slot-finder selection)
(define-event-slot-finder target)
(define-event-slot-finder property)
(define-event-slot-finder requestor)

(define (x-event-pos event) (mk-xpoint (x-event-x event) (x-event-y event)))

(define (x-event-root-pos event) 
  (mk-xpoint (x-event-root-x event) (x-event-root-y event)))

(define (x-event-size event) 
  (mk-xsize (x-event-width event) (x-event-height event)))

(define (x-event-rect event) 
  (mk-xrect (x-event-x event) (x-event-y event)
	    (x-event-width event) (x-event-height event)))

(define (x-event-graph-fun event)
  (cons (x-event-major event) (x-event-minor event)))

(define (x-event-message event)
  (list (sequence->list (x-event-data event))
	(x-event-type event)
	(x-event-format event)))


;; XEventMask

(define (x-make-event-mask keys)
  (apply (function xlib:make-event-mask) (haskell-list->list/identity keys)))

(define (x-event-mask-key-list mask)
  (list->haskell-list/identity (xlib:make-event-keys mask)))

;; XStateMask

(define (x-make-state-mask keys)
  (apply (function xlib:make-state-mask) (haskell-list->list/identity keys)))

(define (x-state-mask-key-list mask)
  (list->haskell-list/identity (xlib:make-state-keys mask)))


(define-keyword-constructor background)
(define-keyword-constructor foreground)
(define-keyword-constructor event-mask)
(define-keyword-constructor depth)
(define-keyword-constructor border-width)
(define-keyword-constructor class)
(define-keyword-constructor visual)
(define-keyword-constructor border)
(define-keyword-constructor backing-store)
(define-keyword-constructor backing-planes)
(define-keyword-constructor backing-pixel)
(define-keyword-constructor save-under)
(define-keyword-constructor do-not-propagate-mask)
(define-keyword-constructor override-redirect)
(define-keyword-constructor colormap)
(define-keyword-constructor cursor)

(define-keyword-constructor arc-mode)
(define-keyword-constructor cap-style)
(define-keyword-constructor clip-mask)
(define-keyword-constructor clip-origin)
(define-keyword-constructor dash-offset)
(define-keyword-constructor dashes)
(define-keyword-constructor exposures)
(define-keyword-constructor fill-rule)
(define-keyword-constructor fill-style)
(define-keyword-constructor font)
(define-keyword-constructor function)
(define-keyword-constructor join-style)
(define-keyword-constructor line-style)
(define-keyword-constructor line-width)
(define-keyword-constructor plane-mask)
(define-keyword-constructor stipple)
(define-keyword-constructor subwindow-mode)
(define-keyword-constructor tile)
(define-keyword-constructor tile-origin)

(define-keyword-constructor bit-lsb-first-p)
(define-keyword-constructor bits-per-pixel)
(define-keyword-constructor blue-mask)
(define-keyword-constructor byte-lsb-first-p)
(define-keyword-constructor bytes-per-line)
(define-keyword-constructor data)
(define-keyword-constructor format)
(define-keyword-constructor green-mask)
(define-keyword-constructor size)
(define-keyword-constructor name)
(define-keyword-constructor red-mask)
(define-keyword-constructor hot-spot)


(define-keyword-constructor owner-p)
(define-keyword-constructor sync-pointer-p)
(define-keyword-constructor sync-keyboard-p)
(define-keyword-constructor confine-to)


;; XClipMask

(define (not-pixmap-and-list-p x) 
  (and (pair? x) (not (xlib:pixmap-p x))))
(define (mk-clip-mask-rects rects) 
  (rects->point-seq (haskell-list->list/identity rects)))
(define (sel-clip-mask-rects point-seq) 
  (list->haskell-list/identity (point-seq->rects point-seq)))

;; XPoint

(define (mk-xpoint x y) (cons x y))
(define (xpoint-x x) (car x))
(define (xpoint-y x) (cdr x))

;; XSize

(define (mk-xsize x y) (cons x y))
(define (xsize-w x) (car x))
(define (xsize-h x) (cdr x))

;; XRect
(define (mk-xrect x y w h) (vector x y w h))
(define (xrect-x x) (vector-ref x 0))
(define (xrect-y x) (vector-ref x 1))
(define (xrect-w x) (vector-ref x 2))
(define (xrect-h x) (vector-ref x 3))

;; XArc

(define (mk-xarc x y w h a1 a2) (vector x y w h a1 a2))

(define (xarc-x x) (vector-ref x 0))
(define (xarc-y x) (vector-ref x 1))
(define (xarc-w x) (vector-ref x 2))
(define (xarc-h x) (vector-ref x 3))
(define (xarc-a1 x) (vector-ref x 4))
(define (xarc-a2 x) (vector-ref x 5))

;; BitmapFormat

(define (mk-bitmap-format u p l) 
  (xlib::make-bitmap-format :unit u :pad p :lsb-first-p l))

;; PixmapFormat

(define (mk-pixmap-format u p l) 
  (xlib::make-pixmap-format :depth u :bits-per-pixel p :scanline-pad l))

;; XVisualInfo

(define (mk-xvisual-info id cl rm gm bm bs es) 
  (xlib::make-visual-info :id id :class cl :red-mask rm :green-mask gm 
			  :blue-mask bm :bits-per-rgb bs :colormap-entries es))

;; XFillContent

(define (is-fill-pixel x) (not (or (xlib:pixmap-p x) (symbol? x))))

;; XBackingStore

;; XImageData

(define (bitmap-list-p x) (pair? x))
(define (pixarray-p x) (and (not (pair? x)) (eq? (lisp:array-rank x) 2)))
(define (bytevec-p x) (and (not (pair? x)) (eq? (lisp:array-rank x) 1)))

;; XColor
(define (mk-color r g b) 
  (xlib:make-color :red r :green g :blue b))


(define (x-print x)
  (print x))

(define (x-set-event-mask-key mask key-sym) 
  (lisp:logior mask (xlib:make-event-mask key-sym)))

(define (x-clear-event-mask-key mask key-sym) 
  (lisp:logand mask (lisp:lognot (xlib:make-event-mask key-sym))))


(define (x-test-event-mask-key mask key-sym)
  (if (eqv? 0 (lisp:logand mask (xlib:make-event-mask key-sym))) '#f '#t))

(define (x-set-state-mask-key mask key-sym) 
  (lisp:logior mask (xlib:make-state-mask key-sym)))

(define (x-clear-state-mask-key mask key-sym) 
  (lisp:logand mask (lisp:lognot (xlib:make-state-mask key-sym))))

(define (x-test-state-mask-key mask key-sym)
  (if (eqv? 0 (lisp:logand mask (xlib:make-state-mask key-sym))) '#f '#t))


;;; Display is a string of the format name:d.s
;;; ignore s; if d is omitted, default it to zero.

(define (x-open-display display)
  (let* ((end    (string-length display))
	 (colon  (or (string-position #\: display 0 end) end))
	 (dot    (or (string-position #\. display colon end) end)))
    (declare (type fixnum end colon dot))
    (xlib:open-display
      (substring display 0 colon)
      :display (if (eqv? colon dot)
		   0
		   (string->number (substring display (1+ colon) dot))))))

(define (x-set-display-error-handler display error-fun)
  (declare (ignore display error-fun))
  (error "not implemented"))

(define (x-set-display-after-function display after-fun)
  (declare (ignore display after-fun))
  (error "not implemented"))

(define (x-screen-depths screen)
  (let ((depths (xlib:screen-depths screen)))
    (map (lambda (l) (make-h-tuple (car l) (list->haskell-list/identity (cdr l))))
	 depths)))

(define (x-screen-size screen)
  (mk-xsize (xlib:screen-width screen) (xlib:screen-height screen)))

(define (x-screen-mmsize screen)
  (mk-xsize (xlib:screen-width-in-millimeters screen) 
	    (xlib:screen-height-in-millimeters screen)))

(define (x-create-window parent rect attrs)
  (apply (function XLIB:CREATE-WINDOW)
	 `(:parent ,parent :x ,(xrect-x rect) :y ,(xrect-y rect)
	   :width ,(xrect-w rect) :height ,(xrect-h rect)
	   ,@(attrs->keywords attrs))))

(define-attribute-setter drawable border-width)

(define (x-drawable-size drawable)
  (mk-xsize (xlib:drawable-width drawable) (xlib:drawable-height drawable)))

(define (x-drawable-resize drawable size)
  (setf (xlib:drawable-width drawable) (xsize-w size))
  (setf (xlib:drawable-height drawable) (xsize-h size)))

(define (x-window-pos window)
  (mk-xpoint (xlib:drawable-x window) (xlib:drawable-y window)))

(define (x-window-move window point)
  (setf (xlib:drawable-x window) (xpoint-x point))
  (setf (xlib:drawable-y window) (xpoint-y point)))

(define-attribute-setter window background)
(define-attribute-setter window backing-pixel)
(define-attribute-setter window backing-planes)
(define-attribute-setter window backing-store)
(define-attribute-setter window bit-gravity)
(define-attribute-setter window border)
(define-attribute-setter window colormap)

(define (x-set-window-cursor window cursor)
  (let ((val (if (null? cursor) :none cursor)))
    (setf (xlib:window-cursor window) val)))

(define-attribute-setter window do-not-propagate-mask)
(define-attribute-setter window event-mask)
(define-attribute-setter window gravity)
(define-attribute-setter window override-redirect)
(define-attribute-setter window priority)
(define-attribute-setter window save-under)

(define (x-query-tree window)
  (multiple-value-bind (children parent root)
		       (xlib:query-tree window)
     (make-h-tuple (list->haskell-list/identity children) parent root)))

(define (x-reparent-window window parent point)
  (xlib:reparent-window window parent (xpoint-x point) (xpoint-y point)))

(define (x-translate-coordinates source point dest)
  (xlib:translate-coordinates source (xpoint-x point) (xpoint-y point) dest))

(define (x-create-pixmap size depth drawable)
  (xlib:create-pixmap :width (xsize-w size)
		      :height (xsize-h size)
		      :depth depth
		      :drawable drawable))

(define (x-create-gcontext drawable attrs)
  (apply (function XLIB:CREATE-GCONTEXT)
	 `(:drawable ,drawable ,@(attrs->keywords attrs))))

(define (x-update-gcontext gcontext attrs)
  (do ((keys (attrs->keywords attrs) (cddr keys)))
      ((null? keys))
    (x-update-gcontext-attr gcontext (car keys) (cadr keys))))

(define (x-update-gcontext-attr gcontext key attr)
  (case key
    (:arc-mode (setf (xlib:gcontext-arc-mode gcontext) attr))
    (:background (setf (xlib:gcontext-background gcontext) attr))
    (:cap-style (setf (xlib:gcontext-cap-style gcontext) attr))
    (:fill-style (setf (xlib:gcontext-fill-style gcontext) attr))
    (:clip-mask (setf (xlib:gcontext-clip-mask gcontext) attr))
    (:clip-x (setf (xlib:gcontext-clip-x gcontext) attr))
    (:clip-y (setf (xlib:gcontext-clip-y gcontext) attr))
    (:dash-offset (setf (xlib:gcontext-dash-offset gcontext) attr))
    (:dashes (setf (xlib:gcontext-dashes gcontext) attr))
    (:exposures (setf (xlib:gcontext-exposures gcontext) attr))
    (:fill-rule (setf (xlib:gcontext-fill-rule gcontext) attr))
    (:font (setf (xlib:gcontext-font gcontext) attr))
    (:foreground (setf (xlib:gcontext-foreground gcontext) attr))
;    (:function (setf (xlib:gcontext-function gcontext) attr))
    (:join-style (setf (xlib:gcontext-join-style gcontext) attr))
    (:line-style (setf (xlib:gcontext-line-style gcontext) attr))
;    (:line-width (setf (xlib:gcontext-line-width gcontext) attr))
;    (:plane-mask (setf (xlib:gcontext-plane-mask gcontext) attr))
;    (:stipple (setf (xlib:gcontext-stipple gcontext) attr))
    (:subwindow-mode (setf (xlib:gcontext-subwindow-mode gcontext) attr))
;    (:tile (setf (xlib:gcontext-tile gcontext) attr))
;    (:ts-x (setf (xlib:gcontext-ts-x gcontext) attr))
;    (:ts-y (setf (xlib:gcontext-ts-y gcontext) attr))
    (else (format '#t "Graphics context attribute ~A is not settable.~%"
		  key))))

(define (x-query-best-stipple dsize drawable)
  (multiple-value-bind (w h) 
        (xlib:query-best-stipple (xsize-w dsize) (xsize-h dsize) drawable)
     (mk-xsize w h)))

(define (x-query-best-tile dsize drawable)
  (multiple-value-bind (w h) 
        (xlib:query-best-tile (xsize-w dsize) (xsize-h dsize) drawable)
     (mk-xsize w h)))

(define (x-clear-area window rect exposures-p)
  (xlib:clear-area window 
		   :x (xrect-x rect)
		   :y (xrect-y rect)
		   :width (xrect-w rect)
		   :height (xrect-h rect)
		   :exposures-p exposures-p))

(define (x-copy-area src gcontext rect dest point)
  (xlib:copy-area src 
		  gcontext 
		  (xrect-x rect) (xrect-y rect) 
		  (xrect-w rect) (xrect-h rect) 
		  dest 
		  (xpoint-x point) (xpoint-y point)))

(define (x-copy-plane src gcontext plane rect dest point)
  (xlib:copy-plane src 
		   gcontext 
		   plane 
		   (xrect-x rect) (xrect-y rect) 
		   (xrect-w rect) (xrect-h rect) 
		   dest 
		   (xpoint-x point) (xpoint-y point)))

(define (x-draw-point drawable gcontext point)
  (xlib:draw-point drawable gcontext (xpoint-x point) (xpoint-y point)))

(define (x-draw-points drawable gcontext points)
  (xlib:draw-points drawable gcontext (points->point-seq points)))

(define (points->point-seq points)
  (if (null? points)
      '()
      (let ((point (car points)))
	(lisp:list* (xpoint-x point)
		    (xpoint-y point)
		    (points->point-seq (cdr points))))))

(define (segments->point-seq segments)
  (if (null? segments)
      '()
      (let* ((first-pair (car segments))
	     (point-1 (force (tuple-select 2 0 first-pair)))
	     (point-2 (force (tuple-select 2 1 first-pair))))
	(lisp:list* (xpoint-x point-1)
		    (xpoint-y point-1) 
		    (xpoint-x point-2)
		    (xpoint-y point-2) 
		    (segments->point-seq (cdr segments))))))

(define (rects->point-seq rects)
  (if (null? rects)
      '()
      (let ((rect (car rects)))
	(lisp:list* (xrect-x rect)
		    (xrect-y rect)
		    (xrect-w rect)
		    (xrect-h rect)
		    (rects->point-seq (cdr rects))))))

(define (point-seq->rects point-seq)
  (if (null? point-seq)
      '()
      (cons (mk-xrect (car point-seq) (cadr point-seq) 
		      (caddr point-seq) (cadddr point-seq))
	    (point-seq->rects (cddddr point-seq)))))

(define (arcs->point-seq arcs)
  (if (null? arcs)
      '()
      (let ((arc (car arcs)))
	(lisp:list* (xarc-x arc)
		    (xarc-y arc)
		    (xarc-w arc)
		    (xarc-h arc)
		    (xarc-a1 arc)
		    (xarc-a2 arc)
		    (arcs->point-seq (cdr arcs))))))

(define (x-draw-line drawable gcontext point-1 point-2)
  (xlib:draw-line drawable gcontext (xpoint-x point-1) (xpoint-y point-1)
		  (xpoint-x point-2) (xpoint-y point-2)))

(define (x-draw-lines drawable gcontext points fill-p)
  (xlib:draw-lines drawable gcontext 
		   (points->point-seq points) :fill-p fill-p))

(define (x-draw-segments drawable gcontext segments)
  (xlib:draw-segments drawable gcontext (segments->point-seq segments)))

(define (x-draw-rectangle drawable gcontext rect fill-p)
  (xlib:draw-rectangle drawable gcontext
		       (xrect-x rect) (xrect-y rect) 
		       (xrect-w rect) (xrect-h rect)
		       fill-p))

(define (x-draw-rectangles drawable gcontext rects fill-p)
  (xlib:draw-rectangles drawable gcontext
			(rects->point-seq rects)
			fill-p))

(define (x-draw-arc drawable gcontext arc fill-p)
  (xlib:draw-arc drawable gcontext
		 (xarc-x arc) (xarc-y arc) 
		 (xarc-w arc) (xarc-h arc)
		 (xarc-a1 arc) (xarc-a2 arc)
		 fill-p))

(define (x-draw-arcs drawable gcontext arcs fill-p)
  (xlib:draw-arcs drawable gcontext
		  (arcs->point-seq arcs)
		  fill-p))

(define (x-draw-glyph drawable gcontext point element)
  (nth-value 1
	     (xlib:draw-glyph drawable gcontext (xpoint-x point) 
			      (xpoint-y point) element)))

(define (x-draw-glyphs drawable gcontext point element)
  (nth-value 1 (xlib:draw-glyphs drawable gcontext (xpoint-x point) 
				 (xpoint-y point) element)))

(define (x-draw-image-glyph drawable gcontext point element)
  (nth-value 1 (xlib:draw-image-glyph drawable gcontext (xpoint-x point) 
				      (xpoint-y point) element)))

(define (x-draw-image-glyphs drawable gcontext point element)
  (nth-value 1 (xlib:draw-image-glyphs drawable gcontext (xpoint-x point) 
				       (xpoint-y point) element)))

(define (x-image-size image)
  (mk-xsize (xlib:image-width image) (xlib:image-height image)))

(define (x-image-name image)
  (let ((lisp-name (xlib:image-name image)))
    (cond ((null? lisp-name) "")
	  ((symbol? lisp-name) (symbol->string lisp-name))
	  (else lisp-name))))
    
(define-attribute-setter image name)

(define (x-image-hot-spot image)
  (mk-xpoint (xlib:image-x-hot image) (xlib:image-y-hot image)))

(define (x-set-image-hot-spot image point)
  (setf (xlib:image-x-hot image) (xpoint-x point))
  (setf (xlib:image-y-hot image) (xpoint-y point)))

(define-attribute-setter image xy-bitmap-list)
(define-attribute-setter image z-bits-per-pixel)
(define-attribute-setter image z-pixarray)

(define (x-create-image attrs)
  (apply (function xlib:create-image) (attrs->keywords attrs)))

(define (x-copy-image image rect type)
  (xlib:copy-image image :x (xrect-x rect) :y (xrect-y rect)
		   :width (xrect-w rect) :height (xrect-h rect)
		   :result-type type))

(define (x-get-image drawable rect pmask format type)
  (xlib:get-image drawable :x (xrect-x rect) :y (xrect-y rect)
		  :width (xrect-w rect) :height (xrect-h rect)
		  :plane-mask pmask :format format :result-type type))

(define (x-put-image drawable gcontext image point rect)
  (xlib:put-image drawable gcontext image 
		  :src-x (xpoint-x point) :src-y (xpoint-y point)
		  :x (xrect-x rect) :y (xrect-y rect)
		  :width (xrect-w rect) :height (xrect-h rect)))

(define (x-get-raw-image drawable rect pmask format)
  (xlib:get-raw-image drawable 
		      :x (xrect-x rect) :y (xrect-y rect) 
		      :width (xrect-w rect) :height (xrect-h rect)
		      :plane-mask pmask :format format))

(define (x-put-raw-image drawable gcontext data depth rect left-pad format)
  (xlib:put-raw-image drawable gcontext data
		      :depth depth 
		      :x (xrect-x rect) :y (xrect-y rect) 
		      :width (xrect-w rect) :height (xrect-h rect)
		      :left-pad left-pad :format format))

(define (x-font-name font)
  (let ((lisp-name (xlib:font-name font)))
    (cond ((null? lisp-name) "")
	  ((symbol? lisp-name) (symbol->string lisp-name))
	  (else lisp-name))))

(define (x-alloc-color colormap color)
  (multiple-value-bind (pixel screen-color exact-color)
       (xlib:alloc-color colormap color)
     (make-h-tuple pixel screen-color exact-color)))

(define (x-alloc-color-cells colormap colors planes contiguous-p)
  (multiple-value-bind (pixels mask)
       (xlib:alloc-color-cells colormap colors :planes planes 
			       :contiguous-p contiguous-p)
     (make-h-tuple (list->haskell-list/identity pixels) (list->haskell-list/identity mask))))

(define (x-alloc-color-planes colormap colors reds greens blues contiguous-p)
  (multiple-value-bind (pixels red-mask green-mask blue-mask)
       (xlib:alloc-color-planes colormap colors :reds reds :greens greens
				:blues blues :contiguous-p contiguous-p)
     (make-h-tuple (list->haskell-list/identity pixels) 
		   red-mask
		   green-mask
		   blue-mask)))

(define (x-lookup-color colormap name)
  (multiple-value-bind (screen-color exact-color)
      (xlib:lookup-color colormap name)
    (make-h-tuple screen-color exact-color)))

(define (unzip l)
  (if (null? l)
      '()
      (let ((h (car l)))
	(lisp:list* (force (tuple-select 2 0 h))
		    (force (tuple-select 2 1 h))
		    (unzip (cdr l))))))

(define (x-store-colors colormap pixel-colors)
  (xlib:store-colors colormap (unzip pixel-colors)))

(define (x-create-cursor source mask point foreground background)
  (apply (function xlib:create-cursor)
	 `(:source ,source
	   ,@(if mask `(:mask ,mask) '())
	   :x ,(xpoint-x point) :y ,(xpoint-y point)
	   :foreground ,foreground :background ,background)))

(define (x-create-glyph-cursor src mask foreground background)
  (apply (function xlib:create-glyph-cursor)
	 `(:source-font ,(force (tuple-select 2 0 src))
	   :source-char ,(integer->char (force (tuple-select 2 1 src)))
	   ,@(if mask 
		 `(:mask-font ,(force (tuple-select 2 0 mask))
		 :mask-char ,(integer->char (force (tuple-select 2 1 mask))))
		 '())
	   :foreground ,foreground :background ,background)))

(define (x-query-best-cursor size display)
  (multiple-value-bind (w h)
      (xlib:query-best-cursor (xsize-w size) (xsize-h size) display)
    (mk-xsize w h)))

(define (x-change-property window property content)
  (xlib:change-property window property 
			(car content) (cadr content) 
			(caddr content)))

(define (x-get-property window property)
  (lisp:multiple-value-bind (data type format) 
			    (xlib:get-property window property)
	 (list (sequence->list data) type format)))

(define (x-convert-selection selection type requestor property time)
  (apply (function xlib:convert-selection)
	 `(,selection ,type ,requestor ,property ,@(if time `(,time) '()))))

(define (x-set-selection-owner display selection time owner)
  (if time
      (setf (xlib:selection-owner display selection time) owner)
      (setf (xlib:selection-owner display selection) owner)))

(define (sequence->list seq)
  (if (list? seq) seq
      (do ((i (1- (lisp:length seq)) (1- i))
	   (res '() (cons (lisp:elt seq i) res)))
	  ((< i 0) res))))

(define *this-event* '())

(define (translate-event lisp:&rest event-slots lisp:&key event-key 
			 lisp:&allow-other-keys)
  (setf *this-event* (cons event-key event-slots))
  '#t)


(define (x-get-event display)
  (xlib:process-event display :handler #'translate-event :force-output-p '#t)
  *this-event*)

(define (x-queue-event display event append-p)
  (apply (function xlib:queue-event)
	 `(,display ,(car event) ,@(cdr event) :append-p ,append-p)))

(define (x-event-listen display)
  (let ((res (xlib:event-listen display)))
    (if (null? res) 0 res)))

(define (x-send-event window event mask)
  (apply (function xlib:send-event)
	 `(,window ,(car event) ,mask ,@(cdr event))))

(define (x-global-pointer-position display)
  (multiple-value-bind (x y) (xlib:global-pointer-position display)
    (mk-xpoint x y)))

(define (x-pointer-position window)
  (multiple-value-bind (x y same) (xlib:pointer-position window)
    (if same (mk-xpoint x y) '())))

(define (x-motion-events window start stop)
  (do ((npos '() (cons (mk-xpoint (car pos) (cadr pos)) npos))
       (pos (xlib:motion-events window :start start :stop stop) 
	    (cdddr pos)))
      ((null? pos) (nreverse npos))))

(define (x-warp-pointer dest-win point)
  (xlib:warp-pointer dest-win (xpoint-x point) (xpoint-y point)))

(define (x-set-input-focus display focus revert-to time)
  (apply (function xlib:set-input-focus)
	 `(,display ,focus ,revert-to ,@(if time `(,time) '()))))

(define (x-input-focus display)
  (multiple-value-bind (focus revert-to) (xlib:input-focus display)
    (make-h-tuple focus revert-to)))

(define (x-grab-pointer window event-mask attrs time)
  (apply (function xlib:grab-pointer)
	 `(,window ,event-mask
           ,@(attrs->keywords attrs)
	   ,@(if time `(:time ,time) '()))))

(define (x-ungrab-pointer display time)
  (if time
      (xlib:ungrab-pointer display :time time)
      (xlib:ungrab-pointer display)))
      
(define (x-change-active-pointer-grab display event-mask attrs time)
  (apply (function xlib:change-active-pointer-grab)
	 `(,display ,event-mask
           ,@(attrs->keywords attrs)
	   ,@(if time `(,time) '()))))

(define (x-grab-button window button event-mask state-mask attrs)
  (apply (function xlib:grab-button)
	 `(,window ,button ,event-mask :modifiers ,state-mask
	   ,@(attrs->keywords attrs))))

(define (x-ungrab-button window button modifiers)
  (xlib:ungrab-button window button :modifiers modifiers))

(define (x-grab-keyboard window attrs time)
  (apply (function xlib:grab-keyboard)
	 `(,window ,@(attrs->keywords attrs)
	   ,@(if time `(:time ,time) '()))))

(define (x-ungrab-keyboard display time)
  (if time
      (xlib:ungrab-keyboard display :time time)
      (xlib:ungrab-keyboard display)))
      
(define (x-grab-key window key state-mask attrs)
  (apply (function xlib:grab-key)
	 `(,window ,key :modifiers ,state-mask ,@(attrs->keywords attrs))))

(define (x-ungrab-key window key modifiers)
  (xlib:ungrab-button window key :modifiers modifiers))

(define (x-set-pointer-acceleration display val)
  (xlib:change-pointer-control display :acceleration val))

(define (x-set-pointer-threshold display val)
  (xlib:change-pointer-control display :threshold val))

(define (x-pointer-acceleration display)
  (lisp:coerce (nth-value 0 (xlib:pointer-control display)) 
	       'lisp:single-float))

(define (x-pointer-threshold display)
  (lisp:coerce (nth-value 1 (xlib:pointer-control display)) 
	       'lisp:single-float))

(define-attribute-setter pointer mapping)

(define (x-set-keyboard-key-click-percent display v)
  (xlib:change-keyboard-control display :key-click-percent v))

(define (x-set-keyboard-bell-percent display v)
  (xlib:change-keyboard-control display :bell-percent v))

(define (x-set-keyboard-bell-pitch display v)
  (xlib:change-keyboard-control display :bell-pitch v))

(define (x-set-keyboard-bell-duration display v)
  (xlib:change-keyboard-control display :bell-duration v))


;;; Yes, leds are really counted from 1 rather than 0.

(define (x-set-keyboard-led display v)
  (declare (type integer v))
  (do ((led 1 (1+ led))
       (vv v (lisp:ash vv -1)))
      ((> led 32))
      (declare (type fixnum led) (type integer vv))
      (xlib:change-keyboard-control display
        :led led
	:led-mode (if (lisp:logand vv 1) :on :off))))

(define (x-set-keyboard-auto-repeat-mode display v)
  (do ((key 0 (1+ key)))
      ((>= key (lisp:length v)))
      (declare (type fixnum key))
      (xlib:change-keyboard-control display
        :key key
	:auto-repeat-mode (if (eqv? (the fixnum (lisp:aref v key)) 1) :on :off)
	)))

(define (x-keyboard-key-click-percent display)
  (nth-value 0 (xlib:keyboard-control display)))

(define (x-keyboard-bell-percent display)
  (nth-value 1 (xlib:keyboard-control display)))

(define (x-keyboard-bell-pitch display)
  (nth-value 2 (xlib:keyboard-control display)))

(define (x-keyboard-bell-duration display)
  (nth-value 3 (xlib:keyboard-control display)))

(define (x-keyboard-led display)
  (nth-value 4 (xlib:keyboard-control display)))

(define (x-keyboard-auto-repeat-mode display)
  (nth-value 6 (xlib:keyboard-control display)))

(define (x-modifier-mapping display)
  (lisp:multiple-value-list (xlib:modifier-mapping display)))

(define (x-set-modifier-mapping display l)
  (let ((l1 (cddddr l)))
    (xlib:set-modifier-mapping display 
			       :shift (car l)
			       :lock (cadr l)
			       :control (caddr l)
			       :mod1 (cadddr l)
			       :mod2 (car l1)
			       :mod3 (cadr l1)
			       :mod4 (caddr l1)
			       :mod5 (cadddr l1))))

(define (x-keysym-character display keysym state)
  (let ((res (xlib:keysym->character display keysym state)))
    (if (char? res) (char->integer res) '())))

(define (x-keycode-character display keycode state)
  (let ((res (xlib:keycode->character display keycode state)))
    (if (char? res) (char->integer res) '())))

(define-attribute-setter close-down mode)

(define-attribute-setter access control)

(define (x-screen-saver display)
  (lisp:multiple-value-list (xlib:screen-saver display)))

(define (x-set-screen-saver display ss)
  (xlib:set-screen-saver display (car ss) (cadr ss) (caddr ss) (cadddr ss)))

(define (slots->keywords slots)
  (if (null slots) '()
      `(,@(slot->keyword (car slots)) ,@(slots->keywords (cdr slots)))))

(define (slot->keyword slot)
  (let* ((tag (keyword-key slot))
	 (val (keyword-val slot)))
    (case tag
      (:pos `(:x ,(xpoint-x val) :y ,(xpoint-y val)))
      (:root-pos `(:root-x ,(xpoint-x val) :root-y ,(xpoint-y val)))
      (:size `(:width ,(xsize-w val) :height ,(xsize-h val)))
      (:rect `(:x ,(xrect-x val) :y ,(xrect-y val)
	       :width ,(xrect-w val) :height ,(xrect-h val)))
      (:graph-fun `(:major ,(car val) :minor ,(cdr val)))
      (:visibility `(:state ,val))
      (:property-state `(:state ,val))
      (:message `(:data ,(car val) :type ,(cadr val) :format ,(caddr val)))
      (else `(,tag ,val)))))

(define (keywords->slots type keywords event)
  (let* ((slots (keywords->slots1 type keywords))
	 (has-root-xy (memq type '(:key-press :key-release :button-press 
					      :button-release :motion-notify 
					      :enter-notify :leave-notify)))
	 (has-xy (or has-root-xy 
		     (memq type '(:gravity-notify :reparent-notify))))
	 (has-graph-fun (memq type '(:graphics-exposure :no-exposure)))
	 (has-rect (memq type '(:exposure :graphics-exposure 
					  :configure-notify
					  :create-notify :configure-request)))
	 (has-size (memq type '(:resize-request)))
	 (has-message (memq type '(:client-message))))
    (when has-xy
      (push (make-keyword :pos (x-event-pos event)) slots))
    (when has-root-xy
      (push (make-keyword :root-pos (x-event-root-pos event))	slots))
    (when has-graph-fun
      (push (make-keyword :graph-fun (x-event-graph-fun event)) slots))
    (when has-rect
      (push (make-keyword :rect (x-event-rect event))	slots))
    (when has-size
      (push (make-keyword :size (x-event-size event))	slots))
    (when has-message
      (push (make-keyword :message (x-event-message event)) slots))
    slots))
      
(define (keywords->slots1 type keywords)
  (if (null? keywords)
      '()
      (if (memq (car keywords) 
		'(:x :y :width :height :root-x :root-y 
		     :major :minor :type :data :format))
	  (keywords->slots1 type (cddr keywords))
	  (cons (keyword->slot type (car keywords) (cadr keywords))
		(keywords->slots1 type (cddr keywords))))))

(define (keyword->slot type slot val)
  (if (eq? slot :state)
      (case type
	(:property-state (make-keyword :property-state val))
	(:visibility (make-keyword :visibility val))
	(else (make-keyword :state val)))
      (make-keyword slot val)))
		 
(define (attrs->keywords attrs)
  (if (null attrs)
      '()
      (nconc (attr->keyword (car attrs))
	     (attrs->keywords (cdr attrs)))))

(define (attr->keyword attr)
  (let* ((tag (keyword-key attr))
	 (val (keyword-val attr)))
    (case tag
      (:clip-origin `(:clip-x ,(xpoint-x val) :clip-y ,(xpoint-y val)))
      (:dashes `(,tag ,(haskell-list->list/identity val)))
      (:tile-origin `(:ts-x ,(xpoint-x val) :ts-y ,(xpoint-y val)))
      (:size `(:width ,(xsize-w val) :height ,(xsize-h val)))
      (:name `(:name ,(haskell-string->string val)))
      (:hot-spot `(:x-hot ,(xpoint-x val) :y-hot ,(xpoint-y val)))
      (else `(,tag ,val)))))

(define (x-mutable-array-create inits)
  (list->vector inits))

(define (x-mutable-array-lookup a i)
  (vector-ref a i))

(define (x-mutable-array-update a i x)
  (setf (vector-ref a i) x))

(define (x-mutable-array-length a)
  (vector-length a))

(define (get-time-zone)
  (nth-value 8 (lisp:get-decoded-time)))

(define (decode-time time zone)
  (multiple-value-bind (sec min hour date mon year week ds-p)
		       (if zone
			   (lisp:decode-universal-time time zone)
			   (lisp:decode-universal-time time))   
    (make-h-tuple
      (list->haskell-list/identity (list sec min hour date mon year week))
      ds-p)))

(define (encode-time time zone)
  (apply (function lisp:encode-universal-time)
	 (if (null? zone) time (append time (list zone)))))

(define (get-run-time)
  (/ (lisp:coerce (lisp:get-internal-run-time) 'lisp:single-float)
     (lisp:coerce lisp:internal-time-units-per-second 'lisp:single-float)))

(define (get-elapsed-time)
  (/ (lisp:coerce (lisp:get-internal-real-time) 'lisp:single-float)
     (lisp:coerce lisp:internal-time-units-per-second 'lisp:single-float)))

(define (prim.thenio---1 x fn)
  (lambda (state)
    (declare (ignore state))
    (let ((res (funcall x (box 'state))))
      (format '#t "~A~%" res)
      (funcall fn res (box 'state)))))

(define-attribute-setter wm name)
(define-attribute-setter wm icon-name)
