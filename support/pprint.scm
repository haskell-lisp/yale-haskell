;;; pprint.scm -- xp pretty-printer in Scheme
;;;
;;; author :  Sandra Loosemore
;;; date   :  29 Oct 1991
;;;
;;;
;;; This code is adapted from the XP pretty printer originally written
;;; in Common Lisp by Dick Waters.  Here is the copyright notice attached
;;; to the original XP source file:
;;;
;;;------------------------------------------------------------------------
;;;
;;; Copyright 1989,1990 by the Massachusetts Institute of Technology,
;;; Cambridge, Massachusetts.
;;; 
;;; Permission to use, copy, modify, and distribute this software and its
;;; documentation for any purpose and without fee is hereby granted,
;;; provided that this copyright and permission notice appear in all
;;; copies and supporting documentation, and that the name of M.I.T. not
;;; be used in advertising or publicity pertaining to distribution of the
;;; software without specific, written prior permission. M.I.T. makes no
;;; representations about the suitability of this software for any
;;; purpose.  It is provided "as is" without express or implied warranty.
;;; 
;;;  M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
;;;  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
;;;  M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
;;;  ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
;;;  WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
;;;  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
;;;  SOFTWARE.
;;;
;;;------------------------------------------------------------------------
;;;


;;;=====================================================================
;;; Variables
;;;=====================================================================


;;; External variables.  These may be specially bound by user code.

(define *print-escape*           '#t)
(define *print-circle*           '#f)
(define *print-level*            '#f)
(define *print-length*           '#f)
(define *print-base*             10)
(define *print-radix*            '#f)


(define *print-shared*           '#f)
(define *print-pretty*           '#f)
(define *print-right-margin*     '#f)
(define *print-miser-width*      40)
(define *print-lines*            '#f)
(define *default-right-margin*   70)
(define *last-abbreviated-printing*
  (lambda maybe-stream
    (declare (ignore maybe-stream))
    '#f))

(define *print-dispatch*         '#f)  ; initialized later
(define *print-structure*        '#f)
(define *print-structure-slots*  '#t)


;;; *** These variables aren't really supported, but they should be.

(define *print-readably*         '#f)
(define *print-case*             'upcase)



;;; Internal variables.  These are all specially rebound when we initiate
;;; printing to an XP stream.

(define *xp.current-level* 0)
(define *xp.current-length* 0)
(define *xp.abbreviation-happened* '#f)
(define *xp.locating-circularities* '#f)
(define *xp.parents* '())
(define *xp.circularity-hash-table* '#f)
(define *xp.line-limit-abbreviation-exit*
  (lambda values
    (declare (ignore values))
    (error "No line limit abbreviation exit in this extent.")))



;;;=====================================================================
;;; Dispatching
;;;=====================================================================

;;; Since Scheme doesn't have type specifiers or named structures,
;;; the dispatch mechanism defined for the Common Lisp XP won't work
;;; very well.  A more general alternative might be to maintain a
;;; sorted list of <priority predicate printer> tuples, but having to
;;; try each of these in sequence could get very slow.
;;;
;;; What I've decided to to instead is to have the value of
;;; *print-dispatch* be a user-defined dispatcher
;;; function:  given an object, it should return a function to print it,
;;; or #f.  In the latter case, the object is printed in some default
;;; way.
;;;
;;; The standard dispatcher function is defined towards the bottom
;;; of this file.  If you are writing your own dispatcher, you should
;;; probably call this function as the fall-through case.

(define (xp.get-printer object)
  (funcall (dynamic *print-dispatch*) object))


;;;=====================================================================
;;; Internal data structures
;;;=====================================================================

(define-integrable xp.block-stack-entry-size 1)
(define-integrable xp.prefix-stack-entry-size 5)
(define-integrable xp.queue-entry-size 7)
(define-integrable xp.buffer-entry-size 1)
(define-integrable xp.prefix-entry-size 1)
(define-integrable xp.suffix-entry-size 1)

(define-integrable xp.block-stack-min-size (* 35 xp.block-stack-entry-size))
(define-integrable xp.prefix-stack-min-size (* 30 xp.prefix-stack-entry-size))
(define-integrable xp.queue-min-size (* 75 xp.queue-entry-size))
(define-integrable xp.buffer-min-size 256)
(define-integrable xp.prefix-min-size 256)
(define-integrable xp.suffix-min-size 256)


;;; The xp stream structure.
;;; Fields without defaults are initialized by xp.initialize-xp, below.

(define-struct xp
  (prefix xp.)
  (predicate xp.xp-structure-p)
  (slots
   (base-stream (type t) (default '#f))
   (linel (type fixnum) (default 0))
   (line-limit (type (maybe fixnum)) (default '#f))
   (line-no (type fixnum) (default 0))
   (char-mode (type (enum #f up down cap0 cap1 capw)) (default '#f))
   (char-mode-counter (type fixnum) (default 0))
   ;; number of logical blocks at qright that are started but not ended.
   (depth-in-blocks (type fixnum) (default 0))
   ;; This stack is pushed and popped in accordance with the way blocks
   ;; are nested at the moment they are entered into the queue.
   (block-stack (type vector) (default (make-vector xp.block-stack-min-size)))
   ;; Pointer into block-stack vector.
   (block-stack-ptr (type fixnum) (default 0))
   ;; This is a string that builds up the line images that will be printed out.
   (buffer (type string) (default (make-string xp.buffer-min-size)))
   ;; The output character position of the first character in the buffer;
   ;; nonzero only if a partial line has been output.
   (charpos (type fixnum) (default 0))
   ;; The index in the buffer where the next character is to be inserted.
   (buffer-ptr (type fixnum) (default 0))
   ;; This is used in computing total lengths.  It is changed to reflect
   ;; all shifting and insertion of prefixes so that total length computes
   ;; things as they would be if they were all on one line.
   (buffer-offset (type fixnum) (default 0))
   ;; The queue of action descriptors.  The value is a vector.
   (queue (type vector) (default (make-vector xp.queue-min-size)))
   ;; Index of next queue entry to dequeue.
   (qleft (type fixnum) (default 0))
   ;; Index of last entry queued; queue is empty when (> qleft qright).
   (qright (type fixnum) (default 0))
   ;; This stores the prefix that should be used at the start of the line.
   (prefix (type string) (default (make-string xp.buffer-min-size)))
   ;; This stack is pushed and popped in accordance with the way blocks
   ;; are nested at the moment things are taken off the queue and printed.
   (prefix-stack (type vector) (default (make-vector xp.prefix-stack-min-size)))
   ;; Index into prefix-stack.
   (prefix-stack-ptr (type fixnum) (default 0))
   ;; This stores the suffixes that have to be pritned to close of the
   ;; current open blocks.  For convenience in popping, the whole suffix
   ;; is stored in reverse order.
   (suffix (type string) (default (make-string xp.buffer-min-size)))
   ))


(define (xp.make-xp-structure)
  (make xp))


;;; Positions within the buffer are kept in three ways:
;;; * Buffer position (eg BUFFER-PTR)
;;; * Line position (eg (+ BUFFER-PTR CHARPOS)).
;;;   Indentations are stored in this form.
;;; * Total position if all on one line (eg (+ BUFFER-PTR BUFFER-OFFSET))
;;;   Positions are stored in this form.

(define-local-syntax (xp.lp<-bp xp . maybe-ptr)
  (let ((ptr  (if (not (null? maybe-ptr))
		  (car maybe-ptr)
		  `(xp.buffer-ptr ,xp))))
    `(+ ,ptr (xp.charpos ,xp))))

(define-local-syntax (xp.tp<-bp xp)
  `(+ (xp.buffer-ptr ,xp) (xp.buffer-offset ,xp)))

(define-local-syntax (xp.bp<-lp xp ptr)
  `(- ,ptr (xp.charpos ,xp)))

(define-local-syntax (xp.bp<-tp xp ptr)
  `(- ,ptr (xp.buffer-offset ,xp)))

(define-local-syntax (xp.lp<-tp xp ptr)
  `(xp.lp<-bp ,xp (xp.bp<-tp ,xp ,ptr)))


;;; Define some macros for growing the various stacks in the xp-structure.

(define-local-syntax (xp.check-block-stack-size xp ptr)
  `(setf (xp.block-stack ,xp)
	 (xp.grow-vector (xp.block-stack ,xp) ,ptr xp.block-stack-entry-size)))

(define-local-syntax (xp.check-prefix-size xp ptr)
  `(setf (xp.prefix ,xp)
	 (xp.grow-string (xp.prefix ,xp) ,ptr xp.prefix-entry-size)))

(define-local-syntax (xp.check-prefix-stack-size xp ptr)
  `(setf (xp.prefix-stack ,xp)
	 (xp.grow-vector (xp.prefix-stack ,xp) ,ptr xp.prefix-stack-entry-size)))

(define-local-syntax (xp.check-queue-size xp ptr)
  `(setf (xp.queue ,xp)
	 (xp.grow-vector (xp.queue ,xp) ,ptr xp.queue-entry-size)))

(define-local-syntax (xp.check-buffer-size xp ptr)
  `(setf (xp.buffer ,xp)
	 (xp.grow-string (xp.buffer ,xp) ,ptr xp.buffer-entry-size)))

(define-local-syntax (xp.check-suffix-size xp ptr)
  `(setf (xp.suffix ,xp)
	 (xp.grow-string (xp.suffix ,xp) ,ptr xp.suffix-entry-size)))

(define (xp.grow-vector old ptr entry-size)
  (let ((end  (vector-length old)))
    (if (> ptr (- end entry-size))
	(let ((new  (make-vector (+ ptr 50))))
	  (dotimes (i end)
	    (setf (vector-ref new i) (vector-ref old i)))
	  new)
	old)))

(define (xp.grow-string old ptr entry-size)
  (let ((end  (string-length old)))
    (if (> ptr (- end entry-size))
	(let ((new  (make-string (+ ptr 50))))
	  (dotimes (i end)
	    (setf (string-ref new i) (string-ref old i)))
	  new)
	old)))



;;; Things for manipulating the block stack.

(define-local-syntax (xp.section-start xp)
  `(vector-ref (xp.block-stack ,xp) (xp.block-stack-ptr ,xp)))

(define (xp.push-block-stack xp)
  (incf (xp.block-stack-ptr xp) xp.block-stack-entry-size)
  (xp.check-block-stack-size xp (xp.block-stack-ptr xp)))

(define (xp.pop-block-stack xp)
  (decf (xp.block-stack-ptr xp) xp.block-stack-entry-size))


;;; Prefix stack manipulations

(define-local-syntax (xp.prefix-ptr xp)
  `(vector-ref (xp.prefix-stack ,xp) (xp.prefix-stack-ptr ,xp)))
(define-local-syntax (xp.suffix-ptr xp)
  `(vector-ref (xp.prefix-stack ,xp) (+ (xp.prefix-stack-ptr ,xp) 1)))
(define-local-syntax (non-blank-prefix-ptr xp)
  `(vector-ref (xp.prefix-stack ,xp) (+ (xp.prefix-stack-ptr ,xp) 2)))
(define-local-syntax (initial-prefix-ptr xp)
  `(vector-ref (xp.prefix-stack ,xp) (+ (xp.prefix-stack-ptr ,xp) 3)))
(define-local-syntax (xp.section-start-line xp)
  `(vector-ref (xp.prefix-stack ,xp) (+ (xp.prefix-stack-ptr ,xp) 4)))

(define (xp.push-prefix-stack xp)
  (let ((old-prefix 0)
	(old-suffix 0)
	(old-non-blank 0))
    (when (not (negative? (xp.prefix-stack-ptr xp)))
      (setf old-prefix (xp.prefix-ptr xp))
      (setf old-suffix (xp.suffix-ptr xp))
      (setf  old-non-blank (non-blank-prefix-ptr xp)))
    (incf (xp.prefix-stack-ptr xp) xp.prefix-stack-entry-size)
    (xp.check-prefix-stack-size xp (xp.prefix-stack-ptr xp))
    (setf (xp.prefix-ptr xp) old-prefix)
    (setf (xp.suffix-ptr xp) old-suffix)
    (setf (non-blank-prefix-ptr xp) old-non-blank)))

(define (xp.pop-prefix-stack xp)
  (decf (xp.prefix-stack-ptr xp) xp.prefix-stack-entry-size))


;;; The queue entries have several parts:
;;; QTYPE one of :NEWLINE/:IND/:START-BLOCK/:END-BLOCK
;;; QKIND :LINEAR/:MISER/:FILL/:MANDATORY or :UNCONDITIONAL/:FRESH
;;;  or :BLOCK/:CURRENT
;;; QPOS total position corresponding to this entry
;;; QDEPTH depth in blocks of this entry.
;;; QEND offset to entry marking end of section this entry starts.
;;   (NIL until known.)
;;;  Only :start-block and non-literal :newline entries can start sections.
;;; QOFFSET offset to :END-BLOCK for :START-BLOCK (NIL until known).
;;; QARG for :IND indentation delta
;;;      for :START-BLOCK suffix in the block if any.
;;;                       or if per-line-prefix then cons of suffix and
;;;                       per-line-prefix.
;;;      for :END-BLOCK suffix for the block if any.

(define-local-syntax (xp.qtype   xp index)
  `(vector-ref (xp.queue ,xp) ,index))
(define-local-syntax (xp.qkind   xp index)
  `(vector-ref (xp.queue ,xp) (1+ ,index)))
(define-local-syntax (xp.qpos    xp index)
  `(vector-ref (xp.queue ,xp) (+ ,index 2)))
(define-local-syntax (xp.qdepth  xp index)
  `(vector-ref (xp.queue ,xp) (+ ,index 3)))
(define-local-syntax (xp.qend    xp index)
  `(vector-ref (xp.queue ,xp) (+ ,index 4)))
(define-local-syntax (xp.qoffset xp index)
  `(vector-ref (xp.queue ,xp) (+ ,index 5)))
(define-local-syntax (xp.qarg    xp index)
  `(vector-ref (xp.queue ,xp) (+ ,index 6)))

;;; we shift the queue over rather than using a circular queue because
;;; that works out to be a lot faster in practice.  Note, short printout
;;; does not ever cause a shift, and even in long printout, the queue is
;;; shifted left for free every time it happens to empty out.

(define (xp.enqueue xp type kind . maybe-arg)
  (incf (xp.qright xp) xp.queue-entry-size)
  (when (> (xp.qright xp) (- xp.queue-min-size xp.queue-entry-size))
    (vector-replace (xp.queue xp) (xp.queue xp) 0 (xp.qleft xp) (xp.qright xp))
    (setf (xp.qright xp) (- (xp.qright xp) (xp.qleft xp)))
    (setf (xp.qleft xp) 0))
  (xp.check-queue-size xp (xp.qright xp))
  (setf (xp.qtype xp (xp.qright xp)) type)
  (setf (xp.qkind xp (xp.qright xp)) kind)
  (setf (xp.qpos xp (xp.qright xp)) (xp.tp<-bp xp))
  (setf (xp.qdepth xp (xp.qright xp)) (xp.depth-in-blocks xp))
  (setf (xp.qend xp (xp.qright xp)) '#f)
  (setf (xp.qoffset xp (xp.qright xp)) '#f)
  (setf (xp.qarg xp (xp.qright xp)) (car maybe-arg)))

(define-local-syntax (xp.qnext index) `(+ ,index xp.queue-entry-size))



;;; Print routine for xp structures
;;; *** this is broken, it uses unimplemented format options.

(define *xp.describe-xp-streams-fully* '#f)

(define (xp.describe-xp xp . maybe-stream)
  (let ((s  (if (not (null? maybe-stream))
		(car maybe-stream)
		(current-output-port))))
    (format s "#<XP stream ")
    (if (not (xp.base-stream xp))
	(format s "not currently in use")
	(begin
 	  (format s "outputting to ~S" (xp.base-stream xp))
	  (format s "~&buffer= ~S"
		  (substring (xp.buffer xp) 0 (max (xp.buffer-ptr xp) 0)))
	  (if (not (dynamic *xp.describe-xp-streams-fully*))
	      (format s " ...")
	      (begin
	        (format s "~&   pos   _123456789_123456789_123456789_123456789")
		(format s "~&depth-in-blocks= ~D linel= ~D line-no= ~D line-limit= ~D"
			(xp.depth-in-blocks xp) (xp.linel xp)
			(xp.line-no xp) (xp.line-limit xp))
		(when (or (xp.char-mode xp) (not (zero? (xp.char-mode-counter xp))))
		  (format s "~&char-mode= ~S char-mode-counter= ~D"
			  (xp.char-mode xp) (xp.char-mode-counter xp)))
		(unless (negative? (xp.block-stack-ptr xp))
		  (format s "~&section-start")
		  (do ((save (xp.block-stack-ptr xp)))
		      ((negative? (xp.block-stack-ptr xp))
		       (setf (xp.block-stack-ptr xp) save))
		      (format s " ~D" (xp.section-start xp))
		      (xp.pop-block-stack xp)))
		(format s "~&linel= ~D charpos= ~D buffer-ptr= ~D buffer-offset= ~D"
			(xp.linel xp) (xp.charpos xp)
			(xp.buffer-ptr xp) (xp.buffer-offset xp))
		(unless (negative? (xp.prefix-stack-ptr xp))
		  (format s "~&prefix= ~S"
			  (substring (xp.prefix xp) 0 (max (xp.prefix-ptr xp) 0)))
		  (format s "~&suffix= ~S"
			  (substring (xp.suffix xp) 0 (max (xp.suffix-ptr xp) 0))))
		(unless (> (xp.qleft xp) (xp.qright xp))
		  (format s "~&ptr type         kind           pos depth end offset arg")
		  (do ((p (xp.qleft xp) (xp.qnext p)))
		      ((> p (xp.qright xp)))
		      (format s "~&~4A~13A~15A~4A~6A~4A~7A~A"
			      (/ (- p (xp.qleft xp)) xp.queue-entry-size)
			      (xp.qtype xp p)
			      (if (memq (xp.qtype xp p) '(newline ind))
				  (xp.qkind xp p)
				  "")
			      (xp.bp<-tp xp (xp.qpos xp p))
			      (xp.qdepth xp p)
			      (if (not (memq (xp.qtype xp p)
					     '(newline start-block)))
				  ""
				  (and (xp.qend xp p)
				       (/ (- (+ p (xp.qend xp p)) (xp.qleft xp))
					  xp.queue-entry-size)))
			      (if (not (eq? (xp.qtype xp p) 'start-block))
				  ""
				  (and (xp.qoffset xp p)
				       (/ (- (+ p (xp.qoffset xp p)) (xp.qleft xp))
					  xp.queue-entry-size)))
			      (if (not (memq (xp.qtype xp p)
					     '(ind start-block end-block)))
				  ""
				  (xp.qarg xp p)))))
		(unless (negative? (xp.prefix-stack-ptr xp))
		  (format s "~&initial-prefix-ptr prefix-ptr suffix-ptr non-blank start-line")
		  (do ((save (xp.prefix-stack-ptr xp)))
		      ((negative? (xp.prefix-stack-ptr xp))
		       (setf (xp.prefix-stack-ptr xp) save))
		      (format s "~& ~19A~11A~11A~10A~A"
			      (initial-prefix-ptr xp)
			      (xp.prefix-ptr xp)
			      (xp.suffix-ptr xp)
			      (non-blank-prefix-ptr xp)
			      (xp.section-start-line xp))
		      (xp.pop-prefix-stack xp)))))))
    (format s ">")))



;;; Allocation of XP structures

;;; This maintains a list of XP structures.  We save them
;;; so that we don't have to create new ones all of the time.
;;; We have separate objects so that many can be in use at once
;;; (e.g. for printing to multiple streams).

(define xp.free-xps '())

(define (xp.get-pretty-print-stream stream)
  (xp.initialize-xp
      (if (not (null? xp.free-xps))
	  (pop xp.free-xps)
	  (xp.make-xp-structure))
      stream))


;;; If you call this, the xp-stream gets efficiently recycled.

(define (xp.free-pretty-print-stream xp)
  (setf (xp.base-stream xp) '#f)
  (if (not (memq xp xp.free-xps))
      (push xp xp.free-xps)))


;;; This is called to initialize things when you start pretty printing.

(define (xp.initialize-xp xp stream)
  (setf (xp.base-stream xp) stream)
  (setf (xp.linel xp)
	(max 0
	     (cond ((dynamic *print-right-margin*))
		   ((internal-output-width stream))
		   (else (dynamic *default-right-margin*)))))
  (setf (xp.line-limit xp) (dynamic *print-lines*))
  (setf (xp.line-no xp) 1)
  (setf (xp.char-mode xp) '#f)
  (setf (xp.char-mode-counter xp) 0)
  (setf (xp.depth-in-blocks xp) 0)
  (setf (xp.block-stack-ptr xp) 0)
  (setf (xp.charpos xp) (or (internal-output-position stream) 0))
  (setf (xp.section-start xp) 0)
  (setf (xp.buffer-ptr xp) 0)
  (setf (xp.buffer-offset xp) (xp.charpos xp))
  (setf (xp.qleft xp) 0)
  (setf (xp.qright xp) (- xp.queue-entry-size))
  (setf (xp.prefix-stack-ptr xp) (- xp.prefix-stack-entry-size))
  xp)



;;; The char-mode stuff is a bit tricky.
;;; one can be in one of the following modes:
;;; NIL no changes to characters output.
;;; :UP CHAR-UPCASE used.
;;; :DOWN CHAR-DOWNCASE used.
;;; :CAP0 capitalize next alphanumeric letter then switch to :DOWN.
;;; :CAP1 capitalize next alphanumeric letter then switch to :CAPW
;;; :CAPW downcase letters.  When a word break letter found, switch to :CAP1.
;;; It is possible for ~(~) to be nested in a format string, but note that
;;; each mode specifies what should happen to every letter.  Therefore, inner
;;; nested modes never have any effect.  You can just ignore them.

(define (xp.push-char-mode xp new-mode)
  (if (zero? (xp.char-mode-counter xp))
      (setf (xp.char-mode xp) new-mode))
  (incf (xp.char-mode-counter xp)))

(define (xp.pop-char-mode xp)
  (decf (xp.char-mode-counter xp))
  (if (zero? (xp.char-mode-counter xp))
      (setf (xp.char-mode xp) '#f)))


;;; Assumes is only called when char-mode is non-nil

(define (xp.handle-char-mode xp char)
  (case (xp.char-mode xp)
    ((CAP0)
     (cond ((not (or (char-alphabetic? char) (char-numeric? char))) char)
	   (else (setf (xp.char-mode xp) 'DOWN) (char-upcase char))))
    ((CAP1)
     (cond ((not (or (char-alphabetic? char) (char-numeric? char))) char)
	   (else (setf (xp.char-mode xp) 'CAPW) (char-upcase char))))
    ((CAPW)
     (cond ((or (char-alphabetic? char) (char-numeric? char))
	    (char-downcase char))
	   (else (setf (xp.char-mode xp) 'CAP1) char)))
    ((UP)
     (char-upcase char))
    (else
     (char-downcase char)))) ;DOWN


;;; All characters output are passed through the handler above.  However, 
;;; it must be noted that on-each-line prefixes are only processed in the 
;;; context of the first place they appear.  They stay the same later no 
;;; matter what.  Also non-literal newlines do not count as word breaks.

;;; This handles the basic outputting of characters.  note + suffix means that
;;; the stream is known to be an XP stream, all inputs are mandatory, and no
;;; error checking has to be done.  Suffix ++ additionally means that the
;;; output is guaranteed not to contain a newline char.

(define (xp.write-char+ char xp)
  (if (eqv? char #\newline)
      (xp.pprint-newline+ 'unconditional xp)
      (xp.write-char++ char xp)))

(define (xp.write-string+ mystring xp start end)
  (let ((next-newline (string-position #\newline mystring start end)))
    (if next-newline
	(begin
	  (xp.write-string++ mystring xp start next-newline)
	  (xp.pprint-newline+ 'unconditional xp)
	  (xp.write-string+ mystring xp (1+ next-newline) end))
	(xp.write-string++ mystring xp start end))))


;;; note this checks (> BUFFER-PTR LINEL) instead of (> (xp.lp<-bp) LINEL)
;;; this is important so that when things are longer than a line they
;;; end up getting printed in chunks of size LINEL.

(define (xp.write-char++ char xp)
  (when (> (xp.buffer-ptr xp) (xp.linel xp))
    (xp.force-some-output xp))
  (let ((new-buffer-end (1+ (xp.buffer-ptr xp))))
    (xp.check-buffer-size xp new-buffer-end)
    (if (xp.char-mode xp) (setf char (xp.handle-char-mode xp char)))
    (setf (string-ref (xp.buffer xp) (xp.buffer-ptr xp)) char)    
    (setf (xp.buffer-ptr xp) new-buffer-end)))

(define (xp.force-some-output xp)
  (xp.attempt-to-output xp '#f '#f)
  (when (> (xp.buffer-ptr xp) (xp.linel xp)) ;only if printing off end of line
    (xp.attempt-to-output xp '#t '#t)))

(define (xp.write-string++ mystring xp start end)
  (when (> (xp.buffer-ptr xp) (xp.linel xp))
    (xp.force-some-output xp))
  (xp.write-string+++ mystring xp start end))


;;; never forces output; therefore safe to call from within xp.output-line.

(define (xp.write-string+++ mystring xp start end) 
  (let ((new-buffer-end (+ (xp.buffer-ptr xp) (- end start))))
    (xp.check-buffer-size xp new-buffer-end)
    (do ((buffer (xp.buffer xp))
	 (i (xp.buffer-ptr xp) (1+ i))
	 (j start (1+ j)))
	((= j end))
      (let ((char (string-ref mystring j)))
	(if (xp.char-mode xp) (setf char (xp.handle-char-mode xp char)))
	(setf (string-ref buffer i) char)))
    (setf (xp.buffer-ptr xp) new-buffer-end)))


(define (xp.pprint-tab+ kind colnum colinc xp)
  (let ((indented? '#f)
	(relative? '#f))
    (case kind
      ((section) (setf indented? '#t))
      ((line-relative) (setf relative? '#t))
      ((section-relative) (setf indented? '#t) (setf relative? '#t)))
    (let* ((current
	     (if (not indented?)
		 (xp.lp<-bp xp)
		 (- (xp.tp<-bp xp) (xp.section-start xp))))
	   (new
	     (if (zero? colinc)
		 (if relative? (+ current colnum) (max colnum current))
		 (cond (relative?
			(* colinc
			   (quotient (+ current colnum colinc -1) colinc)))
		       ((> colnum current) colnum)
		       (else
			(+ colnum
			   (* colinc
			      (quotient (+ current (- colnum) colinc)
					colinc)))))))
	   (end (- new current)))
      (when (positive? end)
	(if (xp.char-mode xp) (xp.handle-char-mode xp #\space))
	(let ((end (+ (xp.buffer-ptr xp) end)))
	  (xp.check-buffer-size xp end)
	  (string-fill (xp.buffer xp) #\space (xp.buffer-ptr xp) end)
	  (setf (xp.buffer-ptr xp) end))))))


;;; note following is smallest number >= x that is a multiple of colinc
;;;  (* colinc (quotient (+ x (1- colinc)) colinc))


(define (xp.pprint-newline+ kind xp)
  (xp.enqueue xp 'newline kind)
  (do ((ptr (xp.qleft xp) (xp.qnext ptr)))    ;find sections we are ending
      ((not (< ptr (xp.qright xp))))	;all but last
    (when (and (not (xp.qend xp ptr))
	       (not (> (xp.depth-in-blocks xp) (xp.qdepth xp ptr)))
	       (memq (xp.qtype xp ptr) '(newline start-block)))
      (setf (xp.qend xp ptr) (- (xp.qright xp) ptr))))
  (setf (xp.section-start xp) (xp.tp<-bp xp))
  (when (and (memq kind '(fresh unconditional)) (xp.char-mode xp))
    (xp.handle-char-mode xp #\newline))
  (when (memq kind '(fresh unconditional mandatory))
    (xp.attempt-to-output xp '#t '#f)))


(define (xp.start-block xp prefix-string on-each-line? suffix-string)
  (xp.write-prefix-suffix prefix-string xp)
  (if (and (xp.char-mode xp) on-each-line?)
      (setf prefix-string
	    (substring (xp.buffer xp)
		       (- (xp.buffer-ptr xp) (string-length prefix-string))
		       (xp.buffer-ptr xp))))
  (xp.push-block-stack xp)
  (xp.enqueue xp 'start-block '#f
	   (if on-each-line? (cons suffix-string prefix-string) suffix-string))
  (incf (xp.depth-in-blocks xp))	      ;must be after enqueue
  (setf (xp.section-start xp) (xp.tp<-bp xp)))


(define (xp.end-block xp suffix)
  (unless (and (dynamic *xp.abbreviation-happened*)
	       (eqv? (dynamic *xp.abbreviation-happened*)
		     (dynamic *print-lines*)))
    (xp.write-prefix-suffix suffix xp)
    (decf (xp.depth-in-blocks xp))
    (xp.enqueue xp 'end-block '#f suffix)
    (block foundit
      (do ((ptr (xp.qleft xp) (xp.qnext ptr))) ;look for start of block we are ending
	  ((not (< ptr (xp.qright xp))))    ;all but last
	  (when (and (= (xp.depth-in-blocks xp) (xp.qdepth xp ptr))
		     (eq? (xp.qtype xp ptr) 'start-block)
		     (not (xp.qoffset xp ptr)))
	    (setf (xp.qoffset xp ptr) (- (xp.qright xp) ptr))
	    (return-from foundit '#f)))	;can only be 1
      )
    (xp.pop-block-stack xp)))

(define (xp.write-prefix-suffix mystring xp)
  (when mystring
    (xp.write-string++ mystring xp 0 (string-length mystring))))

(define (xp.pprint-indent+ kind n xp)
  (xp.enqueue xp 'ind kind n))


;;; attempt-to-output scans the queue looking for things it can do.
;;; it keeps outputting things until the queue is empty, or it finds
;;; a place where it cannot make a decision yet.
;;; If flush-out? is T and force-newlines? is NIL then the buffer,
;;; prefix-stack, and queue will be in an inconsistent state after the call.
;;; You better not call it this way except as the last act of outputting.


(define-local-syntax (xp.maybe-too-large xp Qentry)
  `(let ((limit (xp.linel ,xp)))
     (when (eqv? (xp.line-limit ,xp) (xp.line-no ,xp)) ;prevents suffix overflow
       (decf limit 2) ;3 for " .." minus 1 for space (heuristic)
       (when (not (negative? (xp.prefix-stack-ptr ,xp)))
	 (decf limit (xp.suffix-ptr ,xp))))
     (cond ((xp.qend ,xp ,Qentry)
	    (> (xp.lp<-tp ,xp (xp.qpos ,xp (+ ,Qentry (xp.qend ,xp ,Qentry)))) limit))
	   ((or force-newlines? (> (xp.lp<-bp ,xp) limit))
	    '#t)
	   (else ;wait until later to decide.
	    (return-from attempt-to-output '#f)))))

(define-local-syntax (xp.misering? xp)
  `(and (dynamic *print-miser-width*)
	(<= (- (xp.linel ,xp) (initial-prefix-ptr ,xp))
	    (dynamic *print-miser-width*))))

(define (xp.attempt-to-output xp force-newlines? flush-out?)
  (block attempt-to-output
    (do ()
	((> (xp.qleft xp) (xp.qright xp))
	 (setf (xp.qleft xp) 0)
	 (setf (xp.qright xp) (- xp.queue-entry-size))) ;saves shifting
      (case (xp.qtype xp (xp.qleft xp))
	    ((ind)
	     (unless (xp.misering? xp)
	       (xp.set-indentation-prefix
		   xp
		   (case (xp.qkind xp (xp.qleft xp))
			 ((block)
			  (+ (initial-prefix-ptr xp) (xp.qarg xp (xp.qleft xp))))
			 (else ; current
			  (+ (xp.lp<-tp xp (xp.qpos xp (xp.qleft xp)))
			     (xp.qarg xp (xp.qleft xp)))))))
	     (setf (xp.qleft xp) (xp.qnext (xp.qleft xp))))
	    ((start-block)
	     (cond ((xp.maybe-too-large xp (xp.qleft xp))
		    (xp.push-prefix-stack xp)
		    (setf (initial-prefix-ptr xp) (xp.prefix-ptr xp))
		    (xp.set-indentation-prefix
		        xp (xp.lp<-tp xp (xp.qpos xp (xp.qleft xp))))
		    (let ((arg (xp.qarg xp (xp.qleft xp))))
		      (when (pair? arg) (xp.set-prefix xp (cdr arg)))
		      (setf (initial-prefix-ptr xp) (xp.prefix-ptr xp))
		      (cond ((not (list? arg)) (xp.set-suffix xp arg))
			    ((car arg) (xp.set-suffix xp (car arg)))))
		    (setf (xp.section-start-line xp) (xp.line-no xp)))
		   (else (incf (xp.qleft xp) (xp.qoffset xp (xp.qleft xp)))))
	     (setf (xp.qleft xp) (xp.qnext (xp.qleft xp))))
	    ((end-block)
	     (xp.pop-prefix-stack xp)
	     (setf (xp.qleft xp) (xp.qnext (xp.qleft xp))))
	    (else ; newline
	     (when (case (xp.qkind xp (xp.qleft xp))
			 ((fresh) (not (zero? (xp.lp<-bp xp))))
			 ((miser) (xp.misering? xp))
			 ((fill) (or (xp.misering? xp)
				      (> (xp.line-no xp) (xp.section-start-line xp))
				      (xp.maybe-too-large xp (xp.qleft xp))))
			 (else '#t)) ;(linear unconditional mandatory) 
	       (xp.output-line xp (xp.qleft xp))
	       (xp.setup-for-next-line xp (xp.qleft xp)))
	     (setf (xp.qleft xp) (xp.qnext (xp.qleft xp)))))))
  (when flush-out? (xp.flush xp)))


;;; this can only be called last!

(define (xp.flush xp)
  (unless (dynamic *xp.locating-circularities*)
    (internal-write-string (xp.buffer xp) (xp.base-stream xp) 0 (xp.buffer-ptr xp)))
  (incf (xp.buffer-offset xp) (xp.buffer-ptr xp))
  (incf (xp.charpos xp) (xp.buffer-ptr xp))
  (setf (xp.buffer-ptr xp) 0))


;;; This prints out a line of stuff.

(define (xp.output-line xp Qentry)
  (let* ((out-point (xp.bp<-tp xp (xp.qpos xp Qentry)))
	 (last-non-blank (string-position-not-from-end
			     #\space (xp.buffer xp) 0 out-point))
	 (end (cond ((memq (xp.qkind xp Qentry) '(fresh unconditional))
		     out-point)
		    (last-non-blank (1+ last-non-blank))
		    (else 0)))
	 (line-limit-exit (and (xp.line-limit xp)
			       (not (> (xp.line-limit xp) (xp.line-no xp))))))
    (when line-limit-exit
      (setf (xp.buffer-ptr xp) end)          ;truncate pending output.
      (xp.write-string+++ " .." xp 0 3)
      (string-nreverse (xp.suffix xp) 0 (xp.suffix-ptr xp))
      (xp.write-string+++ (xp.suffix xp) xp 0 (xp.suffix-ptr xp))
      (setf (xp.qleft xp) (xp.qnext (xp.qright xp)))
      (setf (dynamic *xp.abbreviation-happened*) (dynamic *print-lines*))
      (funcall (dynamic *xp.line-limit-abbreviation-exit*) '#t))
    (incf (xp.line-no xp))
    (unless (dynamic *xp.locating-circularities*)
      (internal-write-string (xp.buffer xp) (xp.base-stream xp) 0 end)
      (newline (xp.base-stream xp)))))

(define (xp.setup-for-next-line xp Qentry)
  (let* ((out-point (xp.bp<-tp xp (xp.qpos xp Qentry)))
	 (prefix-end
	   (cond ((memq (xp.qkind xp Qentry) '(unconditional fresh))
		  (non-blank-prefix-ptr xp))
		 (else (xp.prefix-ptr xp))))
	 (change (- prefix-end out-point)))
    (setf (xp.charpos xp) 0)
    (when (positive? change)                  ;almost never happens
      (xp.check-buffer-size xp (+ (xp.buffer-ptr xp) change)))
    (string-replace (xp.buffer xp) (xp.buffer xp)
		    prefix-end out-point (xp.buffer-ptr xp))
    (string-replace (xp.buffer xp) (xp.prefix xp) 0 0 prefix-end)
    (incf (xp.buffer-ptr xp) change)
    (decf (xp.buffer-offset xp) change)
    (when (not (memq (xp.qkind xp Qentry) '(unconditional fresh)))
      (setf (xp.section-start-line xp) (xp.line-no xp)))))

(define (xp.set-indentation-prefix xp new-position)
  (let ((new-ind (max (non-blank-prefix-ptr xp) new-position)))
    (setf (xp.prefix-ptr xp) (initial-prefix-ptr xp))
    (xp.check-prefix-size xp new-ind)
    (when (> new-ind (xp.prefix-ptr xp))
      (string-fill (xp.prefix xp) #\space (xp.prefix-ptr xp) new-ind))
    (setf (xp.prefix-ptr xp) new-ind)))

(define (xp.set-prefix xp prefix-string)
  (let ((end  (string-length prefix-string)))
    (string-replace (xp.prefix xp) prefix-string
		    (- (xp.prefix-ptr xp) end) 0 end))
  (setf (non-blank-prefix-ptr xp) (xp.prefix-ptr xp)))

(define (xp.set-suffix xp suffix-string)
  (let* ((end (string-length suffix-string))
	 (new-end (+ (xp.suffix-ptr xp) end)))
    (xp.check-suffix-size xp new-end)
    (do ((i (1- new-end) (1- i))
	 (j 0 (1+ j)))
	((= j end))
      (setf (string-ref (xp.suffix xp) i) (string-ref suffix-string j)))
    (setf (xp.suffix-ptr xp) new-end)))


;;;=====================================================================
;;; Basic interface functions
;;;=====================================================================

;;; The internal functions in this file
;;; use the '+' forms of these functions directly (which is faster) because,
;;; they do not need error checking of fancy stream coercion.  The '++' forms
;;; additionally assume the thing being output does not contain a newline.

(define (write object . maybe-stream)
  (let ((stream  (if (not (null? maybe-stream))
		     (car maybe-stream)
		     (current-output-port))))
    (cond ((xp.xp-structure-p stream)
	   (xp.write+ object stream))
	  ((xp.get-printer object)
	   (xp.initiate-xp-printing
	     (lambda (s o) (xp.write+ o s))
	     stream
	     object))
	  (else
	   (internal-write object stream)))))

(define (xp.maybe-initiate-xp-printing fn stream . args)
  (if (xp.xp-structure-p stream)
      (apply fn stream args)
      (apply (function xp.initiate-xp-printing) fn stream args)))

(define (xp.initiate-xp-printing fn stream . args)
  (dynamic-let ((*xp.abbreviation-happened*
		    '#f)
		(*xp.locating-circularities*
		    (if (dynamic *print-circle*)
			0
			'#f))
		(*xp.circularity-hash-table*
		    (if (dynamic *print-circle*)
			(make-table)
			'#f))
		(*xp.parents*
		    (if (not (dynamic *print-shared*))
			(list '#f)
			'()))			;*** is this right?
		(*xp.current-level*
		    0)
		(*xp.current-length*
		    0))
    (let ((result  (xp.xp-print fn stream args)))
      (when (dynamic *xp.abbreviation-happened*)
	(setf args (list-copy args))
	(setf (dynamic *last-abbreviated-printing*)
	      (lambda maybe-stream
		(let ((stream  (if (not (null? maybe-stream))
				   (car maybe-stream)
				   stream)))
		  (apply (function xp.maybe-initiate-xp-printing)
			 fn stream args)))))
      result)))

(define (xp.xp-print fn stream args)
  (let ((result  (xp.do-xp-printing fn stream args)))
    (when (dynamic *xp.locating-circularities*)
      (setf (dynamic *xp.locating-circularities*) '#f)
      (setf (dynamic *xp.abbreviation-happened*) '#f)
      (setf (dynamic *xp.parents*) '())
      (setf result (xp.do-xp-printing fn stream args)))
    result))

(define (xp.do-xp-printing fn stream args)
  (let ((xp (xp.get-pretty-print-stream stream))
	(result '#f))
    (dynamic-let ((*xp.current-level* 0))
      (let/cc catch
        (dynamic-let ((*xp.line-limit-abbreviation-exit* catch))
	  (xp.start-block xp '#f '#f '#f)
	  (setf result (apply fn xp args))
	  (xp.end-block xp '#f)))
      (when (and (dynamic *xp.locating-circularities*)
		 (zero? (dynamic *xp.locating-circularities*)) ;No circularities.
		 (= (xp.line-no xp) 1)	     	;Didn't suppress line.
		 (zero? (xp.buffer-offset xp)))	;Didn't suppress partial line.
	(setf (dynamic *xp.locating-circularities*) '#f))	;print what you have got.
      (when (let/cc catch
	      (dynamic-let ((*xp.line-limit-abbreviation-exit* catch))
	        (xp.attempt-to-output xp '#f '#t)
		'#f))
	(xp.attempt-to-output xp '#t '#t))
      (xp.free-pretty-print-stream xp)
      result)))


(define (xp.write+ object xp)
  (dynamic-let ((*xp.parents* (dynamic *xp.parents*)))
    (unless (and (dynamic *xp.circularity-hash-table*)
		 (eq? (xp.circularity-process xp object '#f) 'subsequent))
      (when (and (dynamic *xp.circularity-hash-table*) (pair? object))
	;; Must do this to avoid additional circularity detection by
        ;; pprint-logical-block; otherwise you get stuff like #1=#1#.
	(setf object (cons (car object) (cdr object))))
      (funcall (or (xp.get-printer object) (function xp.print-default))
	       object
	       xp))
    object))



(define (xp.print-default object xp)
  (let ((stuff (internal-write-to-string object)))
    (xp.write-string+ stuff xp 0 (string-length stuff))))


;;; It is vital that this function be called EXACTLY once for each occurrence 
;;;   of each thing in something being printed.
;;; Returns nil if printing should just continue on.
;;;   Either it is not a duplicate, or we are in the first pass and do not 
;;;   know.
;;; returns :FIRST if object is first occurrence of a DUPLICATE.
;;;   (This can only be returned on a second pass.)
;;;   After an initial code (printed by this routine on the second pass)
;;;   printing should continue on for the object.
;;; returns :SUBSEQUENT if second or later occurrence.
;;;   Printing is all taken care of by this routine.

;;; Note many (maybe most) lisp implementations have characters and small 
;;; numbers represented in a single word so that the are always eq when 
;;; they are equal and the reader takes care of properly sharing them 
;;; (just as it does with symbols).  Therefore, we do not want circularity 
;;; processing applied to them.  However, some kinds of numbers 
;;; (e.g., bignums) undoubtedly are complex structures that the reader 
;;; does not share.  However, they cannot have circular pointers in them
;;; and it is therefore probably a waste to do circularity checking on them.  
;;; In any case, it is not clear that it easy to tell exactly what kinds of 
;;; numbers a given implementation is going to have the reader 
;;; automatically share.

(define (xp.circularity-process xp object interior-cdr?)
  (unless (or (number? object)
	      (char? object)
	      (and (symbol? object) (not (gensym? object))))
    (let ((id (table-entry (dynamic *xp.circularity-hash-table*) object)))
      (if (dynamic *xp.locating-circularities*)
	  ;; This is the first pass.
	  (cond ((not id)	;never seen before
		 (when (not (null? (dynamic *xp.parents*)))
		   (push object (dynamic *xp.parents*)))
		 (setf (table-entry (dynamic *xp.circularity-hash-table*) object)
		       0)
		 '#f)
		((zero? id) ;possible second occurrence
		 (cond ((or (null? (dynamic *xp.parents*))
			    (memq object (dynamic *xp.parents*)))
			(setf (table-entry
			          (dynamic *xp.circularity-hash-table*) object)
			      (incf (dynamic *xp.locating-circularities*)))
			'subsequent)
		       (else '#f)))
		(else 'subsequent));third or later occurrence
	  ;; This is the second pass.
	  (cond ((or (not id)	;never seen before (note ~@* etc. conses)
		     (zero? id));no duplicates
		 '#f)
		((positive? id) ; first occurrence
		 (cond (interior-cdr?
			(decf (dynamic *xp.current-level*))
			(xp.write-string++ ". #" xp 0 3))
		       (else (xp.write-char++ #\# xp)))
		 (xp.print-integer id xp)
		 (xp.write-char++ #\= xp)
		 (setf (table-entry (dynamic *xp.circularity-hash-table*) object)
		       (- id))
		 'first)
		(else
		 (if interior-cdr?
		     (xp.write-string++ ". #" xp 0 3)
		     (xp.write-char++ #\# xp))
		 (xp.print-integer(- id) xp)
		 (xp.write-char++ #\# xp)
		 'subsequent))))))


;;; Here are all the standard Common Lisp printing functions.

(define (print object . maybe-stream)
  (let ((stream  (if (not (null? maybe-stream))
		     (car maybe-stream)
		     (current-output-port))))
    (dynamic-let ((*print-escape* '#t))
      (terpri stream)
      (write object stream)
      (write-char #\space stream)
      object)))

(define (prin1 object . maybe-stream)
  (let ((stream  (if (not (null? maybe-stream))
		     (car maybe-stream)
		     (current-output-port))))
    (dynamic-let ((*print-escape* '#t))
      (write object stream)
      object)))

(define (princ object . maybe-stream)
  (let ((stream  (if (not (null? maybe-stream))
		     (car maybe-stream)
		     (current-output-port))))
    (dynamic-let ((*print-escape* '#f))
      (write object stream)
      object)))

(define (display object . maybe-stream)
  (apply (function princ) object maybe-stream))


(define (pprint object . maybe-stream)
  (let ((stream  (if (not (null? maybe-stream))
		     (car maybe-stream)
		     (current-output-port))))
    (dynamic-let ((*print-escape* '#t)
		  (*print-pretty* '#t))
      (terpri stream)
      (write object stream)
      (values))))

(define (prin1-to-string object)
  (call-with-output-string
      (lambda (stream)
	(dynamic-let ((*print-escape* '#t))
	  (write object stream)))))

(define (princ-to-string object)
  (call-with-output-string
      (lambda (stream)
	(dynamic-let ((*print-escape* '#f))
	  (write object stream)))))



(define (write-char char . maybe-stream)
  (let ((stream  (if (not (null? maybe-stream))
		     (car maybe-stream)
		     (current-output-port))))
    (if (xp.xp-structure-p stream)
	(xp.write-char+ char stream)
	(internal-write-char char stream))
    char))

(define (write-string mystring . maybe-stream-start-end)
  (let* ((stream  (if (not (null? maybe-stream-start-end))
		      (car maybe-stream-start-end)
		      (current-output-port)))
	 (start   (if (not (null? (cdr maybe-stream-start-end)))
		      (cadr maybe-stream-start-end)
		      0))
	 (end     (if (not (null? (cddr maybe-stream-start-end)))
		      (caddr maybe-stream-start-end)
		      (string-length mystring))))
    (if (xp.xp-structure-p stream)
	(xp.write-string+ mystring stream start end)
	(internal-write-string mystring stream start end))
    mystring))

(define (write-line mystring . maybe-stream-start-end)
  (let* ((stream  (if (not (null? maybe-stream-start-end))
		      (car maybe-stream-start-end)
		      (current-output-port)))
	 (start   (if (not (null? (cdr maybe-stream-start-end)))
		      (cadr maybe-stream-start-end)
		      0))
	 (end     (if (not (null? (cddr maybe-stream-start-end)))
		      (caddr maybe-stream-start-end)
		      (string-length mystring))))
    (if (xp.xp-structure-p stream)
	(begin
	  (xp.write-string+ mystring stream start end)
	  (xp.pprint-newline+ 'unconditional stream))
	(begin 
	  (internal-write-string mystring stream start end)
	  (internal-newline stream)))
    mystring))

(define (terpri . maybe-stream)
  (let ((stream  (if (not (null? maybe-stream))
		     (car maybe-stream)
		     (current-output-port))))
    (if (xp.xp-structure-p stream)
	(xp.pprint-newline+ 'unconditional stream)
	(internal-newline stream))
    '#f))

(define (newline . maybe-stream)
  (apply (function terpri) maybe-stream))


;;; This has to violate the XP data abstraction and fool with internal
;;; stuff, in order to find out the right info to return as the result.

(define (fresh-line . maybe-stream)
  (let ((stream  (if (not (null? maybe-stream))
		     (car maybe-stream)
		     (current-output-port))))
    (cond ((xp.xp-structure-p stream)
	   (xp.attempt-to-output stream '#t '#t) ;ok because we want newline
	   (when (not (zero? (xp.lp<-bp stream)))
	     (xp.pprint-newline+ 'fresh stream)
	     '#t))
	  (else
	   (internal-fresh-line stream)))))


;;; Each of these causes the stream to be pessimistic and insert
;;; newlines wherever it might have to, when forcing the partial output
;;; out.  This is so that things will be in a consistent state if
;;; output continues to the stream later.

(define (finish-output . maybe-stream)
  (let ((stream  (if (not (null? maybe-stream))
		     (car maybe-stream)
		     (current-output-port))))
    (if (xp.xp-structure-p stream)
	(xp.attempt-to-output stream '#t '#t)
	(internal-finish-output stream))
    '#f))

(define (force-output . maybe-stream)
  (let ((stream  (if (not (null? maybe-stream))
		     (car maybe-stream)
		     (current-output-port))))
    (if (xp.xp-structure-p stream)
	(xp.attempt-to-output stream '#t '#t)
	(internal-force-output stream))
    '#f))

(define (clear-output . maybe-stream)
  (let ((stream  (if (not (null? maybe-stream))
		     (car maybe-stream)
		     (current-output-port))))
    (if (xp.xp-structure-p stream)
	(dynamic-let ((*xp.locating-circularities* 0)) ;hack to prevent visible output
	  (xp.attempt-to-output stream '#t '#t)
	(internal-clear-output stream)))
    '#f))
   



;;;=====================================================================
;;; Functional interface to dynamic formatting
;;;=====================================================================

;;; The internal functions in this file, and the (formatter "...") expansions
;;; use the '+' forms of these functions directly (which is faster) because,
;;; they do not need error checking or fancy stream coercion.  The '++' forms
;;; additionally assume the thing being output does not contain a newline.

(define-syntax (pprint-logical-block stream-symbol-stuff . body)
  (let* ((stream-symbol    (car stream-symbol-stuff))
	 (mylist             (cadr stream-symbol-stuff))
	 (rest             (cddr stream-symbol-stuff))
	 (prefix           (if (not (null? rest)) (pop rest) ""))
	 (suffix           (if (not (null? rest)) (pop rest) ""))
	 (per-line?        (if (not (null? rest)) (pop rest) '#f)))
    `(xp.maybe-initiate-xp-printing
       (lambda (,stream-symbol)
	 (let ((+l ,mylist)
	       (+p ,prefix)
	       (+s ,suffix)
	       (+x ,stream-symbol))
	   (xp.pprint-logical-block+ (+x +l +p +s ,per-line? '#t '#f)
	     ,@body
	     '#f)))
       ,stream-symbol)))


;;; Assumes var and args must be variables.  Other arguments must be literals 
;;; or variables.

(define-syntax (xp.pprint-logical-block+ stuff . body)
  (let* ((var            (pop stuff))
	 (args           (pop stuff))
	 (prefix         (pop stuff))
	 (suffix         (pop stuff))
	 (per-line?      (pop stuff)))
    `(unless (xp.check-abbreviation ,var ,args)
       (dynamic-let ((*xp.current-level* (1+ (dynamic *xp.current-level*)))
		     (*xp.current-length* -1)
		     (*xp.parents* (dynamic *xp.parents*)))
	 (block logical-block
	   (if (dynamic *print-pretty*)
	       (xp.start-block ,var ,prefix ,per-line? ,suffix)
	       (xp.write-prefix-suffix ,prefix ,var))
	   (unwind-protect
	       (begin ,@body)
	     (if (dynamic *print-pretty*)
		 (xp.end-block ,var ,suffix)
		 (xp.write-prefix-suffix ,suffix ,var))))))
    ))

(define (xp.check-abbreviation xp object)
  (cond ((and (dynamic *print-level*)
	      (>= (dynamic *xp.current-level*)
		  (dynamic *print-level*)))
	 (xp.write-char++ #\# XP)
	 (setf (dynamic *xp.abbreviation-happened*) '#t)
	 '#t)
	((and (dynamic *xp.circularity-hash-table*)
	      (eq? (xp.circularity-process xp object '#f) 'subsequent))
	 '#t)
	(else '#f)))


(define-syntax (pprint-pop)
  `(xp.pprint-pop+ +l +x))

(define-syntax (xp.pprint-pop+ args xp)
  `(if (xp.pprint-pop-check+ ,args ,xp)
       (return-from logical-block '#f)
       (if (null? ,args) '() (pop ,args))))

(define (xp.pprint-pop-check+ args xp)
  (incf (dynamic *xp.current-length*))
  (cond ((not (or (pair? args) (null? args)))
	 ;; must be first to supersede length abbreviation
	 (xp.write-string++ ". " xp 0 2)
	 (xp.write+ args xp)
	 '#t)
	((and (dynamic *print-length*)
	      (not (< *xp.current-length* (dynamic *print-length*))))
	 ;; must supersede circularity check
	 (xp.write-string++ "..." xp 0 3)
	 (setf (dynamic *xp.abbreviation-happened*) '#t)
	 '#t)
	((and (dynamic *xp.circularity-hash-table*)
	      (not (zero? *xp.current-length*)))
	 (case (xp.circularity-process xp args '#t)
	       ((first)
		(xp.write+ (cons (car args) (cdr args)) xp) '#t)
	       ((subsequent)
		'#t)
	       (else
		'#f)))
	(else
	 '#f)))

(define-syntax (pprint-exit-if-list-exhausted)
  `(xp.pprint-exit-if-list-exhausted+ +l))

(define-syntax (xp.pprint-exit-if-list-exhausted+ mylist)
  `(if (null? ,mylist) (return-from logical-block '#f)))


(define (pprint-newline kind . maybe-stream)
  (let ((stream  (if (not (null? maybe-stream))
		     (car maybe-stream)
		     (current-output-port))))
    (when (not (memq kind '(linear miser fill mandatory)))
      (error "Invalid KIND argument ~A to PPRINT-NEWLINE" kind))
    (when (and (xp.xp-structure-p stream) (dynamic *print-pretty*))
      (xp.pprint-newline+ kind stream))
    '#f))

(define (pprint-indent relative-to n . maybe-stream)
  (let ((stream  (if (not (null? maybe-stream))
		     (car maybe-stream)
		     (current-output-port))))
    (when (not (memq relative-to '(block current)))
      (error "Invalid KIND argument ~A to PPRINT-INDENT" relative-to))
    (when (and (xp.xp-structure-p stream) (dynamic *print-pretty*))
      (xp.pprint-indent+ relative-to n stream))
    '#f))

(define (pprint-tab kind colnum colinc . maybe-stream)
  (let ((stream  (if (not (null? maybe-stream))
		     (car maybe-stream)
		     (current-output-port))))
    (when (not (memq kind '(line section line-relative section-relative)))
      (error "Invalid KIND argument ~A to PPRINT-TAB" kind))
    (when (and (xp.xp-structure-p stream) (dynamic *print-pretty*))
      (xp.pprint-tab+ kind colnum colinc stream))
    '#f))




;;;=====================================================================
;;; Standard print dispatch function
;;;=====================================================================


(define (xp.print-null object xp)
  (declare (ignore object))
  (xp.write-string+ "()" xp 0 2))

(define (xp.print-true object xp)
  (declare (ignore object))
  (xp.write-string+ "#t" xp 0 2))

(define (xp.print-false object xp)
  (declare (ignore object))
  (xp.write-string+ "#f" xp 0 2))

(define (xp.print-symbol object xp)
  (if (dynamic *print-escape*)
      (xp.print-default object xp)
      (let ((mystring  (symbol->string object)))
	(xp.write-string+ mystring xp 0 (string-length mystring)))))

(define (xp.print-number object xp)
  (if (and (integer? object)
	   (eqv? (dynamic *print-base*) 10)
	   (not (dynamic *print-radix*)))
      (begin
        (when (negative? object)
	  (xp.write-char++ #\- xp)
	  (setf object (- object)))
	(xp.print-integer object xp))
      (xp.print-default object xp)))

(define (xp.print-integer n xp)
  (let ((quot  (quotient n 10))
	(rem   (remainder n 10)))
    (unless (zero? quot)
      (xp.print-integer quot xp))
    (xp.write-char++ (string-ref "0123456789" rem) xp)))

(define (xp.print-string object xp)
  (if (dynamic *print-escape*)
      (begin
        (xp.write-char++ #\" xp)
	(do ((i 0 (1+ i))
	     (n (string-length object)))
	    ((= i n))
	    (let ((c  (string-ref object i)))
	      (if (or (char=? c #\") (char=? c #\\))
		  (xp.write-char++ #\\ xp))
	      (xp.write-char++ c xp)))
	(xp.write-char++ #\" xp))
      (xp.write-string+ object xp 0 (string-length object))))

(define (xp.print-character object xp)
  (if (dynamic *print-escape*)
      (let ((name  (char-name object)))
        (xp.write-char++ #\# xp)
	(xp.write-char++ #\\ xp)
	(if name
	    (xp.write-string++ name xp 0 (string-length name))
	    (xp.write-char++ object xp)))
      (xp.write-char+ object xp)))

(define (xp.print-vector object xp)
  (let* ((pretty?  (dynamic *print-pretty*))
	 (end      (vector-length object)))
    (pprint-logical-block (xp '() "#(" ")")
      (do ((i 0 (1+ i)))
	  ((eqv? i end) '#f)
	  (when (not (eqv? i 0))
	    (xp.write-char++ #\space xp)
	    (if pretty?
		(xp.pprint-newline+ 'fill xp)))
	  (pprint-pop)
	  (xp.write+ (vector-ref object i) xp)
	  ))))

(define (xp.print-table object xp)
  (let ((pretty?  (dynamic *print-pretty*)))
    (pprint-logical-block (xp '() "#<Table" ">")
      (table-for-each
        (lambda (key value)
	  (xp.write-char++ #\space xp)
	  (if pretty?
	      (xp.pprint-newline+ 'fill xp))
	  (pprint-pop)
	  (xp.write+ (cons key value) xp))
	object))))

(define (xp.print-pair object xp)
  (if (dynamic *print-pretty*)
      (xp.pretty-print-list object xp)
      (xp.pprint-logical-block+ (xp object "(" ")" '#f '#t '#f)
	(do ()
	    ((null? object) '#f)
	    (xp.write+ (xp.pprint-pop+ object xp) xp)
	    (when (not (null? object)) (xp.write-char++ #\space xp))))))

(define (xp.print-struct object xp)
  (if (dynamic *print-structure*)
      (print-structure-default object xp)
      (funcall (get-structure-printer (struct-type-descriptor object))
	       object xp)))

(define (get-structure-printer td)
  (or (td-printer td)
      (if (eq? (td-name td) 'struct)
          (function print-structure-default)
          (get-structure-printer (td-parent-type td)))))



(define (print-structure-default object xp)
  (let* ((td       (struct-type-descriptor object))
	 (slots    (td-slots td))
	 (pretty?  (dynamic *print-pretty*)))
    (pprint-logical-block (xp '() "#<Struct " ">")
      (prin1 (td-name td) xp)
      (when (dynamic *print-structure-slots*)
	(dolist (s slots)
	  (write-char #\space xp)
	  (if pretty? (pprint-newline 'fill xp))
	  (pprint-pop)
	  (prin1 (sd-name s) xp)
	  (write-char #\space xp)
	  (write (funcall (sd-getter-function s) object) xp)))
      )))


;;; This table can't be initialized until after all the functions
;;; have been defined.

(define *standard-print-dispatch-table*
  (list (cons (function null?)         (function xp.print-null))
	(cons (lambda (x) (eq? x '#t)) (function xp.print-true))
	(cons (function not)           (function xp.print-false))
	(cons (function symbol?)       (function xp.print-symbol))
	(cons (function number?)       (function xp.print-number))
	(cons (function pair?)         (function xp.print-pair))
	(cons (function string?)       (function xp.print-string))
	(cons (function char?)         (function xp.print-character))
	(cons (function struct?)       (function xp.print-struct))
	(cons (function vector?)       (function xp.print-vector))
	(cons (function table?)        (function xp.print-table))))

(define (standard-print-dispatch object)
  (standard-print-dispatch-aux
     object (dynamic *standard-print-dispatch-table*)))

(define (standard-print-dispatch-aux object table)
  (cond ((null? table) (function xp.print-default))
	((funcall (car (car table)) object)
	 (cdr (car table)))
	(else
	 (standard-print-dispatch-aux object (cdr table)))))

(setf (dynamic *print-dispatch*) (function standard-print-dispatch))



;;;=====================================================================
;;; Pretty printing formats for code
;;;=====================================================================


;;; The standard prettyprinters for lists dispatch off the CAR of the list.

(define *xp.pair-dispatch-table* (make-table))

(define (xp.pretty-print-list object xp)
  (funcall (or (table-entry (dynamic *xp.pair-dispatch-table*) (car object))
	       (if (symbol? (car object)) (function xp.fn-call) '#f)
	       (lambda (object xp)
		 (pprint-fill xp object)))
	   object
	   xp))


;;; Must use pprint-logical-block (no +) in the following three, because they 
;;; are exported functions.
;;; *** Note that the argument order on these is backwards; that's the
;;; *** way it is in Common Lisp....

(define (pprint-linear s object . moreargs)
  (let* ((colon?  (if (not (null? moreargs)) (pop moreargs) '#t))
	 (atsign? (if (not (null? moreargs)) (pop moreargs) '#f)))
    (declare (ignore atsign?))
    (pprint-logical-block (s object (if colon? "(" "") (if colon? ")" ""))
      (pprint-exit-if-list-exhausted)
      (do () ('#f)
	  (xp.write+ (pprint-pop) s)
	  (pprint-exit-if-list-exhausted)
	  (xp.write-char++ #\space s)
	  (xp.pprint-newline+ 'linear s)))))

(define (pprint-fill s object . moreargs)
  (let* ((colon?  (if (not (null? moreargs)) (pop moreargs) '#t))
	 (atsign? (if (not (null? moreargs)) (pop moreargs) '#f)))
    (declare (ignore atsign?))
    (pprint-logical-block (s object (if colon? "(" "") (if colon? ")" ""))
      (pprint-exit-if-list-exhausted)
      (do () ('#f)
	  (xp.write+ (pprint-pop) s)
	  (pprint-exit-if-list-exhausted)
	  (xp.write-char++ #\space s)
	  (xp.pprint-newline+ 'fill s)))))

(define (pprint-tabular s object . moreargs)
  (let* ((colon?  (if (not (null? moreargs)) (pop moreargs) '#t))
	 (atsign? (if (not (null? moreargs)) (pop moreargs) '#f))
	(tabsize (or (and (not (null? moreargs)) (pop moreargs)) 16)))
    (declare (ignore atsign?))
    (pprint-logical-block (s object (if colon? "(" "") (if colon? ")" ""))
      (pprint-exit-if-list-exhausted)
      (do () ('#f)
	  (xp.write+ (pprint-pop) s)
	  (pprint-exit-if-list-exhausted)
	  (xp.write-char++ #\space s)
	  (xp.pprint-tab+ 'section-relative 0 tabsize s)
	  (xp.pprint-newline+ 'fill s)))))


(define (xp.fn-call object xp)
  ;; (formatter "~:<~W~^ ~:I~@_~@{~W~^ ~_~}~:>")
  (xp.pprint-logical-block+ (xp object "(" ")" '#f '#t '#f)
    (xp.write+ (xp.pprint-pop+ object xp) xp)
    (xp.pprint-exit-if-list-exhausted+ object)
    (xp.write-char++ #\space xp)
    (xp.pprint-indent+ 'current 0 xp)
    (xp.pprint-newline+ 'miser xp)
    (xp.write+ (xp.pprint-pop+ object xp) xp)
    (do ()
	((null? object) '#f)
	(xp.write-char++ #\space xp)
	(xp.pprint-newline+ 'linear xp)
	(xp.write+ (xp.pprint-pop+ object xp) xp))))


;;; Although idiosyncratic, I have found this very useful to avoid large
;;; indentations when printing out code.

(define (xp.alternative-fn-call object xp)
  (if (> (string-length (symbol->string (car object))) 12)
      ;; (formatter "~:<~1I~@{~W~^ ~_~}~:>")
      (xp.pprint-logical-block+ (xp object "(" ")" '#f '#t '#f)
        (xp.pprint-indent+ 'block 1 xp)
	(when (not (null? object))
	  (xp.write+ (xp.pprint-pop+ object xp) xp)
	  (do ()
	      ((null? object) '#f)
	      (xp.write-char++ #\space xp)
	      (xp.pprint-newline+ 'linear xp)
	      (xp.write+ (xp.pprint-pop+ object xp) xp))))
      (xp.fn-call object xp)))


(define (xp.bind-list object xp . args)
  (declare (ignore args))
  ;; (formatter "~:<~@{~:/xp:pprint-fill/~^ ~_~}~:>")
  (xp.pprint-logical-block+ (xp object "(" ")" '#f '#t '#f)
    (when (not (null? object))
      (pprint-fill xp (xp.pprint-pop+ object xp) '#t '#f)
      (do ()
	  ((null? object) '#f)
	  (xp.write-char++ #\space xp)
	  (xp.pprint-newline+ 'linear xp)
	  (pprint-fill xp (xp.pprint-pop+ object xp) '#t '#f)))))

(define (xp.fbind-list object xp . args)
  (declare (ignore args))
  ;; (formatter "~:<~@{~:/xp:pprint-fill/~^ ~_~}~:>")
  (xp.pprint-logical-block+ (xp object "(" ")" '#f '#t '#f)
    (when (not (null? object))
      (pprint-fill xp (xp.pprint-pop+ object xp) '#t '#f)
      (do ()
	  ((null? object) '#f)
	  (xp.write-char++ #\space xp)
	  (xp.pprint-newline+ 'linear xp)
	  (xp.block-like (xp.pprint-pop+ object xp) xp)))))


(define (xp.block-like object xp . args)
  (declare (ignore args))
  ;; (formatter "~:<~1I~^~W~^ ~@_~W~^~@{ ~_~W~^~}~:>")
  (xp.pprint-logical-block+ (xp object "(" ")" '#f '#t '#f)
    (xp.pprint-indent+ 'block 1 xp)
    (xp.pprint-exit-if-list-exhausted+ object)
    (xp.write+ (xp.pprint-pop+ object xp) xp)
    (xp.pprint-exit-if-list-exhausted+ object)
    (xp.write-char++ #\space xp)
    (xp.pprint-newline+ 'miser xp)
    (xp.write+ (xp.pprint-pop+ object xp) xp)
    (xp.pprint-exit-if-list-exhausted+ object)
    (do ()
	((null? object) '#f)
	(xp.write-char++ #\space xp)
	(xp.pprint-newline+ 'linear xp)
	(xp.write+ (xp.pprint-pop+ object xp) xp))))


(define (xp.print-fancy-fn-call object xp template)
  (xp.pprint-logical-block+ (xp object "(" ")" '#f '#t '#f)
    (xp.write+ (xp.pprint-pop+ object xp) xp)
    (xp.pprint-indent+ 'current 1 xp)
    (do ((i 0 (1+ i))
	 (in-first-section '#t))
	((null? object) '#f)
	(xp.write-char++ #\space xp)
	(when (eqv? i (car template))
	  (xp.pprint-indent+ 'block (cadr template) xp)
	  (setf template (cddr template))
	  (setf in-first-section '#f))
	(pprint-newline (cond ((zero? i) 'miser)
			      (in-first-section 'fill)
			      (else 'linear))
			xp)
	(xp.write+ (xp.pprint-pop+ object xp) xp))))

(define (xp.let-print object xp)
  ;; (formatter "~:<~1I~W~^ ~@_~/xp:xp.bind-list/~^~@{ ~_~W~^~}~:>")
  (xp.pprint-logical-block+ (xp object "(" ")" '#f '#t '#f)
    (xp.pprint-indent+ 'block 1 xp)
    (xp.write+ (xp.pprint-pop+ object xp) xp)
    (xp.pprint-exit-if-list-exhausted+ object)
    (xp.write-char++ #\space xp)
    (xp.pprint-newline+ 'miser xp)
    (xp.bind-list (xp.pprint-pop+ object xp) xp '#f '#f)
    (xp.pprint-exit-if-list-exhausted+ object)
    (do ()
	((null? object) '#f)
	(xp.write-char++ #\space xp)
	(xp.pprint-newline+ 'linear xp)
	(xp.write+ (xp.pprint-pop+ object xp) xp))))

(define (xp.flet-print object xp)
  (xp.pprint-logical-block+ (xp object "(" ")" '#f '#t '#f)
    (xp.pprint-indent+ 'block 1 xp)
    (xp.write+ (xp.pprint-pop+ object xp) xp)
    (xp.pprint-exit-if-list-exhausted+ object)
    (xp.write-char++ #\space xp)
    (xp.pprint-newline+ 'miser xp)
    (xp.fbind-list (xp.pprint-pop+ object xp) xp '#f '#f)
    (xp.pprint-exit-if-list-exhausted+ object)
    (do ()
	((null? object) '#f)
	(xp.write-char++ #\space xp)
	(xp.pprint-newline+ 'linear xp)
	(xp.write+ (xp.pprint-pop+ object xp) xp))))

(define (xp.cond-print object xp)
  ;; (formatter "~:<~W~^ ~:I~@_~@{~:/xp:pprint-linear/~^ ~_~}~:>")
  (xp.pprint-logical-block+ (xp object "(" ")" '#f '#t '#f)
    (xp.write+ (xp.pprint-pop+ object xp) xp)
    (xp.pprint-exit-if-list-exhausted+ object)
    (xp.write-char++ #\space xp)
    (xp.pprint-indent+ 'current 0 xp)
    (xp.pprint-newline+ 'miser xp)
    (pprint-linear xp (xp.pprint-pop+ object xp) '#t '#f)
    (do ()
	((null? object) '#f)
	(xp.write-char++ #\space xp)
	(xp.pprint-newline+ 'linear xp)
	(pprint-linear xp (xp.pprint-pop+ object xp) '#t '#f))))

(define (xp.do-print object xp)
  ;; (formatter "~:<~W~^ ~:I~@_~/xp:xp.bind-list/~^ ~_~:/xp:pprint-linear/ ~1I~^~@{ ~_~W~^~}~:>")
  (xp.pprint-logical-block+ (xp object "(" ")" '#f '#t '#f)
    (xp.write+ (xp.pprint-pop+ object xp) xp)
    (xp.pprint-exit-if-list-exhausted+ object)
    (xp.write-char++ #\space xp)
    (xp.pprint-indent+ 'current 0 xp)
    (xp.pprint-newline+ 'miser xp)
    (xp.bind-list (xp.pprint-pop+ object xp) xp '#f '#f)
    (xp.pprint-exit-if-list-exhausted+ object)
    (xp.write-char++ #\space xp)
    (xp.pprint-newline+ 'linear xp)
    (pprint-linear xp (xp.pprint-pop+ object xp) '#t '#f)
    (xp.write-char++ #\space xp)
    (xp.pprint-indent+ 'block 1 xp)
    (do ()
	((null? object) '#f)
	(xp.write-char++ #\space xp)
	(xp.pprint-newline+ 'linear xp)
	(xp.write+ (xp.pprint-pop+ object xp) xp))))

(define (xp.mvb-print object xp)
  (xp.print-fancy-fn-call object xp '(1 3 2 1)))

(define (xp.setf-print object xp)
  ;; (formatter "~:<~W~^ ~:I~@_~@{~W~^ ~:_~W~^ ~_~}~:>")
  (xp.pprint-logical-block+ (xp object "(" ")" '#f '#t '#f)
    (xp.write+ (xp.pprint-pop+ object xp) xp)
    (xp.pprint-exit-if-list-exhausted+ object)
    (xp.write-char++ #\space xp)
    (xp.pprint-indent+ 'current 0 xp)
    (xp.pprint-newline+ 'miser xp)
    (xp.write+ (xp.pprint-pop+ object xp) xp)
    (do ()
	((null? object) '#f)
	(xp.write-char++ #\space xp)
	(xp.pprint-newline+ 'fill xp)
	(xp.write+ (xp.pprint-pop+ object xp) xp)
	(when (not (null? object))
	    (xp.write-char++ #\space xp)
	    (xp.pprint-newline+ 'linear xp)
	    (xp.write+ (xp.pprint-pop+ object xp) xp)))))

(define (xp.quote-print object xp)
  (if (and (pair? (cdr object)) (null? (cddr object)))
      (begin
         (xp.write-char++ #\' xp)
	 (xp.write+ (cadr object) xp))
      (pprint-fill xp object)))

(define (xp.up-print object xp)
  (xp.print-fancy-fn-call object xp '(0 3 1 1)))


;;; Install printers for built-in macros and special forms into the
;;; standard dispatch table.

(define-local-syntax (define-printer symbol function)
  `(setf (table-entry (dynamic *xp.pair-dispatch-table*) ',symbol)
	 (function ,function)))


;;; *** Missing support for backquote here.

(define-printer quote xp.quote-print)
(define-printer lambda xp.block-like)
(define-printer when xp.block-like)
(define-printer unless xp.block-like)
(define-printer cond xp.cond-print)
(define-printer case xp.block-like)
(define-printer setf xp.setf-print)
(define-printer set! xp.setf-print)
(define-printer let xp.let-print)
(define-printer let* xp.let-print)
(define-printer letrec xp.let-print)
(define-printer flet xp.flet-print)
(define-printer labels xp.flet-print)
(define-printer dynamic-let xp.let-print)
(define-printer block xp.block-like)
(define-printer do xp.do-print)
(define-printer dolist xp.block-like)
(define-printer dotimes xp.block-like)
(define-printer multiple-value-bind xp.mvb-print)
(define-printer let/cc xp.block-like)
(define-printer unwind-protect xp.up-print)
(define-printer define xp.block-like)
(define-printer define-syntax xp.block-like)
(define-printer define-local-syntax xp.block-like)
(define-printer pprint-logical-block xp.block-like)
(define-printer xp.pprint-logical-block+ xp.block-like)

;;; Here are some hacks for struct macros.

(define-printer update-slots xp.mvb-print)
(define-printer make xp.block-like)
