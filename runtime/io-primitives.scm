
;;; These are the IO primitives used by PreludeIOPrims

;;; Note: the box in write-string-stdout, write-string-file, and
;;;  append-string-file are due to the NoConversion in the .hi file.
;;; The problem is that NoConversion applies to everything, not just
;;; the input arg that the conversion is not needed or.


(predefine (notify-input-request))

(define *emacs-notified* '#f)
(define *stdin-read* '#f)

(define (initialize-io-system)
  (setf *emacs-notified* '#f)
  (setf *stdin-read* '#f))

(define (io-success . res)
  (make-tagged-data 0
    (if (null? res)
	(box 0)
	(box (make-haskell-string (car res))))))

(define (io-success/bin res)
  (make-tagged-data 0 (box res)))

(define (io-success/lazy res)
  (make-tagged-data 0 res))

(define (io-failure string)
  (make-tagged-data 1 (box (make-haskell-string string))))

; primReadStringFile
(define (prim.read-string-file filename)
  (if (file-exists? filename)
      (let ((str (call-with-input-file filename
		   (lambda (port)
		     (port->string port)))))
	(io-success str))
      (io-failure (format '#f "File not found: ~A~%" filename))))

(define (port->string port)
  (call-with-output-string
   (lambda (string-port)
     (copy-till-eof port string-port))))

(define (copy-till-eof in-port out-port)
  (do ((ch (read-char in-port) (read-char in-port)))
      ((eof-object? ch))
    (write-char ch out-port)))

; primWriteStringFile
(define (prim.write-string-file filename contents state)
 (declare (ignore state))
 (box
  (let ((stream (lisp:open (haskell-string->string filename)
			   :direction :output 
			   :if-exists :overwrite
			   :if-does-not-exist :create)))
    (print-haskell-string contents stream)
    (close-output-port stream)
    (io-success))))
        
;primAppendStringFile
(define (prim.append-string-file filename contents state)
 (declare (ignore state))
 (box
  (let ((stream (lisp:open (haskell-string->string filename)
			   :direction :output 
			   :if-exists :append
			   :if-does-not-exist '())))
    (cond ((not (eq? stream '()))
           (print-haskell-string contents stream)
           (close-output-port stream)
	   (io-success))
          (else
	   (io-failure "Can't open file"))))))

; primReadBinFile
(define (prim.read-bin-file name)
  (let ((bin (lisp-read name)))
    (if (and (pair? bin) (eq? (car bin) ':binary))
	(io-success/bin bin)
	(io-failure "Not a bin file"))))

; primWriteBinFile
(define (prim.write-bin-file name contents)
  (let ((stream (lisp:open name :direction :output 
			   :if-exists :overwrite
			   :if-does-not-exist :create)))
    (write (cons ':binary contents) stream)
    (close-output-port stream)
    (io-success)))

; primAppendBinFile
(define (prim.append-bin-file name contents)
 (let ((bin (lisp-read name)))
   (if (and (pair? bin) (eq? (car bin) ':binary))
       (let ((stream (lisp:open name :direction :output :if-exists :overwrite)))
	 (write (append bin contents) stream)
	 (io-success))
       (io-failure "Can't open Bin file"))))

; primDeleteFile
(define (prim.delete-file name)
  (if (file-exists? name)
      (if (lisp:delete-file name)
	  (io-success)
	  (io-failure "Can't delete file"))
      (io-failure "File not found")))

; primStatusFile
(define (prim.status-file name)
  (if (file-exists? name)
      (io-success "frw")
      (io-failure (format '#f "File ~A not found" name))))

;primReadStdin
(define (prim.read-string-stdin state)
  (declare (ignore state))
  (cond (*stdin-read*
	 (haskell-runtime-error "Multiple ReadChan from stdin"))
	(else
	 (setf *stdin-read* '#t)
	 (delay (read-next-char)))))

(define (read-next-char)
  (when (and *emacs-mode* (not *emacs-notified*))
    (setf *emacs-notified* '#t)
    (notify-input-request))
  (let ((ch (read-char)))
    (if (eof-object? ch)
	'()
	(cons (box (char->integer ch))
	      (delay (read-next-char))))))

; primWriteStdout
(define (prim.write-string-stdout string state)
  (declare (ignore state))
  (print-haskell-string string (current-output-port))
  (box (io-success)))

; primReadBinStdin
(define (prim.read-bin-stdin)
  (haskell-runtime-error  "ReadBinChan not implemented"))

; primWriteBinStdout
(define (prim.write-bin-stdout bin)
  (declare (ignore bin))
  (haskell-runtime-error  "WriteBinChan not implemented"))

;;; %%% probably bogus
; primGetEnv
(define (prim.getenv name)
  (io-success (getenv name)))

(define (lisp-read file)
  (if (not (file-exists? file))
      'error
      (call-with-input-file file
        (lambda (port)
	  (lisp:read port '#f 'error '#f)))))

(define-integrable (prim.returnio x s)
  (declare (ignore s))
  x)

(define-integrable (prim.getstate x)
  (declare (ignore x))
  'state)

(define-integrable (prim.getres x)
  (force x))




