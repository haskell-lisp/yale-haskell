;;; ==================================================================
;;; File: 		haskell.el     				   ;;;
;;;                                                                ;;;
;;;			Author: 	A. Satish Pai		   ;;;
;;;                                     Maria M. Gutierrez         ;;;
;;;                                     Dan Rabin (Jul-1991)       ;;;
;;; ==================================================================

;;; Description: Haskell mode for GNU Emacs.

;;; Related files:  comint.el

;;; Contents:

;;;  Update Log

;;;  Known bugs / problems
;;;  - the haskell editing mode (indentation, etc) is still missing.
;;;  - the handling for errors from haskell needs to be rethought.
;;;  - general cleanup of code.


;;;  Errors generated

;;; ==================================================================
;;; Haskell mode for editing files, and an Inferior Haskell mode to
;;; run a Haskell process. This file contains stuff snarfed and 
;;; modified from tea.el, scheme.el, etc. This file may be freely
;;; modified; however, if you have any bug-corrections or useful
;;; improvements, I'd appreciate it if you sent me the mods so that
;;; I can merge them into the version I maintain.
;;;
;;; The inferior Haskell mode requires comint.el. 
;;; 
;;; You might want to add this to your .emacs to go automagically
;;; into Haskell mode while finding .hs files.
;;; 
;;;   (setq auto-mode-alist 
;;;         (cons '("\\.hs$" . haskell-mode)
;;;                auto-mode-alist)_)
;;;
;;; To use this file, set up your .emacs to autoload this file for 
;;; haskell-mode. For example:
;;; 
;;;    (autoload 'haskell-mode "$HASKELL/emacs-tools/haskell.elc" 
;;;       "Load Haskell mode" t)
;;;
;;;    (autoload 'run-mode "$HASKELL/emacs-tools/haskell.elc" 
;;;       "Load Haskell mode" t)
;;;
;;; [Note: The path name given above is Yale specific!! Modify as
;;; required.]
;;; ================================================================

;;; Announce your existence to the world at large.

(provide 'haskell)


;;; Load these other files.

(require 'comint)        ; Olin Shivers' comint mode is the substratum




;;; ================================================================
;;; Declare a bunch of variables.
;;; ================================================================


;;; User settable (via M-x set-variable and M-x edit-options)

(defvar haskell-program-name (getenv "HASKELLPROG")
  "*Program invoked by the haskell command")

(defvar *haskell-buffer* "*haskell*"
  "*Name of the haskell process buffer")

(defvar *haskell-show-error* 1
  "*If not nil move to the buffer where the error was found")


(defvar haskell-auto-create-process t
  "*If not nil, create a Haskell process automatically when required to evaluate or compile Haskell code")

(defvar *haskell-debug-in-lisp* nil
  "*If not nil, enter Lisp debugger on error; otherwise, automagically return
to Haskell top-level.")


;;; Command interface related variables

(defvar *emacs* nil
  "When not nil means haskell is in emacs mode")


;;; Pad/buffer Initialization variables

(defvar haskell-main-pad "\*Main-pad\*"
  "Scratch pad associated with module Main")

(defvar haskell-main-file "Main")

(defvar haskell-main-module "Main")


(defvar *last-loaded* haskell-main-file
  "Last file loaded with a :load command - Defaults to Main")

(defvar *last-loaded-modtime* nil
  "Modification time of last file loaded, used to determine whether it
needs to be reloaded.")

(defvar *last-module* haskell-main-module
  "Last module set with a :module command - Defaults to Main")

(defvar *last-pad* haskell-main-pad
  "Last pad saved with a :save command - Defaults to Main")


;;; These are used for haskell-tutorial mode.

(defvar *ht-source-file* "$HASKELL/progs/tutorial/tutorial.hs")
(defvar *ht-temp-buffer* nil)
(defvar *ht-file-buffer* "Haskell-Tutorial-Master")



;;; ================================================================
;;; Haskell editing mode stuff
;;; ================================================================

;;; Leave this place alone...
;;; The definitions below have been pared down to the bare
;;; minimum; they will be restored later.
;;;
;;; -Satish 2/5.

;;; Keymap for Haskell mode
(defvar haskell-mode-map nil
  "Keymap used for haskell-mode")

(defun haskell-establish-key-bindings (keymap)
  (define-key keymap "\C-ce"    'haskell-eval)
  (define-key keymap "\C-cr"    'haskell-run)
  (define-key keymap "\C-cm"    'haskell-run-main)
  (define-key keymap "\C-c\C-r" 'haskell-run-file)
  (define-key keymap "\C-cp"    'haskell-get-pad)
  (define-key keymap "\C-c\C-o" 'haskell-optimizers)
  (define-key keymap "\C-c\C-p" 'haskell-printers)
  (define-key keymap "\C-cc"    'haskell-compile)
  (define-key keymap "\C-cl"    'haskell-load)
  (define-key keymap "\C-ch"    'haskell-switch)
  (define-key keymap "\C-c:"    'haskell-command)
  (define-key keymap "\C-cq"    'haskell-exit)
  (define-key keymap "\C-ci"    'haskell-interrupt)
  (define-key keymap "\C-cu"    'haskell-edit-unit)
  (define-key keymap "\C-cd"    'haskell-please-recover)
  (define-key keymap "\C-c("    'haskell-ensure-lisp-mode)
  (define-key keymap "\C-c)"    'haskell-resume-command-loop))


(if haskell-mode-map
    nil
    (progn
      (setq haskell-mode-map (make-sparse-keymap))
      ;; Compiler commands
      (haskell-establish-key-bindings haskell-mode-map)
      ))

(defvar haskell-mode-syntax-table nil
  "Syntax table used for haskell-mode")

(if haskell-mode-syntax-table
    nil
    (setq haskell-mode-syntax-table (standard-syntax-table)))

;;; Command for invoking the Haskell mode
(defun haskell-mode nil
  "Major mode for editing Haskell code to run in Emacs
The following commands are available:
\\{haskell-mode-map}

A Haskell process can be fired up with \"M-x haskell\". 

Customization: Entry to this mode runs the hooks that are the value of variable 
haskell-mode-hook.

Windows:

There are 3 types of windows associated with Haskell mode.  They are:
   *haskell*:  which is the process window.
   Pad:        which are buffers available for each module.  It is here
               where you want to test things before preserving them in a
               file.  Pads are always associated with a module.
               When issuing a command:
                 The pad and its associated module are sent to the Haskell
                 process prior to the execution of the command.
   .hs:        These are the files where Haskell programs live.  They
               have .hs as extension.
               When issuing a command:
                 The file is sent to the Haskell process prior to the
                 execution of the command.

Commands:

Each command behaves differently according to the type of the window in which 
the cursor is positioned when the command is issued .

haskell-eval:   \\[haskell-eval]
  Always promts user for a Haskell expression to be evaluated.  If in a
  .hs file buffer, then the cursor tells which module is the current 
  module and the pad for that module (if any) gets loaded as well.

haskell-run:    \\[haskell-run]
  Always queries for a variable of type Dialogue to be evaluated.

haskell-run-main:    \\[haskell-run-main]
  Run Dialogue named main.

haskell-run-file:   \\[haskell-run-file]
  Runs a file.  Ideally the file has a set of variable of type Dialogue
  that get evaluated.

haskell-mode:   \\[haskell-mode]
  Puts the current buffer in haskell mode.

haskell-compile:   \\[haskell-compile]
  Compiles file in current buffer.

haskell-load:   \\[haskell-load]
  Loads file in current buffer.

haskell-pad:   \\[haskell-pad]
  Creates a scratch pad for the current module.

haskell-optimizers:  \\[haskell-optimizers]
  Shows the list of available optimizers.  Commands for turning them on/off.

haskell-printers:  \\[haskell-printers]
  Shows the list of available printers.  Commands for turning them on/off.

haskell-command:   \\[haskell-command]
  Prompts for a command to be sent to the command interface.  You don't
  need to put the : before the command.

haskell-quit:   \\[haskell-quit]
  Terminates the haskell process.

switch-to-haskell:   \\[switch-to-haskell]
  Switchs to the inferior Haskell buffer (*haskell*) and positions the
  cursor at the end of the buffer.

haskell-interrupt:   \\[haskell-interrupt]
  Interrupts haskell process and resets it.

haskell-edit-unit:   \\[haskell-edit-unit]
  Edit the .hu file for the unit containing this file.
"
  (interactive)
  (kill-all-local-variables)
  (use-local-map haskell-mode-map)
  (setq major-mode 'haskell-mode)
  (setq mode-name "Haskell")
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'indent-relative-maybe)
  ;(setq local-abbrev-table haskell-mode-abbrev-table)
  (set-syntax-table haskell-mode-syntax-table)
  ;(setq tab-stop-list haskell-tab-stop-list) ;; save old list??
  (run-hooks 'haskell-mode-hook))
 


;;;================================================================
;;; Inferior Haskell stuff
;;;================================================================


(defvar inferior-haskell-mode-map nil)

(if inferior-haskell-mode-map
    nil
  (setq inferior-haskell-mode-map
	(full-copy-sparse-keymap comint-mode-map))
  ;;; Haskell commands
  (haskell-establish-key-bindings inferior-haskell-mode-map)
  (define-key inferior-haskell-mode-map "\C-m"     'haskell-send-input))

(defvar haskell-source-modes '(haskell-mode)
  "*Used to determine if a buffer contains Haskell source code.
If it's loaded into a buffer that is in one of these major modes, 
it's considered a Haskell source file.")

(defvar haskell-prev-l/c-dir/file nil
  "Caches the (directory . file) pair used in the last invocation of
haskell-run-file.")

(defvar haskell-prompt-pattern "^[A-Z]\\([A-Z]\\|[a-z]\\|[0-9]\\)*>\\s-*"
  "Regular expression capturing the Haskell system prompt.")

(defvar haskell-prompt-ring ()
  "Keeps track of input to haskell process from the minibuffer")

(defvar tea-prompt-pattern "^>+\\s-*"
   "Regular expression capturing the T system prompt.")

(defvar haskell-version "Yale University Haskell Version 0.8, 1991"
  "Current Haskell system version")  

(defun inferior-haskell-mode-variables ()
  nil)  


;;; INFERIOR-HASKELL-MODE (adapted from comint.el)

(defun inferior-haskell-mode ()
  "Major mode for interacting with an inferior Haskell process.

The following commands are available:
\\{inferior-haskell-mode-map}

A Haskell process can be fired up with \"M-x haskell\". 

Customization: Entry to this mode runs the hooks on comint-mode-hook and
inferior-haskell-mode-hook (in that order).

You can send text to the inferior Haskell process from other buffers containing
Haskell source.  


Windows:

There are 3 types of windows in the inferior-haskell-mode.  They are:
   *haskell*:  which is the process window.
   Pad:        which are buffers available for each module.  It is here
               where you want to test things before preserving them in a
               file.  Pads are always associated with a module.
               When issuing a command:
                 The pad and its associated module are sent to the Haskell
                 process prior to the execution of the command.
   .hs:        These are the files where Haskell programs live.  They
               have .hs as extension.
               When issuing a command:
                 The file is sent to the Haskell process prior to the
                 execution of the command.

Commands:

Each command behaves differently according to the type of the window in which 
the cursor is positioned when the command is issued.

haskell-eval:   \\[haskell-eval]
  Always promts user for a Haskell expression to be evaluated.  If in a
  .hs file, then the cursor tells which module is the current module and
  the pad for that module (if any) gets loaded as well.

haskell-run:    \\[haskell-run]
  Always queries for a variable of type Dialogue to be evaluated.

haskell-run-main:    \\[haskell-run-main]
  Run Dialogue named main.

haskell-run-file:   \\[haskell-run-file]
  Runs a file.  Ideally the file has a set of variable of type Dialogue
  that get evaluated.

haskell-mode:   \\[haskell-mode]
  Puts the current buffer in haskell mode.

haskell-compile:   \\[haskell-compile]
  Compiles file in current buffer.

haskell-load:   \\[haskell-load]
  Loads file in current buffer.

haskell-pad:   \\[haskell-pad]
  Creates a scratch pad for the current module.

haskell-optimizers:  \\[haskell-optimizers]
  Shows the list of available optimizers.  Commands for turning them on/off.

haskell-printers:  \\[haskell-printers]
  Shows the list of available printers.  Commands for turning them on/off.

haskell-command:   \\[haskell-command]
  Prompts for a command to be sent to the command interface.  You don't
  need to put the : before the command.

haskell-quit:   \\[haskell-quit]
  Terminates the haskell process.

switch-to-haskell:   \\[switch-to-haskell]
  Switchs to the inferior Haskell buffer (*haskell*) and positions the
  cursor at the end of the buffer.

haskell-interrupt:   \\[haskell-interrupt]
  Interrupts haskell process and resets it.

haskell-edit-unit:   \\[haskell-edit-unit]
  Edit the .hu file for the unit containing this file.

The usual comint functions are also available. In particular, the 
following are all available:

comint-bol: Beginning of line, but skip prompt. Bound to C-a by default.
comint-delchar-or-maybe-eof: Delete char, unless at end of buffer, in 
            which case send EOF to process. Bound to C-d by default.

Note however, that the default keymap bindings provided shadow some of
the default comint mode bindings, so that you may want to bind them 
to your choice of keys. 

Comint mode's dynamic completion of filenames in the buffer is available.
(Q.v. comint-dynamic-complete, comint-dynamic-list-completions.)

If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it."

  (interactive)
  (comint-mode)
  (setq comint-prompt-regexp haskell-prompt-pattern)
  ;; Customise in inferior-haskell-mode-hook
  (inferior-haskell-mode-variables) 
  (setq major-mode 'inferior-haskell-mode)
  (setq mode-name "Inferior Haskell")
  (setq mode-line-process '(": %s : busy"))
  (use-local-map inferior-haskell-mode-map)
  (setq comint-input-filter 'haskell-input-filter)
  (setq comint-input-sentinel 'ignore)
  (setq comint-get-old-input 'haskell-get-old-input)
  (run-hooks 'inferior-haskell-mode-hook)
    ;Do this after the hook so the user can mung INPUT-RING-SIZE w/his hook.
    ;The test is so we don't lose history if we run comint-mode twice in
    ;a buffer.
  (setq haskell-prompt-ring (make-ring input-ring-size)))


;;; Install the process communication commands in the
;;; inferior-haskell-mode keymap.

(defvar inferior-haskell-mode-hook 'haskell-fresh-start
  "*Hook for customizing inferior-Haskell mode")

(defun haskell-input-filter (str)
  "Don't save whitespace."
  (not (string-match "\\s *" str)))



;;; ==================================================================
;;; Handle output from Haskell process
;;; ==================================================================


;;; This keeps track of the status of the haskell process.
;;; Values are:
;;; busy -- The process is busy.
;;; ready -- The process is ready for a command.
;;; input -- The process is waiting for input.
;;; dead -- The process is dead (exited or not started yet).


(defvar *haskell-status* 'dead
  "Status of the haskell process")

(defun set-haskell-status (value)
  (setq *haskell-status* value)
  (update-mode-line))

(defun get-haskell-status ()
  *haskell-status*)

(defun update-mode-line ()
  (save-excursion
    (set-buffer *haskell-buffer*)
    (cond ((eq *haskell-status* 'ready)
	   (setq mode-line-process '(": %s: ready")))
	  ((eq *haskell-status* 'input)
	   (setq mode-line-process '(": %s: input")))
	  ((eq *haskell-status* 'busy)
	   (setq mode-line-process '(": %s: busy")))
	  ((eq *haskell-status* 'dead)
	   (setq mode-line-process '(": %s: dead")))
	  (t
	   (haskell-mode-error "Confused about status of haskell process!")))
    ;; Yes, this is the officially sanctioned technique for forcing
    ;; a redisplay of the mode line.
    (set-buffer-modified-p (buffer-modified-p))))


;;; Filter
;;; The haskell process produces output with embedded control codes.
;;; These control codes are used to keep track of what kind of input
;;; the haskell process is expecting.  Ordinary output is just displayed.
;;;
;;; This is kind of complicated because control sequences can be broken
;;; across multiple batches of text received from the haskell process.
;;; If the string ends in the middle of a control sequence, save it up
;;; for the next call.

(defvar *haskell-saved-output* nil)

(defun process-haskell-output (process str)
  "Filter for output from Yale Haskell command interface"
  (let ((idx     0)
	(lastidx 0)
	(data    (match-data)))
    (unwind-protect
	(progn
	  ;; If there was saved output from last time, glue it in front of the
	  ;; newly received input.
	  (if *haskell-saved-output*
	      (progn
		(setq str (concat *haskell-saved-output* str))
		(setq *haskell-saved-output* nil)))
	  ;; Loop, looking for complete command sequences.
	  ;; Set idx to point to the first one.
	  ;; lastidx points to next character to be processed.
	  (while (setq idx (ci-response-start str lastidx))
	    ;; Display any intervening ordinary text.
	    (if (not (eq idx lastidx))
		(haskell-display-output (substring str lastidx idx)))
	    ;; Now dispatch on the particular command sequence found.
	    ;; Handler functions are called with the string and start index
	    ;; as arguments, and should return the index of the "next"
	    ;; character -- usually (match-end 0).
	    (setq lastidx (funcall (ci-response-handler str idx) str idx)))
	  ;; Look to see whether the string ends with an incomplete 
	  ;; command sequence.
	  ;; If so, save the tail of the string for next time.
	  (if (setq idx (ci-prefix-start str lastidx))
	      (setq *haskell-saved-output* (substring str idx))
	      (setq idx (length str)))
	  ;; Display any leftover ordinary text.
	  (if (not (eq idx lastidx))
	      (haskell-display-output (substring str lastidx idx))))
      (store-match-data data))))



;;; Here is code for matching command sequences from haskell.

;;; The first entry of each item is the full regexp; the second is a prefix
;;; regexp; the third is a handler function to call.

(defvar *ci-responses*
  '(("\C-Ar"          "\C-A"            haskell-got-ready)
    ("\C-Ai"          "\C-A"            haskell-got-input-request)
    ("\C-Ae"          "\C-A"            haskell-got-error)
    ("\C-Ap.*\n"      "\C-A\\(p.*\\)?"  haskell-got-printers)
    ("\C-Ao.*\n"      "\C-A\\(o.*\\)?"  haskell-got-optimizers)
    ("\C-As.*\n"      "\C-A\\(s.*\\)?"  haskell-got-message)
    ;; This is the error string for T
;    ("^\\*\\* Error"
;     "^\\*\\(\\*\\( \\(E\\(r\\(r\\(or?\\)?\\)?\\)?\\)?\\)?\\)?"
;     haskell-got-lisp-error)
    ;; This is the prompt for Lucid's break loop
    ("\n-> "    "\n\\(-\\(> ?\\)?\\)?" haskell-got-lisp-error)
    ;; This is the prompt for CMU CL's break loop
    ("0\\] "    "0\\(\\] ?\\)?" haskell-got-lisp-error)
    ;; This is the prompt for AKCL's break loop
    ("USER>>" "U\\(S\\(E\\(R\\(>>?\\)?\\)?\\)?\\)?" haskell-got-lisp-error)
    ;; This is the prompt for Allegro CL
    ("USER(.*):" "U\\(S\\(E\\(R\\((.*)?\\)?\\)?\\)?\\)?" haskell-got-lisp-error)
    ;; This is the prompt for Harlequin Lispworks
    ("USER .* : .* >" "U\\(S\\(E\\(R\\( .*\\( \\(:\\( .*\\( >?\\)?\\)?\\)?\\)?\\)?\\)?\\)?\\)?" haskell-got-lisp-error)
    ))

(defun command-match-regexp (x) (car x))
(defun command-prefix-regexp (x) (car (cdr x)))
(defun command-handler (x) (car (cdr (cdr x))))

(defun glue-together (extractor)
  (let ((result (concat "\\(" (funcall extractor (car *ci-responses*)) "\\)"))
	(stuff  (cdr *ci-responses*)))
    (while stuff
      (setq result
	    (concat result "\\|\\(" (funcall extractor (car stuff)) "\\)"))
      (setq stuff (cdr stuff)))
    result))

(defvar *ci-response-regexp* (glue-together 'command-match-regexp))

(defvar *ci-prefix-regexp*
  (concat "\\(" (glue-together 'command-prefix-regexp) "\\)\\'"))
			   
(defun ci-response-start (str idx)
  (string-match *ci-response-regexp* str idx))

(defun ci-prefix-start (str idx)
  (string-match *ci-prefix-regexp* str idx))

(defun ci-response-handler (str idx)
  (let ((list    *ci-responses*)
	(result  nil))
    (while (and list (null result))
      (if (eq (string-match (command-match-regexp (car list)) str idx) idx)
	  (setq result (command-handler (car list)))
	  (setq list (cdr list))))
    (if (null result)
	(haskell-mode-error "Failed to find command handler!!!"))
    result))


;;; Here are the low-level handler functions.  Basically, these
;;; guys just parse the input for the command sequence and then call some
;;; other function to do the real work.

(defun haskell-got-ready (str idx)
  (let ((result  (match-end 0)))
    (haskell-reset)
    result))

(defun haskell-got-input-request (str idx)
  (let ((result  (match-end 0)))
    (get-user-input)
    result))

(defun haskell-got-error (str idx)
  (let ((result  (match-end 0)))
    (haskell-error-handler)
    result))

(defun haskell-got-printers (str idx)
  (let ((result  (match-end 0)))
    (update-printers-list (substring str (+ idx 2) (- result 1)))
    result))

(defun haskell-got-optimizers (str idx)
  (let ((result  (match-end 0)))
    (update-optimizers-list (substring str (+ idx 2) (- result 1)))
    result))

(defun haskell-got-message (str idx)
  (let ((result  (match-end 0)))
    (message (substring str (+ idx 2) (- result 1)))
    result))

(defun haskell-got-lisp-error (str idx)
  (haskell-handle-lisp-error idx str)
  (length str))


;;; Something really bad happened and we got a Lisp error.
;;; Either let the user mess around in the Lisp debugger, or else
;;; just get out of it and go back into the Haskell command loop.

(defun haskell-handle-lisp-error (location str)
  (haskell-display-output (substring str location))
  (if *emacs*
      ;; Don't ding if we were already in the break loop when the
      ;; error happened.
      (progn
	(ding)
	(if *haskell-debug-in-lisp*
	    (haskell-talk-to-lisp)
	    (haskell-flush-commands-and-reset)))))

(defun loaded-tutorial-p ()
  (and *ht-temp-buffer*
       (get-buffer *ht-temp-buffer*)
       (equal *last-loaded* (buffer-file-name (get-buffer *ht-temp-buffer*)))))

(defun haskell-flush-commands-and-reset ()
  (haskell-flush-command-queue)
  (save-excursion
    (switch-to-buffer *haskell-buffer*)
    (haskell-ensure-lisp-mode)
    (haskell-resume-command-loop)))

(defun haskell-talk-to-lisp ()
  (pop-to-buffer *haskell-buffer*)
  (goto-char (point-max))
  (haskell-ensure-lisp-mode))


(defun haskell-resume-command-loop ()
  "Resumes Haskell command processing after debugging in Lisp.  \\[haskell-resume-command-loop]"
  (interactive)
  (if (not *emacs*)
      (progn
	(process-send-string "haskell" "(mumble-user::restart-haskell)\n")
	(haskell-ensure-emacs-mode))))



;;; Displays output at end of given buffer.
;;; This function only ensures that the output is visible, without 
;;; selecting the buffer in which it is displayed.
;;; Note that just using display-buffer instead of all this rigamarole
;;; won't work; you need to temporarily select the window containing
;;; the *haskell-buffer*, or else the display won't be scrolled to show
;;; the new output.
;;; *** This should really position the window in the buffer so that 
;;; *** the point is on the last line of the window.

(defun haskell-display-output (str)
  (if (eq (get-haskell-status) 'dead)
      (save-excursion
	(set-buffer *haskell-buffer*)
	(haskell-display-output-aux str))
      (let ((window  (selected-window)))
	(unwind-protect
	    (progn
	      (pop-to-buffer *haskell-buffer*)
	      (haskell-display-output-aux str))
	  (select-window window)))))

(defun haskell-display-output-aux (str)
  (haskell-move-marker)
  (insert str)
  (haskell-move-marker))



;;; The haskell process says it's expecting the user to type in some input.
;;; Switch to the *haskell-buffer* so the user can type things.
;;; Once we have received an input message, stay in input mode until
;;; we get a ready message back from haskell.  This permits multiple
;;; data messages to be sent to haskell from a single input request.
;;;
;;; This user interface isn't really ideal.  You can be typing
;;; away in some other buffer and all of a sudden have Haskell decide
;;; it wants some input, and bingo!  You're switched into the Haskell
;;; buffer behind your back.  There's also the problem that you're
;;; left in the Haskell buffer afterwards, instead of getting swapped
;;; back into the buffer that was current when the input request was
;;; received.
;;; Not sure how to fix this -- seems like a totally synchronous interface
;;; would be worse....

(defun get-user-input ()
  (message "Haskell is waiting for input...")
  (pop-to-buffer *haskell-buffer*)
  (goto-char (point-max))
  (set-haskell-status 'input)
  (haskell-pop-data-queue))


;;; The haskell process says it encountered an error.  
;;; Remember to flush the command queue before continuing.

(defun haskell-error-handler ()
  (ding)
  (haskell-flush-command-queue)
  ;; *** See comments below for why this is disabled.
;  (if *haskell-show-error*
;    (haskell-show-error))
  (set-haskell-status 'ready)
  (haskell-end-interaction nil))
  

;;; Pop up a buffer containing the file with the error, and put the 
;;; point on the line where the error was reported.
;;; *** This code does the wrong thing in some situations.  For example,
;;; *** if you type in garbage to C-c e, it thinks that it should
;;; *** show you the last pad sent to the haskell process, which is
;;; *** clearly bogus.
;;; *** I also think it would be better interaction style to have to
;;; *** request to be shown the error explicitly, instead of unexpectedly
;;; *** being thrown into some other buffer.

;;; Error handling Variables

(defvar *yh-error-def*  "Error occured in definition of\\s *")
(defvar *yh-error-line* "at line\\s *")
(defvar *yh-error-file* "of file\\s *")
(defvar *haskell-line* "\\([0-9]\\)*")

(defun haskell-show-error ()
  "Point out error to user if possible"
  (set-buffer *haskell-buffer*)
  (save-excursion
    (let ((function-name nil)
	  (line-number   nil)
	  (filename      nil))
      (if (and (setq function-name (get-function-name))
	       (setq line-number (get-line-number))
	       (setq filename (get-filename)))
	  (point-error-to-user function-name line-number filename)))))

(defvar *haskell-function-name*
  "\\([a-z]\\|[A-Z]\\|[0-9]\\|'\\|_\\|\-\\)*")

(defun get-function-name ()
  (if (and (re-search-backward *yh-error-def* (point-min) t)  
	   (re-search-forward *yh-error-def* (point-max) t))
      (let ((beg (point)))
	(if (re-search-forward *haskell-function-name* (point-max) t)
	    (buffer-substring beg (point))
	    nil))
      nil))

(defun get-line-number ()
  (if (re-search-forward  *yh-error-line* (point-max) t)
      (let ((beg  (point)))
	(if (re-search-forward *haskell-line* (point-max) t)
	    (string-to-int (buffer-substring beg (point)))
	    nil))
      nil))
	

(defun get-filename ()
  (if (re-search-forward  *yh-error-file* (point-max) t)
      (let ((beg  (point)))
	(if (re-search-forward "\\($\\| \\|\t\\)" (point-max) t)
	    (buffer-substring beg (point))
	    nil))
      nil))

(defun point-error-to-user (function-name line-number filename)
  (if (equal filename "Interactive")
    (pop-to-buffer *last-pad*)
    (let ((fname (strip-fext filename)))
      (if (get-buffer fname)
	(pop-to-buffer fname)
	(find-file-other-window filename))))
  (goto-line line-number))


;;; The haskell process says it is ready to execute another command.
;;; Tell the user the last command has finished and execute the next
;;; command from the queue, if there is one.

(defun haskell-reset ()
  (set-haskell-status 'ready)
  (haskell-pop-command-queue))




;;; ==================================================================
;;; Command queue utilities
;;; ==================================================================

;;; Here's the stuff for managing the command queue.
;;; There are three kinds of things that show up in the queue:
;;; * Strings to be sent as commands to the haskell process.  These 
;;;   are queued with haskell-send-command.
;;; * Other stuff to be sent to the haskell process (e.g., text to
;;;   be read as dialogue input).  These are queued with
;;;   haskell-send-data.
;;; * Messages indicating start of an interaction sequence.  These
;;;   are just shown to the user.  These are added to the queue with
;;;   haskell-begin-interaction.
;;; * Messages indicating end of an interaction sequence.  These are
;;;   queued with haskell-end-interaction.
;;;
;;; Representationally, the queue is just a list of conses.  The car of each
;;; entry is a symbol that identifies the kind of queue entry, and the cdr
;;; is associated data.  Only the functions in this section need to know
;;; about the internal format of the queue.


(defvar *command-interface-queue* nil
  "Contains the commands to be sent to the Haskell command interface")


;;; Here's a helper function.

(defun haskell-queue-or-execute (fn request data)
  (cond (*command-interface-queue*
	 (setq *command-interface-queue*
	       (nconc *command-interface-queue* (list (cons request data)))))
	((eq (get-haskell-status) 'ready)
	 (funcall fn data))
	(t
	 (setq *command-interface-queue* (list (cons request data))))))
  

;;; Queue a command.

(defun haskell-send-command (str)
  "Queues STRING for transmission to haskell process."
  (haskell-queue-or-execute 'haskell-send-command-aux 'command str))

(defun haskell-send-command-aux (str)
  (process-send-string "haskell" str)
  (process-send-string "haskell" "\n")
  (if (not (eq (get-haskell-status) 'input))
      (set-haskell-status 'busy)))


;;; Queue a begin-interaction message.

(defvar *begin-interaction-delimiter* nil ;; "-------------\n"
  "*Delimiter showing an interaction has begun")

(defun haskell-begin-interaction (msg)
  (haskell-queue-or-execute 'haskell-begin-interaction-aux 'begin msg))

(defun haskell-begin-interaction-aux (msg)
  (if *begin-interaction-delimiter*
      (haskell-display-output *begin-interaction-delimiter*))
  (if msg
      (haskell-display-output (concat "\n" msg "\n"))))


;;; Queue an end-interaction message.

(defvar *end-interaction-delimiter* nil ;; "\n--- ready ---\n\n"
  "*Delimiter showing an interaction has ended")

(defun haskell-end-interaction (msg)
  (haskell-queue-or-execute 'haskell-end-interaction-aux 'end msg))

(defun haskell-end-interaction-aux (msg)
  (if *end-interaction-delimiter*
      (haskell-display-output *end-interaction-delimiter*))
  (if msg
      (message "%s" msg)))


;;; Queue data.  This is treated a little differently because we want
;;; text typed in as input to the program to be sent down the pipe to
;;; the process before processing end-interaction messages and additional
;;; commands in the queue.

(defun haskell-send-data (str)
  (cond ((assoc 'data *command-interface-queue*)
	 (setq *command-interface-queue*
	       (merge-data-into-queue
		   (list (cons 'data str))
		   *command-interface-queue*
		   *command-interface-queue*
		   nil)))
	((or (eq (get-haskell-status) 'ready) (eq (get-haskell-status) 'input))
	 (haskell-send-command-aux str))
	(t
	 (setq *command-interface-queue* (list (cons 'data str))))))

(defun merge-data-into-queue (new head tail lasttail)
  (cond ((null tail)
	 (rplacd lasttail new)
	 head)
	((eq (car (car tail)) 'data)
	 (merge-data-into-queue new head (cdr tail) tail))
	(lasttail
	 (rplacd lasttail new)
	 (rplacd new tail)
	 head)
	(t
	 (rplacd new tail)
	 new)))


;;; This function is called when the haskell process reports that it
;;; has finished processing a command.  It sends the next queued
;;; command (if there is one) down the pipe.

(defun haskell-pop-command-queue ()
  (if *command-interface-queue*
    (let ((entry  (car *command-interface-queue*)))
      (setq *command-interface-queue* (cdr *command-interface-queue*))
      (cond ((eq (car entry) 'command)
	     (haskell-send-command-aux (cdr entry)))
	    ((eq (car entry) 'begin)
	     (haskell-begin-interaction-aux (cdr entry))
	     (haskell-pop-command-queue))
	    ((eq (car entry) 'end)
	     (haskell-end-interaction-aux (cdr entry))
	     (haskell-pop-command-queue))
	    ((eq (car entry) 'data)
	     (haskell-send-command-aux (cdr entry)))
	    (t
	     (haskell-mode-error "Invalid command in queue!!!"))
	    ))))


;;; This function is called when the haskell process reports that it
;;; wants to read some input.  If there's queued data, send it; but
;;; don't do commands or messages on the queue.
;;; Remember, we can send multiple pieces of input data for one input
;;; request from haskell.

(defun haskell-pop-data-queue ()
  (if *command-interface-queue*
      (let ((entry  (car *command-interface-queue*)))
	(if (eq (car entry) 'data)
	    (progn
	      (setq *command-interface-queue* (cdr *command-interface-queue*))
	      (haskell-send-command-aux (cdr entry))
	      (haskell-pop-data-queue))))))


;;; This is called when there is an error.

(defun haskell-flush-command-queue ()
  (setq *command-interface-queue* nil))
	


;;; ==================================================================
;;; Interactive commands
;;; ==================================================================


;;; HASKELL and RUN HASKELL
;;; ------------------------------------------------------------------

;;; These are the two functions that start a Haskell process.
;;; Rewritten to avoid doing anything if a Haskell process
;;; already exists.  1991-Sep-09 Dan Rabin.

;;; *** Dan says:
;;; *** If the *haskell* buffer still exists, and the process has status
;;; *** `dead', the usual evaluation commands don't create a new one, so no
;;; *** evaluation happens.


(defun haskell ()
  "Run an inferior Haskell process with input and output via buffer *haskell*.
Takes the program name from the variable haskell-program-name.  
Runs the hooks from inferior-haskell-mode-hook 
(after the comint-mode-hook is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (interactive)
  (let ((haskell-buffer  (get-buffer *haskell-buffer*)))
    (if (not (and haskell-buffer (comint-check-proc haskell-buffer)))
	(progn
	  (setq haskell-buffer
		(apply 'make-comint
		       "haskell"
		       haskell-program-name
		       nil
                       nil))
	  (save-excursion
	    (set-buffer haskell-buffer)
	    (inferior-haskell-mode))
	  (display-buffer haskell-buffer)))))


;;; Fresh start

(defun haskell-fresh-start ()
  (set-haskell-status 'busy)
  (setq *command-interface-queue* nil)
  (setq *last-loaded* haskell-main-file)
  (setq *last-pad* haskell-main-pad)
  (setq *emacs* nil)
  (setq *haskell-saved-output* nil)
  (haskell-ensure-emacs-mode))


;;; Called from evaluation and compilation commands to start up a Haskell
;;; process if none is already in progress.

(defun haskell-maybe-create-process ()
  (if haskell-auto-create-process
      (haskell)))


;;; This is called from HASKELL-FRESH-START to ensure that
;;; there is a pad when starting up a Haskell interaction.

(defun haskell-ensure-emacs-mode ()
  (create-main-pad)
  (setq *emacs* t)
  (ci-emacs))


;;; This is called when a Lisp error has been detected.

(defun haskell-ensure-lisp-mode ()
  "Switch to talking to Lisp.  \\[haskell-ensure-lisp-mode]"
  (interactive)
  (setq *emacs* nil))


;;; HASKELL-GET-PAD
;;; ------------------------------------------------------------------

;;; This always puts the pad buffer in the "other" window.
;;; Having it wipe out the .hs file window is clearly the wrong
;;; behavior.

(defun haskell-get-pad ()
  "Creates a new scratch pad for the current module.
Signals an error if the current buffer is not a .hs file."
  (interactive)
  (let ((fname (buffer-file-name)))
    (if fname
	(do-get-pad fname (current-buffer))
        (haskell-mode-error "Not in a .hs buffer"))))


(defun do-get-pad (fname buff)
  (let* ((mname (or (get-modname buff)
		    (read-no-blanks-input "Scratch pad for module? " nil)))
	 (pname (lookup-pad mname fname))
	 (pbuff nil))
    ;; Generate the base name of the pad buffer, then create the
    ;; buffer.  The actual name of the pad buffer may be something
    ;; else because of name collisions.
    (if (or (not pname) (not (setq pbuff (get-buffer pname))))
	(progn
	  (setq pname (get-padname mname))
	  (setq pbuff (generate-new-buffer pname))
	  (setq pname (buffer-name pbuff))
	  (record-pad-mapping pname mname fname)
	  ))
    ;; Make sure the pad buffer is in haskell mode.
    (pop-to-buffer pbuff)
    (haskell-mode)))


;;; HASKELL-SWITCH
;;; ------------------------------------------------------------------

(defun haskell-switch ()
  "Switches to \*haskell\* buffer"
  (interactive)
  (haskell-maybe-create-process)
  (switch-to-haskell t))


(defun switch-to-haskell (eob-p)
  "Really switch to the \*haskell\* buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (pop-to-buffer *haskell-buffer*)
  (cond (eob-p
	 (push-mark)
	 (goto-char (point-max)))))


;;; HASKELL-COMMAND
;;; ------------------------------------------------------------------

(defun haskell-command (str)
  "Format STRING as a haskell command and send it to haskell process.  \\[haskell-command]"
  (interactive "sHaskell command: ")
  (if (eq ?Q (capitalize (aref str 0)))
      (ci-quit)
      (progn
	(haskell-begin-interaction
	    (concat "Executing command: :" str))
	(haskell-send-command (concat ":" str))
	(haskell-end-interaction
	    (concat "Executing command: :" str "  ...done.")))))


;;; HASKELL-EVAL and HASKELL-RUN
;;; ------------------------------------------------------------------

(defun haskell-eval ()
  "Evaluate expression in current module. \\[haskell-eval]"
  (interactive)
  (haskell-maybe-create-process)
  (haskell-eval-aux (get-haskell-expression "Haskell expression: ")
		    nil
		    "Evaluating"))

(defun haskell-run ()
  "Run Haskell Dialogue in current module"
  (interactive)
  (haskell-maybe-create-process)
  (haskell-eval-aux (get-haskell-expression "Haskell dialogue: ")
		    t
		    "Running"))

(defun haskell-run-main ()
  "Run Dialogue named main in current module"
  (interactive)
  (haskell-maybe-create-process)
  (haskell-eval-aux "main" t "Running"))

(defun haskell-eval-aux (exp dialogue-p what)
  (cond ((equal *haskell-buffer* (buffer-name))
	 (let* ((pname  *last-pad*)
		(mname  *last-module*)
		(fname  *last-loaded*)
		(msg    (format "%s: %s" what exp)))
	   (haskell-eval-aux-aux exp pname mname fname msg dialogue-p)))
	((equal *ht-temp-buffer* (buffer-name))
	 (let* ((fname  (buffer-file-name))
		(mname  (get-modname (current-buffer)))
		(pname  (lookup-pad mname fname))
		(msg    (format "%s (in tutorial): %s" what exp)))
	   (haskell-eval-aux-aux exp pname mname fname msg dialogue-p)))
	((buffer-file-name)
	 (let* ((fname  (buffer-file-name))
		(mname  (get-modname (current-buffer)))
		(pname  (lookup-pad mname fname))
		(msg    (format "%s (in file %s): %s"
				what (file-name-nondirectory fname) exp)))
	   (haskell-eval-aux-aux exp pname mname fname msg dialogue-p)))
	(t
	 (let* ((pname  (buffer-name (current-buffer)))
		(mname  (get-module-from-pad pname))
		(fname  (get-file-from-pad pname))
		(msg    (format "%s (in pad %s): %s" what pname exp)))
	   (haskell-eval-aux-aux exp pname mname fname msg dialogue-p)))
	))

(defun haskell-eval-aux-aux (exp pname mname fname msg dialogue-p)
  (haskell-begin-interaction msg)
  (ci-kill)
  (haskell-load-file-if-modified fname)
  (ci-module mname)
  (if pname (haskell-save-pad-if-modified pname))
  (if dialogue-p
      (ci-send-name exp)
      (ci-print-exp exp))
  (ci-eval)
  (haskell-end-interaction (concat msg "  ...done.")))


;;; Save pad only if modified.  Keep track of *last-pad* sent to process.

(defun haskell-save-pad-if-modified (pad)
  (save-excursion
    (set-buffer pad)
    (if (or (equal pad haskell-main-pad) (buffer-modified-p))
	(progn
	  (setq *last-pad* pad)
	  (ci-clear)
	  (ci-set-file pad)
	  (ci-send-buffer pad)
;	  (set-buffer-modified-p t)  ;***???
	  (ci-save)))))



;;; HASKELL-RUN-FILE
;;; ------------------------------------------------------------------

(defun haskell-run-file ()
  "Run all Dialogues in current file"
  (interactive)
  (haskell-maybe-create-process)
  (cond ((equal *haskell-buffer* (buffer-name))
	 ;; When called from the haskell process buffer, prompt for
	 ;; a file to run.
	 (call-interactively 'haskell-run-file/process))
	((buffer-file-name)
	 ;; When called from a .hs file buffer, run that file.
	 (haskell-run-file-aux (buffer-file-name)))
	(t
	 ;; When called from a pad, run the file that the module the
	 ;; pad belongs to lives in.
	 (haskell-run-file-aux
	     (get-file-from-pad (buffer-name (current-buffer)))))
	))

(defun haskell-run-file/process (filename)
  (interactive (comint-get-source "Haskell file to run:  "
				  haskell-prev-l/c-dir/file
				  haskell-source-modes t))
  (comint-check-source filename)
  (setq haskell-prev-l/c-dir/file
	(cons (file-name-directory filename)
	      (file-name-nondirectory filename)))
  (haskell-run-file-aux filename))

(defun haskell-run-file-aux (fname)
  (let ((msg  (concat "Running file: " fname)))
    (haskell-begin-interaction msg)
    (ci-kill)
    (save-modified-source-files buffer-file-name)
    (ci-run (strip-fext fname))
    (haskell-end-interaction (concat msg "  ...done."))))


;;; HASKELL-LOAD
;;; ------------------------------------------------------------------

(defun haskell-load ()
  "Load current file"
  (interactive)
  (haskell-maybe-create-process)
  (let* ((fname  (buffer-file-name))
	 (msg    (concat "Loading file: " fname)))
    (cond (fname
	   (haskell-begin-interaction msg)
	   (haskell-load-file-if-modified fname)
	   (haskell-end-interaction (concat msg "  ...done.")))
	  (t
	   (haskell-mode-error "Must be in a file to load")))))


;;; Load file only if modified or not *last-loaded*.
;;; For now, this just loads the file unconditionally.

(defun haskell-load-file-if-modified (filename)
  (save-modified-source-files buffer-file-name)
  (cond ((string= filename haskell-main-file)
	 (setq *last-loaded* haskell-main-file)
	 (ci-load-main))
	(t
	 (setq *last-loaded* filename)
	 (ci-load (strip-fext filename)))))


;;; ***This isn't used any more.
;(defun file-modification-time (file)
;  "Get modification time for FILE from filesystem information."
;  (car (cdr (car (nthcdr 5 (file-attributes file))))))


;;; HASKELL-COMPILE
;;; ------------------------------------------------------------------

(defun haskell-compile ()
  "Compile current file"
  (interactive)
  (haskell-maybe-create-process)
  (let ((fname  (buffer-file-name)))
    (cond (fname
	   (haskell-begin-interaction (concat "Compiling: " fname))
	   (haskell-compile-file-if-modified fname)
	   (haskell-end-interaction
	    (concat "Compiling: " fname "  ...done.")))
	  (t
	   (haskell-mode-error "Must be in a file to compile")))))

(defun haskell-compile-file-if-modified (fname)
  ;; *** For now it unconditionally compiles the file.
  (save-modified-source-files buffer-file-name)
  (ci-compile (strip-fext fname)))


;;; HASKELL-EXIT
;;; ------------------------------------------------------------------

(defun haskell-exit ()
  "Quit the haskell process"
  (interactive)
  (ci-quit)
  ;; If we were running the tutorial, mark the temp buffer as unmodified
  ;; so we don't get asked about saving it later.
  (if (and *ht-temp-buffer*
	   (get-buffer *ht-temp-buffer*))
      (save-excursion
	(set-buffer *ht-temp-buffer*)
	(set-buffer-modified-p nil)))
  ;; Try to remove the haskell output buffer from the screen.
  (bury-buffer *haskell-buffer*)
  (replace-buffer-in-windows *haskell-buffer*))


;;; HASKELL-INTERRUPT
;;; ------------------------------------------------------------------

(defun haskell-interrupt ()
  "Interrupt the haskell process"
  (interactive)
  ;; Do not queue the interrupt character; send it immediately.
  (haskell-send-command-aux "\C-c")       ; interrupt Haskell
  (haskell-end-interaction "done.")    ; send a reset to Lisp
  )


;;; HASKELL-EDIT-UNIT
;;; ------------------------------------------------------------------

(defun haskell-edit-unit ()
  "Edit the .hu file."
  (interactive)
  (let ((fname       (buffer-file-name)))
    (if fname
	(let ((find-file-not-found-hooks  (list 'haskell-new-unit))
	      (file-not-found             nil)
	      (units-fname                (haskell-get-unit-file)))
	  (find-file-other-window units-fname)
	  (if file-not-found
	      ;; *** this is broken.
	      (units-add-source-file
	         (if (string= (file-name-directory fname)
			      (file-name-directory units-fname))
		     (file-name-nondirectory fname)
		     fname))))
	(haskell-mode-error "Not in a .hs buffer"))))

(defun haskell-new-unit ()
  (setq file-not-found t))

(defun units-add-source-file (file)
  (save-excursion
    (insert (strip-fext file) "\n")))


;;; Look for a comment like "-- unit:" at top of file.
;;; If not found, assume unit file has same name as the buffer but
;;; a .hu extension.

(defun haskell-get-unit-file ()
  (let ((name  nil))
    (save-excursion
      (beginning-of-buffer)
      (if (re-search-forward "-- unit:[ \t]*" (point-max) t)
	  (let ((beg  (match-end 0)))
	    (end-of-line)
	    (setq name (buffer-substring beg (point))))
	  (setq name (concat (strip-fext (buffer-file-name)) ".hu"))))
    name))


;;; HASKELL-PLEASE-RECOVER
;;; ------------------------------------------------------------------

(defun haskell-please-recover ()
  (interactive)
  (haskell-flush-commands-and-reset)
  (haskell-end-interaction "done."))



;;; ==================================================================
;;; Support for printers/optimizers menus
;;; ==================================================================

;;; This code was adapted from the standard buff-menu.el code.

(defvar haskell-menu-mode-map nil "")

(if (not haskell-menu-mode-map)
    (progn
      (setq haskell-menu-mode-map (make-keymap))
      (suppress-keymap haskell-menu-mode-map t)
      (define-key haskell-menu-mode-map "m" 'haskell-menu-mark)
      (define-key haskell-menu-mode-map "u" 'haskell-menu-unmark)
      (define-key haskell-menu-mode-map "x" 'haskell-menu-exit)
      (define-key haskell-menu-mode-map "q" 'haskell-menu-exit)
      (define-key haskell-menu-mode-map " " 'next-line)
      (define-key haskell-menu-mode-map "\177" 'haskell-menu-backup-unmark)
      (define-key haskell-menu-mode-map "?" 'describe-mode)))

;; Printers Menu mode is suitable only for specially formatted data.

(put 'haskell-menu-mode 'mode-class 'special)

(defun haskell-menu-mode ()
  "Major mode for editing Haskell flags.
Each line describes a flag.
Letters do not insert themselves; instead, they are commands.
m -- mark flag (turn it on)
u -- unmark flag (turn it off)
x -- exit; tell the Haskell process to update the flags, then leave menu.
q -- exit; same as x.
Precisely,\\{haskell-menu-mode-map}"
  (kill-all-local-variables)
  (use-local-map haskell-menu-mode-map)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq major-mode 'haskell-menu-mode)
  (setq mode-name "Haskell Flags Menu")
  ;; These are all initialized elsewhere
  (make-local-variable 'haskell-menu-current-flags)
  (make-local-variable 'haskell-menu-request-fn)
  (make-local-variable 'haskell-menu-update-fn)
  (run-hooks 'haskell-menu-mode-hook))


(defun haskell-menu (help-file buffer request-fn update-fn)
  (haskell-maybe-create-process)
  (if (get-buffer buffer)
      (progn
	(pop-to-buffer buffer)
	(goto-char (point-min)))
      (progn
        (pop-to-buffer buffer)
	(insert-file-contents help-file)
	(haskell-menu-mode)
	(setq haskell-menu-request-fn request-fn)
	(setq haskell-menu-update-fn update-fn)
	))
  (haskell-menu-mark-current)
  (message "m = mark; u = unmark; x = execute; q = quit; ? = more help."))



;;; A line that starts with *haskell-menu-marked* is a menu item turned on.
;;; A line that starts with *haskell-menu-unmarked* is turned off.
;;; A line that starts with anything else is just random text and is
;;; ignored by commands that deal with menu items.

(defvar *haskell-menu-marked*   " on")
(defvar *haskell-menu-unmarked* "   ")
(defvar *haskell-menu-marked-regexp*   " on   \\w")
(defvar *haskell-menu-unmarked-regexp* "      \\w")

(defun haskell-menu-mark ()
  "Mark flag to be turned on."
  (interactive)
  (beginning-of-line)
  (cond ((looking-at *haskell-menu-marked-regexp*)
	 (forward-line 1))
	((looking-at *haskell-menu-unmarked-regexp*)
	 (let ((buffer-read-only  nil))
	   (delete-char (length *haskell-menu-unmarked*))
	   (insert *haskell-menu-marked*)
	   (forward-line 1)))
	(t
	 (forward-line 1))))

(defun haskell-menu-unmark ()
  "Unmark flag."
  (interactive)
  (beginning-of-line)
  (cond ((looking-at *haskell-menu-unmarked-regexp*)
	 (forward-line 1))
	((looking-at *haskell-menu-marked-regexp*)
	 (let ((buffer-read-only  nil))
	   (delete-char (length *haskell-menu-marked*))
	   (insert *haskell-menu-unmarked*)
	   (forward-line 1)))
	(t
	 (forward-line 1))))

(defun haskell-menu-backup-unmark ()
  "Move up and unmark."
  (interactive)
  (forward-line -1)
  (haskell-menu-unmark)
  (forward-line -1))


;;; Actually make the changes.

(defun haskell-menu-exit ()
  "Update flags, then leave menu."
  (interactive)
  (haskell-menu-execute)
  (haskell-menu-quit))

(defun haskell-menu-execute ()
  "Tell haskell process to tweak flags."
  (interactive)
  (start-setting-flags)
  (save-excursion
    (goto-char (point-min))
    (while (not (eq (point) (point-max)))
      (cond ((looking-at *haskell-menu-unmarked-regexp*)
	     (funcall haskell-menu-update-fn (haskell-menu-flag) nil))
	    ((looking-at *haskell-menu-marked-regexp*)
	     (funcall haskell-menu-update-fn (haskell-menu-flag) t))
	    (t
	     nil))
      (forward-line 1)))
  (finish-setting-flags))

(defun haskell-menu-quit ()
  (interactive)
  "Make the menu go away."
  (bury-buffer (current-buffer))
  (replace-buffer-in-windows (current-buffer)))


(defun haskell-menu-flag ()
  (save-excursion
    (beginning-of-line)
    (forward-char 6)
    (let ((beg  (point)))
      ;; End of flag name marked by tab or two spaces.
      (re-search-forward "\t\\|  ")
      (buffer-substring beg (match-beginning 0)))))


(defun start-setting-flags ()
  nil)

(defun finish-setting-flags ()
  (haskell-end-interaction "Setting flags....done."))


;;; Update the menu to mark only those items currently turned on.

(defun haskell-menu-mark-current ()
  (funcall haskell-menu-request-fn)
  (save-excursion
    (goto-char (point-min))
    (while (not (eq (point) (point-max)))
      (cond ((and (looking-at *haskell-menu-unmarked-regexp*)
		  (menu-item-currently-on-p (haskell-menu-flag)))
	     (haskell-menu-mark))
	    ((and (looking-at *haskell-menu-marked-regexp*)
		  (not (menu-item-currently-on-p (haskell-menu-flag))))
	     (haskell-menu-unmark))
	    (t
	     (forward-line 1))))))


;;; See if a menu item is turned on.

(defun menu-item-currently-on-p (item)
  (member-string= item haskell-menu-current-flags))

(defun member-string= (item list)
  (cond ((null list)
	 nil)
	((string= item (car list))
	 list)
	(t
	 (member-string= item (cdr list)))))



;;; Make the menu for printers.

(defvar *haskell-printers-help*
  (concat (getenv "HASKELL") "/emacs-tools/printer-help.txt")
  "Help file for printers.")

(defvar *haskell-printers-buffer* "*Haskell printers*")

(defun haskell-printers ()
  "Set printers interactively."
  (interactive)
  (haskell-menu
    *haskell-printers-help*
    *haskell-printers-buffer*
    'get-current-printers
    'set-current-printers))
		
(defun get-current-printers ()
  (setq haskell-menu-current-flags t)
  (haskell-send-command ":p?")
  (while (eq haskell-menu-current-flags t)
    (sleep-for 1)))

(defun update-printers-list (data)
  (setq haskell-menu-current-flags (read data)))

(defun set-current-printers (flag on)
  (let ((was-on (menu-item-currently-on-p flag)))
    (cond ((and on (not was-on))
	   (haskell-send-command (format ":p+ %s" flag)))
	  ((and (not on) was-on)
	   (haskell-send-command (format ":p- %s" flag)))
	  (t
	   nil))))


;;; Equivalent stuff for the optimizers menu

(defvar *haskell-optimizers-help*
  (concat (getenv "HASKELL") "/emacs-tools/optimizer-help.txt")
  "Help file for optimizers.")

(defvar *haskell-optimizers-buffer* "*Haskell optimizers*")

(defun haskell-optimizers ()
  "Set optimizers interactively."
  (interactive)
  (haskell-menu
    *haskell-optimizers-help*
    *haskell-optimizers-buffer*
    'get-current-optimizers
    'set-current-optimizers))
		
(defun get-current-optimizers ()
  (setq haskell-menu-current-flags t)
  (haskell-send-command ":o?")
  (while (eq haskell-menu-current-flags t)
    (sleep-for 1)))

(defun update-optimizers-list (data)
  (setq haskell-menu-current-flags (read data)))

(defun set-current-optimizers (flag on)
  (let ((was-on (menu-item-currently-on-p flag)))
    (cond ((and on (not was-on))
	   (haskell-send-command (format ":o+ %s" flag)))
	  ((and (not on) was-on)
	   (haskell-send-command (format ":o- %s" flag)))
	  (t
	   nil))))




;;; ==================================================================
;;; Random utilities
;;; ==================================================================


;;; Keep track of the association between pads, modules, and files.
;;; The global variable is a list of (pad-buffer-name module-name file-name)
;;; lists.

(defvar *pad-mappings* ()
  "Associates pads with their corresponding module and file.")

(defun record-pad-mapping (pname mname fname)
  (setq *pad-mappings*
	(cons (list pname mname fname) *pad-mappings*)))

(defun get-module-from-pad (pname)
  (car (cdr (assoc pname *pad-mappings*))))

(defun get-file-from-pad (pname)
  (car (cdr (cdr (assoc pname *pad-mappings*)))))

(defun lookup-pad (mname fname)
  (lookup-pad-aux mname fname *pad-mappings*))

(defun lookup-pad-aux (mname fname list)
  (cond ((null list)
	 nil)
	((and (equal mname (car (cdr (car list))))
	      (equal fname (car (cdr (cdr (car list))))))
	 (car (car list)))
	(t
	 (lookup-pad-aux mname fname (cdr list)))))



;;; Save any modified .hs and .hu files.
;;; Yes, the two set-buffer calls really seem to be necessary.  It seems
;;; that y-or-n-p makes emacs forget we had temporarily selected some
;;; other buffer, and if you just do save-buffer directly it will end
;;; up trying to save the current buffer instead.  The built-in
;;; save-some-buffers function has this problem....

(defvar *ask-before-saving* t)

(defun save-modified-source-files (filename)
  (let ((buffers   (buffer-list))
	(found-any nil))
    (while buffers
      (let ((buffer  (car buffers)))
	(if (and (buffer-modified-p buffer)
		 (save-excursion
		   (set-buffer buffer)
		   (and buffer-file-name
			(source-file-p buffer-file-name)
			(setq found-any t)
			(or (null *ask-before-saving*)
			    (string= buffer-file-name filename)
			    (y-or-n-p
			        (format "Save file %s? " buffer-file-name))))))
	    (save-excursion
	      (set-buffer buffer)
	      (save-buffer))))
      (setq buffers (cdr buffers)))
    (if found-any
	(message "")
        (message "(No files need saving)"))))
  
(defun source-file-p (filename)
  (or (string-match "\\.hs$" filename)
      (string-match "\\.lhs$" filename)
      (string-match "\\.hu$" filename)
      (string-match "\\.shu$" filename)
      (string-match "\\.hsp$" filename)
      (string-match "\\.prim$" filename)))


;;; Buffer utilities

(defun haskell-move-marker ()
  "Moves the marker and point to the end of buffer"
  (set-marker comint-last-input-end (point-max))
  (set-marker (process-mark (get-process "haskell")) (point-max))
  (goto-char (point-max)))
  

;;; Pad utils

(defun create-main-pad ()
  (let ((buffer (get-buffer-create haskell-main-pad)))
    (save-excursion
      (set-buffer buffer)
      (haskell-mode))
    (record-pad-mapping haskell-main-pad haskell-main-module haskell-main-file)
    buffer))

	
;;; Extract the name of the module the point is in, from the given buffer.

(defvar *re-module* "^module\\s *\\|^>\\s *module\\s *")
(defvar *re-modname* "[A-Z]\\([a-z]\\|[A-Z]\\|[0-9]\\|'\\|_\\)*")

(defun get-modname (buff)
  "Get module name in BUFFER that point is in."
  (save-excursion
    (set-buffer buff)
    (if (or (looking-at *re-module*)
	    (re-search-backward *re-module* (point-min) t)
	    (re-search-forward *re-module* (point-max) t))
	(progn
	  (goto-char (match-end 0))
	  (if (looking-at *re-modname*)
	      (buffer-substring (match-beginning 0) (match-end 0))
	      (haskell-mode-error "Module name not found!!")))
	"Main")))


;;; Build the base name for a pad buffer.

(defun get-padname (m)
  "Build padname from module name"
  (concat "*" m "-pad*"))


;;; Strip file extensions.
;;; Only strip off extensions we know about; e.g.
;;; "foo.hs" -> "foo" but "foo.bar" -> "foo.bar".

(defvar *haskell-filename-regexp* "\\(.*\\)\\.\\(hs\\|lhs\\)$")

(defun strip-fext (filename)
  "Strip off the extension from a filename."
  (if (string-match *haskell-filename-regexp* filename)
      (substring filename (match-beginning 1) (match-end 1))
      filename))


;;; Haskell mode error

(defun haskell-mode-error (msg)
  "Show MSG in message line as an error from the haskell mode"
  (error (concat "Haskell mode:  " msg)))




;;; ==================================================================
;;; Command generators
;;; ==================================================================

;;; Generate Haskell command interface commands.  These are very simple
;;; routines building the string commands to be sent to the haskell
;;; process.

(defun ci-send-buffer (buff)
  "Send BUFFER to haskell process."
  (let ((str (buffer-string)))
    (if (not (string-match "\\`\\s *\\'" str))  ; ignore if all whitespace
	(save-excursion
	  (set-buffer buff)
	  (haskell-send-command str)))))

(defun ci-kill ()
  (haskell-send-command ":kill"))

(defun ci-clear ()
  (haskell-send-command ":clear"))

(defun ci-set-file (file-name)
  (haskell-send-command (concat ":file " file-name)))

(defun ci-module (modname)
  (setq *last-module* modname)
  (haskell-send-command (concat ":module " modname)))


;;; Keeps track of the last file loaded.
;;; Change to do a :compile (temporary until new csys)
;;;  2-Aug-91 Dan Rabin.

(defun ci-load (filename)
  (haskell-send-command (concat ":load " filename)))

(defun ci-load-main ()
  (haskell-send-command ":Main"))

(defun ci-save ()
  (haskell-send-command ":save"))

(defun ci-compile (filename)
  (haskell-send-command (concat ":compile " filename)))

(defun ci-run (filename)
  (haskell-send-command (concat ":run " filename)))

(defun ci-print-exp (exp)
  (ci-set-file "interactive-expression-buffer")
  (haskell-send-command (concat "= " exp)))

(defun ci-send-name (name)
  (let ((temp  (make-temp-name "etemp")))
    (ci-set-file "interactive-expression-buffer")
    (haskell-send-command (concat temp " = " name))))

(defun ci-eval ()
  (haskell-send-command ":eval"))

(defun ci-quit ()
  (cond ((not (get-buffer-process *haskell-buffer*))
	 (message "No process currently running."))
	((y-or-n-p "Do you really want to quit Haskell? ")
	 (process-send-string "haskell" ":quit\n")
	 (set-haskell-status 'dead))
	(t
	 nil)))


;;; When setting emacs mode (on/off)
;;;     (a) Set process-filter
;;;     (b) Send :Emacs command to Haskell process

(defun ci-emacs ()
  (haskell-reset)
  (set-process-filter (get-process "haskell") 'process-haskell-output)
  (haskell-send-command ":Emacs on"))






;;; ==================================================================
;;; Handle input in haskell process buffer; history commands.
;;; ==================================================================

(defun haskell-get-old-input ()
  "Get old input text from Haskell process buffer."
  (save-excursion
    (if (re-search-forward haskell-prompt-pattern (point-max) 'move)
	(goto-char (match-beginning 0)))
    (cond ((re-search-backward haskell-prompt-pattern (point-min) t)
	   (comint-skip-prompt)
	   (let ((temp  (point)))
	     (end-of-line)
	     (buffer-substring temp (point)))))))


;;; Modified for Haskell (taken from comint-send-input)

(defun haskell-send-input ()
  "Send input to Haskell while in the process buffer"
  (interactive)
  (if *emacs*
      (haskell-send-input-aux)
      (comint-send-input)))

(defun haskell-send-input-aux ()
  ;; Note that the input string does not include its terminal newline.
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc)
	(haskell-mode-error "Current buffer has no process")
	(let* ((pmark (process-mark proc))
	       (pmark-val (marker-position pmark))
	       (input (if (>= (point) pmark-val)
			  (buffer-substring pmark (point))
			  (let ((copy (funcall comint-get-old-input)))
			    (goto-char pmark)
			    (insert copy)
			    copy))))
	  (insert ?\n)
	  (if (funcall comint-input-filter input)
	      (ring-insert input-ring input))
	  (funcall comint-input-sentinel input)
	  (set-marker (process-mark proc) (point))
	  (set-marker comint-last-input-end (point))
	  (haskell-send-data input)))))



;;; ==================================================================
;;; Minibuffer input stuff
;;; ==================================================================

;;; Haskell input history retrieval commands   (taken from comint.el)
;;; M-p -- previous input    M-n -- next input

(defvar haskell-minibuffer-local-map nil
  "Local map for minibuffer when in Haskell")

(if haskell-minibuffer-local-map
    nil
    (progn
      (setq haskell-minibuffer-local-map
	    (full-copy-sparse-keymap minibuffer-local-map))
      ;; Haskell commands
      (define-key haskell-minibuffer-local-map "\ep"   'haskell-previous-input)
      (define-key haskell-minibuffer-local-map "\en"   'haskell-next-input)
      ))

(defun haskell-previous-input (arg)
  "Cycle backwards through input history."
  (interactive "*p")
  (let ((len (ring-length haskell-prompt-ring)))
    (cond ((<= len 0)
	   (message "Empty input ring")
	   (ding))
	  (t
	   (cond ((eq last-command 'haskell-previous-input)
		  (delete-region (mark) (point))
		  (set-mark (point)))
		 (t                          
		  (setq input-ring-index
			(if (> arg 0) -1
			    (if (< arg 0) 1 0)))
		  (push-mark (point))))
	   (setq input-ring-index (comint-mod (+ input-ring-index arg) len))
	   (insert (ring-ref haskell-prompt-ring input-ring-index))
	   (setq this-command 'haskell-previous-input))
	  (t (ding)))))
	 
(defun haskell-next-input (arg)
  "Cycle forwards through input history."
  (interactive "*p")
  (haskell-previous-input (- arg)))

(defvar haskell-last-input-match ""
  "Last string searched for by Haskell input history search, for defaulting.
Buffer local variable.") 

(defun haskell-previous-input-matching (str)
  "Searches backwards through input history for substring match"
  (interactive (let ((s (read-from-minibuffer 
			 (format "Command substring (default %s): "
				 haskell-last-input-match))))
		 (list (if (string= s "") haskell-last-input-match s))))
  (setq haskell-last-input-match str) ; update default
  (let ((str (regexp-quote str))
        (len (ring-length haskell-prompt-ring))
	(n 0))
    (while (and (<= n len)
		(not (string-match str (ring-ref haskell-prompt-ring n))))
      (setq n (+ n 1)))
    (cond ((<= n len) (haskell-previous-input (+ n 1)))
	  (t (haskell-mode-error "Not found.")))))


;;; Actually read an expression from the minibuffer using the new keymap.

(defun get-haskell-expression (prompt)
  (let ((exp  (read-from-minibuffer prompt nil haskell-minibuffer-local-map)))
    (ring-insert haskell-prompt-ring exp)
    exp))




;;; ==================================================================
;;; User customization
;;; ==================================================================

(defvar haskell-load-hook nil
  "This hook is run when haskell is loaded in.
This is a good place to put key bindings."
  )
	
(run-hooks 'haskell-load-hook)




;;;======================================================================
;;; Tutorial mode setup
;;;======================================================================

;;; Set up additional key bindings for tutorial mode.

(defvar ht-mode-map nil)

(if ht-mode-map
    nil
    (progn
      (setq ht-mode-map (make-sparse-keymap))
      (haskell-establish-key-bindings ht-mode-map)
      (define-key ht-mode-map "\C-c\C-f" 'ht-next-page)
      (define-key ht-mode-map "\C-c\C-b" 'ht-prev-page)
      (define-key ht-mode-map "\C-c\C-l" 'ht-restore-page)
      (define-key ht-mode-map "\C-c?"    'describe-mode)))

(defun haskell-tutorial-mode ()
  "Major mode for running the Haskell tutorial.  
You can use these commands:
\\{ht-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map ht-mode-map)
  (setq major-mode 'haskell-tutorial-mode)
  (setq mode-name "Haskell Tutorial")
  (set-syntax-table haskell-mode-syntax-table)
  (run-hooks 'haskell-mode-hook))


(defun haskell-tutorial ()
  "Run the haskell tutorial."
  (interactive)
  (ht-load-tutorial)
  (ht-make-buffer)
  (ht-display-page))


;;; Load the tutorial file into a read-only buffer.  Do not display this
;;; buffer.

(defun ht-load-tutorial ()
  (let ((buffer  (get-buffer *ht-file-buffer*)))
    (if buffer
	(save-excursion
	  (set-buffer buffer)
	  (beginning-of-buffer))
	(save-excursion
	  (set-buffer (setq buffer (get-buffer-create *ht-file-buffer*)))
	  (let ((fname (substitute-in-file-name *ht-source-file*)))
	    (if (file-readable-p fname)
		(ht-load-tutorial-aux fname)
		(call-interactively 'ht-load-tutorial-aux)))))))

(defun ht-load-tutorial-aux (filename)
  (interactive "fTutorial file: ")
  (insert-file filename)
  (set-buffer-modified-p nil)
  (setq buffer-read-only t)
  (beginning-of-buffer))


;;; Create a buffer to use for messing about with each page of the tutorial.
;;; Put the buffer into haskell-tutorial-mode.

(defun ht-make-buffer ()
  (find-file (concat "/tmp/" (make-temp-name "ht") ".hs"))
  (setq *ht-temp-buffer* (buffer-name))
  (haskell-tutorial-mode))


;;; Commands for loading text into the tutorial pad buffer

(defun ht-next-page ()
  "Go to the next tutorial page."
  (interactive)
  (if (ht-goto-next-page)
      (ht-display-page)
      (beep)))

(defun ht-goto-next-page ()
  (let ((buff  (current-buffer)))
    (unwind-protect
	(progn
	  (set-buffer *ht-file-buffer*)
	  (search-forward "\C-l" nil t))
      (set-buffer buff))))

(defun ht-prev-page ()
  "Go to the previous tutorial page."
  (interactive)
  (if (ht-goto-prev-page)
      (ht-display-page)
      (beep)))

(defun ht-goto-prev-page ()
  (let ((buff  (current-buffer)))
    (unwind-protect
	(progn
	  (set-buffer *ht-file-buffer*)
	  (search-backward "\C-l" nil t))
      (set-buffer buff))))

(defun ht-goto-page (arg)
  "Go to the tutorial page specified as the argument."
  (interactive "sGo to page: ")
  (if (ht-searchfor-page (format "-- Page %s " arg))
      (ht-display-page)
      (beep)))

(defun ht-goto-section (arg)
  "Go to the tutorial section specified as the argument."
  (interactive "sGo to section: ")
  (if (ht-searchfor-page (format "-- Section %s " arg))
      (ht-display-page)
      (beep)))

(defun ht-searchfor-page (search-string)
  (let ((buff           (current-buffer)))
    (unwind-protect
	(progn
	  (set-buffer *ht-file-buffer*)
	  (let ((point  (point)))
	    (beginning-of-buffer)
	    (if (search-forward search-string nil t)
		t
		(progn
		  (goto-char point)
		  nil))))
      (set-buffer buff))))

(defun ht-restore-page ()
  (interactive)
  (let ((old-point  (point)))
    (ht-display-page)
    (goto-char old-point)))

(defun ht-display-page ()
  (set-buffer *ht-file-buffer*)
  (let* ((beg   (progn
		 (if (search-backward "\C-l" nil t)
		     (forward-line 1)
		     (beginning-of-buffer))
		 (point)))
	 (end   (progn
		  (if (search-forward "\C-l" nil t)
		      (beginning-of-line)
		      (end-of-buffer))
		  (point)))
	 (text  (buffer-substring beg end)))
    (set-buffer *ht-temp-buffer*)
    (erase-buffer)
    (insert text)
    (beginning-of-buffer)))
