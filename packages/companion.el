
;;; companion.el --- TODO description

;; TODO license here

;;; Commentary:

;; TODO

;;; Code:

;;
;; Constants
;;

(defconst companion-buffer-name " *Companion*"
  "Name of the buffer where companion shows directory contents.")

;;
;; Macros
;;

(defmacro companion--with-buffer (&rest body)
  "Execute the forms in BODY with global Companion buffer."
  (declare (indent 0) (debug t))
  `(let ((companion-buffer (companion--get-buffer)))
     (unless (null companion-buffer)
       (with-current-buffer companion-buffer
         ,@body))))

(defmacro companion--with-editing-buffer (&rest body)
  "Execute BODY in companion buffer without read-only restriction."
  `(let (rlt)
     (companion--with-buffer
       (setq buffer-read-only nil)
       (setq rlt (progn ,@body))
       (setq buffer-read-only t))
     rlt))

(defmacro companion--with-window (&rest body)
  "Execute the forms in BODY with global Companion window."
  (declare (indent 0) (debug t))
  `(save-selected-window
     (companion--select-window)
     ,@body))

(defmacro companion--with-resizable-window (&rest body)
  "Execute BODY in companion window without `window-size-fixed' restriction."
  `(let (rlt)
     (companion--with-buffer
       (companion-buffer--unlock-height))
     (setq rlt (progn ,@body))
     (companion--with-buffer
       (companion-buffer--lock-height))
     rlt))

;;
;; Customization
;;

(defgroup companion nil
  "Options for companion."
  :prefix "companion-")

(defcustom companion-window-height 1
  "*Specifies the height of the Companion window."
  :type 'integer
  :group 'companion)

(defcustom companion-display-action '(companion-default-display-fn)
  "*Action to use for displaying Companion window."
  :type 'sexp
  :group 'companion)

;;
;; Faces
;;

(defface companion-face
  '((t (:inherit header-line)))
  "*Face used for the banner in companion buffer."
  :group 'companion :group 'font-lock-highlighting-faces)
(defvar companion-face 'companion-face)

;;
;; Variables
;;

(defvar companion--buffer nil)

(defvar companion--window nil)

;;
;; Major mode definition
;;

(define-derived-mode companion-mode special-mode "Companion"
  "The major mode for companion."
  (setq indent-tabs-mode nil
        buffer-read-only t
        truncate-lines -1))

;;
;; Global methods
;;

(defun companion--select-window ()
  "Select the Companion window."
  (interactive)
  (let ((window (companion--get-window)))
    (select-window window)))

(defun companion--window-exists-p ()
  "Return non-nil if companion window exists."
  (and (not (null (window-buffer companion--window)))
       (eql (window-buffer companion--window) (companion--get-buffer))))

(defun companion--get-window ()
  "Return the companion window if it exists, else return nil.
But when the companion window does not exist, it will create the companion window and return it."
  (unless (companion--window-exists-p)
    (setf companion--window nil))
  (when (null companion--window)
    (setq companion--window
          (companion--create-window)))
  companion--window)

(defun companion-buffer--create ()
  "Create and switch to Companion buffer."
  (switch-to-buffer
   (generate-new-buffer-name companion-buffer-name))
  (companion-mode)
  ;; disable linum-mode
  (when (and (boundp 'linum-mode)
             (not (null linum-mode)))
    (linum-mode -1))
	;; remove header line and mode line
	(setq-local header-line-format nil)
	(setq-local mode-line-format nil)
	(setq-local cursor-type nil)
	(setq-local cursor-in-non-selected-windows nil)
	(setq-local word-wrap nil)
  (current-buffer))

(defun companion-default-display-fn (buffer _alist)
  "Display BUFFER at the top of the root window.
The root window is the root window of the selected frame.
_ALIST is ignored."
	(display-buffer-in-side-window buffer '((side . top))))

(defun companion--get-buffer ()
  "Return the global companion buffer if it exists.
If global companion buffer not exists, create it."
  (unless (equal (buffer-name companion--buffer)
                 companion-buffer-name)
    (setq companion--buffer nil))
  (when (null companion--buffer)
    (save-window-excursion
      (setq companion--buffer
            (companion-buffer--create))))
  companion--buffer)

(defun companion-window--init (window buffer)
  "Make WINDOW a Companion window.
Companion buffer is BUFFER."
  (companion--with-resizable-window
	(switch-to-buffer buffer)
	(setq-local face-remapping-alist '((default companion-face)))
	(set-window-parameter window 'no-delete-other-windows t)
	(set-window-parameter window 'no-other-window t)
	(set-window-margins window 0 0)
	(set-window-fringes window 0 0)
	(set-window-dedicated-p window t)
	(set-window-scroll-bars window 0 nil 0 nil)
	(setq-local window-min-height 1)
	(setq-local window-safe-min-height 1)
	(let (window-size-fixed)
		(fit-window-to-buffer window 1))
	(setq-local window-size-fixed t)
	(window-preserve-size window nil t)
	(when (fboundp 'window-preserve-size)
		(window-preserve-size window nil t))
	)
	window)

(defun companion--attach ()
  "Attach the global companion buffer"
  (setq companion--buffer (get-buffer companion-buffer-name))
  (setq companion--window (get-buffer-window
                            companion--buffer))
	 ;; disallow winum to select companion buffer
	(when (and
				 (boundp 'winum-ignored-buffers)
				 (not (member companion-buffer-name winum-ignored-buffers)))
		(add-to-list 'winum-ignored-buffers companion-buffer-name))
  (companion--with-buffer
    (companion-buffer--lock-height)))

(defun companion--render ()
	(companion--with-editing-buffer
		(erase-buffer)
		(insert "test")
	))

(defun companion--create-window ()
  "Create global companion window."
  (let ((window nil)
        (buffer (companion--get-buffer)))
    (setq window
          (select-window
           (display-buffer buffer companion-display-action)))
    (companion-window--init window buffer)
    (companion--attach)
    (companion--reset-height)
    window))

(defun companion--set-window-height (height)
  "Set companiontree window height to HEIGHT."
  (companion--with-window
    (companion--with-resizable-window
     (companion-util--set-window-height height))))

(defun companion--reset-height ()
  "Set companion window height to `companion-window-height'."
  (companion--set-window-height companion-window-height))

(defun companion-buffer--lock-height ()
  "Lock the height for Companion window."
	(setq window-size-fixed 'height))

(defun companion-buffer--unlock-height ()
  "Unlock the height for Companion window."
  (setq window-size-fixed nil))

;;
;; Advices
;;

;; (defadvice save-some-buffers
;; 		(after companion-save-buffer-fix activate)
;; 	"Reset companion size."
;; 	(when (companion--window-exists-p)
;; 		(companion-reopen)))

;;
;; Hooks
;;

;;
;; Util methods
;;

(defun companion-util--set-window-height (n)
  "Make selected window N row height."
	(let ((window-safe-min-height 0))
		(window-resize (selected-window) (- n (window-height)) nil 'safe)))

;;
;; Buffer methods
;;

;;
;; Mode-line methods
;;

;;
;; Window methods
;;

;;
;; Interactive functions
;;

(defun companion-close ()
  "Close the Companion window."
  (interactive)
  (if (companion--window-exists-p)
			(kill-buffer companion--buffer)
      (delete-window companion--window)))

(defun companion-open ()
  "Open the Companion window."
  (interactive)
	(let ((cw (selected-window)))
	(companion--get-window)
	(companion--render)
	(select-window cw)))

(defun companion-reopen ()
  "Close then open the Companion window again."
  (interactive)
	(when (companion--window-exists-p)
		(companion-close)
		(companion-open)))

(provide 'companion)
;;; companion.el ends here
