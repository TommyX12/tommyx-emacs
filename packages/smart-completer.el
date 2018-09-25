

;;; smart-completer.el --- TODO description

;; TODO license here

;;; Commentary:

;; TODO

;;; Code:

;;
;; Dependencies
;;

(require 'cl-lib)
(require 's)

;;
;; Constants
;;

;;
;; Macros
;;

;;
;; Customization
;;

(defgroup smart-completer nil
  "Options for smart-completer."
  :prefix "smart-completer-")

;;
;; Faces
;;

;;
;; Variables
;;

(defvar smart-completer-executable-path
	(expand-file-name
	 (if (eq system-type 'windows-nt)
		"smart-completer/smart-completer.exe"
		"smart-completer/smart-completer")
	 (file-name-directory load-file-name)))

(defvar smart-completer-process nil)

(defvar smart-completer-process-return nil)

(defvar smart-completer-async-callback nil)
(defvar smart-completer-async-arg nil)

;;
;; Major mode definition
;;

;;
;; Global methods
;;

(defun smart-completer-enable ()
	"Enable smart-completer."
	(smart-completer-disable)
	(let ((process-connection-type nil))
		(setq smart-completer-process
					(start-process "smart-completer-process" nil smart-completer-executable-path))
		(set-process-filter smart-completer-process
												'smart-completer-process-filter)
	))

(defun smart-completer-disable ()
	"Disable smart-completer."
	(when smart-completer-process
		(delete-process smart-completer-process)
		(setq smart-completer-process nil)))

;; sync
;; (defun smart-completer-query (prefix)
;; 	"TODO"
;; 	(when smart-completer-process
;; 		(setq smart-completer-process-return nil)
;; 		(process-send-string smart-completer-process (concat prefix "\n"))
;; 		(accept-process-output smart-completer-process 1)
;; 		smart-completer-process-return))

(defun smart-completer-query (prefix)
	"TODO"
	(when smart-completer-process
		(setq smart-completer-process-return nil)
		(process-send-string smart-completer-process (concat prefix "\n"))))

;; sync
;; (defun smart-completer-process-filter (process output)
;; 	(setq smart-completer-process-return output))

(defun smart-completer-process-filter (process output)
	(when smart-completer-async-callback
		(funcall smart-completer-async-callback (s-split "\t" output t))
		(setq smart-completer-async-callback nil)))

(defun company-smart-completer (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-smart-completer))
    (prefix (company-grab-symbol))
    (candidates
			(setq smart-completer-async-arg arg)
			'(:async . (lambda (callback)
				(setq smart-completer-async-callback callback)
				(smart-completer-query smart-completer-async-arg)
			)))
		;; sync
    ;; (candidates (when-let ((response (smart-completer-query arg)))
		;; 	(s-split "\t" response t)))
    (meta (format "This value is named %s" arg))
		;; (no-cache t)
		(sorted t)
		))

;;
;; Interactive functions
;;

;;
;; Advices
;;

;;
;; Hooks
;;

;;
;; Segments definition
;;

;;
;; Setup
;; 


(provide 'smart-completer)
;;; smart-completer.el ends here
