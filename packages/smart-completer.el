

;;; smart-completer.el --- TODO description

;; TODO license here

;;; Commentary:

;; TODO

;;; Code:

;;
;; Dependencies
;;

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

;;
;; Major mode definition
;;

;;
;; Global methods
;;

(defun smart-completer-process-filter (process output)
	(message output))

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

(defun smart-completer-send-string (string)
	"TODO"
	(when smart-completer-process
		(process-send-string smart-completer-process string)
		(accept-process-output smart-completer-process 1)))

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
;; alert.el integration
;; 

;;
;; Setup
;; 


(provide 'smart-completer)
;;; smart-completer.el ends here
