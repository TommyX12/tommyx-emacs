;;; smart-completer.el --- TODO description

;; TODO license here

;;; Commentary:

;; TODO

;;; Code:

;;
;; Dependencies
;;

(require 'cl-lib)
(require 'json)
(require 'unicode-escape)
(require 's)

;;
;; Constants
;;

(defconst smart-completer--process-name "smart-completer-process")
(defconst smart-completer--buffer-name "*smart-completer-log*")

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

;; (defvar smart-completer-executable-command
;; 	(expand-file-name
;; 	 (if (eq system-type 'windows-nt)
;; 		"smart-completer/smart-completer.exe"
;; 		"smart-completer/smart-completer")
;; 	 (file-name-directory load-file-name)))

(defvar smart-completer-executable-command "python")

(defvar smart-completer-executable-args
	(list (expand-file-name
				 "smart-completer/smart_completer.py"
				 (file-name-directory load-file-name))))

(defvar smart-completer-process nil)
(defvar smart-completer-log-process nil)

(defvar smart-completer-process-return nil)
(defvar smart-completer-process-processing nil)

(defvar smart-completer-async-callback nil)
(defvar smart-completer-async-prefix nil)

;;
;; Major mode definition
;;

;;
;; Global methods
;;

(defun smart-completer--get-log-buffer ()
	(if-let ((buf (get-buffer smart-completer--buffer-name)))
			buf
		(let ((buf (get-buffer-create smart-completer--buffer-name)))
			(with-current-buffer buf
				(setq buffer-read-only t)
				(let ((inhibit-read-only t))
					(buffer-disable-undo)
					(erase-buffer)))
			buf)))

(defun smart-completer-enable ()
	"Enable smart-completer."
	(smart-completer-disable)
	(let ((process-connection-type nil))
		(setq smart-completer-log-process
					(make-pipe-process
					 :name smart-completer--buffer-name
					 :buffer (smart-completer--get-log-buffer)))
		(setq smart-completer-process
					(make-process
					 :name smart-completer--process-name
					 :command (cons
										 smart-completer-executable-command
										 smart-completer-executable-args)
					 :coding 'utf-8
					 :connection-type 'pipe
					 :filter #'smart-completer-process-filter
					 :stderr smart-completer-log-process))
		;; (start-process
		;;  smart-completer--process-name
		;;  nil
		;;  smart-completer-executable-command
		;;  (car smart-completer-executable-args))
		;; (set-process-filter smart-completer-process
		;; 										'smart-completer-process-filter)
	))

(defun smart-completer-disable ()
	"Disable smart-completer."
	(when smart-completer-process
		(delete-process smart-completer-process)
		(setq smart-completer-process nil))
	(when smart-completer-log-process
		(delete-process smart-completer-log-process)
		(setq smart-completer-log-process nil)))

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
		(setq smart-completer-processing t)
		(let* ((json-null nil)
					 (json-encoding-pretty-print nil)
					 (message (list
										 :command "complete"
										 :args (list
														:prefix prefix
														:context "test")))
					 (encoded (concat (unicode-escape* (json-encode-plist message)) "\n")))
			(process-send-string smart-completer-process encoded))))

(defun smart-completer--decode (msg)
	(alist-get 'candidates
						 (let ((json-array-type 'list))
							 (json-read-from-string msg))))

;; sync
;; (defun smart-completer-process-filter (process output)
;; 	(setq smart-completer-process-return output))

(defun smart-completer-process-filter (process output)
	(when smart-completer-async-callback
		(setq output (s-split "\n" output t))
		(dolist (chunk output)
			(funcall smart-completer-async-callback
							 (smart-completer--decode chunk)))
		(setq smart-completer-async-callback nil)
		(setq smart-completer-processing nil)))

(defun company-smart-completer (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-smart-completer))
    (prefix (company-grab-symbol))
    (candidates
			(smart-completer-query arg)
			(setq smart-completer-async-prefix arg)
			'(:async . (lambda (callback)
				(setq smart-completer-async-callback callback)
			)))

		;; sync
    ;; (candidates (when-let ((response (smart-completer-query arg)))
		;; 	(s-split "\t" response t)))
    (meta (format "This value is named %s" arg))
		(no-cache t)
		;; (sorted t)
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
