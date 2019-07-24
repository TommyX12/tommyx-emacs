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
(defconst smart-completer--hooks-alist
	'((after-save-hook . smart-completer--on-save)))

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

(defvar smart-completer-max-parse-radius 80000)
(defvar smart-completer-max-context-chars 100)
(defvar smart-completer-max-prefix-chars 100)

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
		)
	; hook setup
	(dolist (hook smart-completer--hooks-alist)
		(add-hook (car hook) (cdr hook))))

(defun smart-completer-disable ()
	"Disable smart-completer."
	(when smart-completer-process
		(delete-process smart-completer-process)
		(setq smart-completer-process nil))
	(when smart-completer-log-process
		(delete-process smart-completer-log-process)
		(setq smart-completer-log-process nil))
	(dolist (hook smart-completer--hooks-alist)
		(remove-hook (car hook) (cdr hook))))

;; sync
;; (defun smart-completer-query (prefix)
;; 	"TODO"
;; 	(when smart-completer-process
;; 		(setq smart-completer-process-return nil)
;; 		(process-send-string smart-completer-process (concat prefix "\n"))
;; 		(accept-process-output smart-completer-process 1)
;; 		smart-completer-process-return))

(defun smart-completer-send-command (command)
	"TODO"
	(if (null smart-completer-process)
      (smart-completer-enable)
		(let ((json-null nil)
					(json-encoding-pretty-print nil)
					(encoded (concat (unicode-escape* (json-encode-plist command)) "\n")))
			(process-send-string smart-completer-process encoded))))

(defun smart-completer-query (prefix context)
	"TODO"
	(smart-completer-send-command
	 (list
		:command "complete"
		:args (list
					:file_name (or (buffer-file-name) nil)
					:prefix prefix
					:context context))))

(defun smart-completer-parse ()
  (smart-completer-send-command
	 (list
		:command "parse"
		:args (list
					:file_name (or (buffer-file-name) nil)
					:content (buffer-substring-no-properties
										(max (point-min) (- (point)
																				smart-completer-max-parse-radius))
										(min (point-max) (+ (point)
																				smart-completer-max-parse-radius)))))))

(defun smart-completer-log-status ()
  (interactive)
  (smart-completer-send-command
   (list
    :command "log_status"
    :args nil)))

(defun smart-completer--on-save ()
	"TODO"
	(smart-completer-parse))

(defun smart-completer--decode (msg)
  (setq msg (let ((json-array-type 'list))
              (json-read-from-string msg)))
  (if (alist-get 'not_parsed msg)
      (smart-completer-parse)
    (alist-get 'candidates msg)))

;; sync
;; (defun smart-completer-process-filter (process output)
;; 	(setq smart-completer-process-return output))

(defun smart-completer-process-filter (process output)
	(when smart-completer-async-callback
		(setq output (s-split "\n" output t))
		(dolist (chunk output)
			(funcall smart-completer-async-callback
							 (smart-completer--decode chunk)))))

(defun smart-completer--word-char-p (char)
  (or
   (and (>= char ?a) (<= char ?z))
   (and (>= char ?A) (<= char ?Z))
   (and (>= char ?0) (<= char ?9))
   (= char ?_)))

(defun smart-completer--grab-prefix ()
  (let ((i (point)) (k 0) c)
    (while (and (< k smart-completer-max-prefix-chars)
                (> i (point-min))
                (smart-completer--word-char-p (char-before i)))
      (setq i (1- i))
      (setq k (1+ k)))
    (buffer-substring i (point))))

(defun company-smart-completer (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-smart-completer))
    (prefix `(,(smart-completer--grab-prefix) . t))
    (candidates
		 (let ((context-point (- (point) (length arg))))
			 (smart-completer-query arg
														  (buffer-substring
														   (max (point-min) (- context-point
																			   smart-completer-max-context-chars))
														   context-point)))
		 (setq smart-completer-async-prefix arg)
		 '(:async . (lambda (callback)
		 	(setq smart-completer-async-callback callback)
		 )))

		;; sync
    ;; (candidates (when-let ((response (smart-completer-query arg)))
		;; 	(s-split "\t" response t)))
    ;; (meta (format "This value is named %s" arg))
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
