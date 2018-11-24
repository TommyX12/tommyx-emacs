;;; org-life.el --- TODO
;;
;; Copyright (c) 2018 Tommy Xiang
;;
;; Author: Tommy Xiang <tommyx058@gmail.com>
;; Keywords: convenience, calendar, TODO
;; Version: 0.0.1
;; URL: https://github.com/TommyX12/org-life/
;; Package-Requires: TODO
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;;; Commentary:
;;
;; TODO
;;

;;; Code:

;;
;; Dependencies
;;

(require 'unicode-escape)

;;
;; Constants
;;

(defconst org-life--process-name "org-life--process")
(defconst org-life--hooks-alist nil)

;;
;; Macros
;;

;;
;; Customization
;;

(defgroup org-life nil
  "Options for org-life."
  :link '(url-link :tag "Github" "https://github.com/TommyX12/org-life")
  :group 'org
  :prefix "org-life-")

(defcustom org-life-engine-command
  (list
   (if (eq system-type 'darwin)
       "python3"
     "python")
   (expand-file-name "engine/main.py"
                     (file-name-directory load-file-name)))
  "Command to start the scheduler engine."
  :group 'org-life
  :type 'string)

(defcustom org-life-wait 2
  "Number of seconds to wait for engine to respond."
  :group 'org-life
  :type 'float)

;;
;; Faces
;;

;;
;; Variables
;;

(defvar org-life--process nil
  "Engine process.")

(defvar org-life--response nil
  "Temporarily stored engine responses.")

;;
;; Major mode definition
;;

;;
;; Global methods
;;

(defun org-life-start-engine ()
  "Start engine process."
  (org-life-kill-process)
  (let ((process-connection-type nil))
    (setq org-life--process
          (make-process
           :name org-life--process-name
           :command org-life-engine-command
           :coding 'utf-8
           :connection-type 'pipe
           :filter #'org-life--process-filter
           :sentinel #'org-life--process-sentinel
           :noquery t)))
  ; hook setup
  (message "Engine process started.")
  (dolist (hook org-life--hooks-alist)
    (add-hook (car hook) (cdr hook))))

(defun org-life-kill-process ()
  "Kill TabNine process."
  (when org-life--process
    (let ((process org-life--process))
      (setq org-life--process nil) ; this happens first so sentinel don't catch the kill
      (delete-process process)))
  ; hook remove
  (dolist (hook org-life--hooks-alist)
    (remove-hook (car hook) (cdr hook))))

(defun org-life-send-request (request)
  "Send REQUEST to engine.  REQUEST needs to be JSON-serializable object.
Return the response from engine if arrived before `org-life-wait' seconds."
  (when (null org-life--process)
    (org-life-start-engine))
  (when org-life--process
    (let ((json-null nil)
          (json-encoding-pretty-print nil)
          ;; TODO make sure utf-8 encoding works
          (encoded (concat (unicode-escape* (json-encode-plist request)) "\n")))
      (setq org-life--response nil)
      (process-send-string org-life--process encoded)
      (accept-process-output org-life--process org-life-wait)
      org-life--response)))

(defun org-life-schedule ()
  "Query engine for scheduling."
  (org-life-send-request
   (list :command "schedule")))

(defun org-life--decode (msg)
  "Decode engine response MSG, and return the decoded object."
  (let ((json-array-type 'list)
        (json-object-type 'alist))
    (message msg)
    (json-read-from-string msg)))

(defun org-life--process-sentinel (process event)
  "Sentinel for engine process.
PROCESS is the process under watch, EVENT is the event occurred."
  (when (and org-life--process
             (memq (process-status process) '(exit signal)))
    (message "Engine process shutdown.")))

(defun org-life--process-filter (process output)
  "Filter for engine process.
PROCESS is the process under watch, OUTPUT is the output received."
  (setq output (s-split "\n" output t))
  (setq org-life--response
        (org-life--decode (car (last output)))))

;;
;; Interactive functions
;;

(defun org-life-restart-engine ()
  "Start/Restart engine."
  (interactive)
  (org-life-start-engine))

;;
;; Advices
;;

;;
;; Hooks
;;



(provide 'org-life)

;;; org-life.el ends here
