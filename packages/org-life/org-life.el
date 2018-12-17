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
;; some code snippets taken from and inspired by:
;; https://github.com/weirdNox/dotfiles/blob/26c5c2739aff28af5ed4d6f243c7ec0e9b581821/config/.emacs.d/config.org#agenda
;;

;;; Code:

;;
;; Dependencies
;;

(require 'cl-lib)
(require 'json)
(require 'unicode-escape)

;;
;; Constants
;;

(defconst org-life--engine-process-name "org-life--engine-process")
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

(defcustom org-life-wait 5
  "Number of seconds to wait for engine to respond."
  :group 'org-life
  :type 'float)

;;
;; Faces
;;

;;
;; Variables
;;

(defvar org-life--engine-process nil
  "Engine process.")

(defvar org-life--response nil
  "Temporarily stored engine responses.")

;;
;; Major mode definition
;;

;;
;; Global methods
;;

;; Engine

(defun org-life-start-engine ()
  "Start engine process."
  (org-life-kill-process)
  (let ((process-connection-type nil))
    (setq org-life--engine-process
          (make-process
           :name org-life--engine-process-name
           :command org-life-engine-command
           :coding 'utf-8
           :connection-type 'pipe
           :filter #'org-life--engine-process-filter
           :sentinel #'org-life--engine-process-sentinel
           :noquery t)))
  ; hook setup
  (dolist (hook org-life--hooks-alist)
    (add-hook (car hook) (cdr hook))))

(defun org-life-kill-process ()
  "Kill TabNine process."
  (when org-life--engine-process
    (let ((process org-life--engine-process))
      (setq org-life--engine-process nil) ; this happens first so sentinel don't catch the kill
      (delete-process process)))
  ; hook remove
  (dolist (hook org-life--hooks-alist)
    (remove-hook (car hook) (cdr hook))))

(defun org-life-send-request (request)
  "Send REQUEST to engine.  REQUEST needs to be JSON-serializable object.
Return the response from engine if arrived before `org-life-wait' seconds."
  (when (null org-life--engine-process)
    (org-life-start-engine))
  (when org-life--engine-process
    (let ((json-null nil)
          (json-encoding-pretty-print nil)
          ;; TODO make sure utf-8 encoding works
          (encoded (concat (unicode-escape* (json-encode-plist request)) "\n")))
      (setq org-life--response nil)
      (process-send-string org-life--engine-process encoded)
      (accept-process-output org-life--engine-process org-life-wait)
      org-life--response)))

(defun org-life-schedule ()
  "Query engine for scheduling."
  (org-life-send-request
   (list :command "schedule")))

(defun org-life--decode (msg)
  "Decode engine response MSG, and return the decoded object."
  (let ((json-array-type 'list)
        (json-object-type 'alist))
    (json-read-from-string msg)))

(defun org-life--engine-process-sentinel (process event)
  "Sentinel for engine process.
PROCESS is the process under watch, EVENT is the event occurred."
  (when (and org-life--engine-process
             (memq (process-status process) '(exit signal)))
    (message "org-life engine process shutdown.")))

(defun org-life--engine-process-filter (process output)
  "Filter for engine process.
PROCESS is the process under watch, OUTPUT is the output received."
  (setq output (s-split "\n" output t))
  (setq org-life--response
        (org-life--decode (car (last output)))))

;; Agenda

;; Agenda helper

(defun org-life-agenda-sort-by-priority (a b)
  (let ((pa (or (org-life-agenda-entry-priority a) org-lowest-priority))
        (pb (or (org-life-agenda-entry-priority b) org-lowest-priority)))
    (< pa pb)))

(defun org-life-agenda-flatten-list (l)
  (cond ((not l) nil)
        ((atom l) (list l))
        (t (append (org-life-agenda-flatten-list (car l)) (org-life-agenda-flatten-list (cdr l))))))

;; Agenda renderer

(defun org-life-agenda-format-entry (prefix entry)
  (let ((props (list 'mouse-face 'highlight
                     'undone-face nil
                     'done-face 'org-agenda-done
                     'org-marker (org-life-agenda-entry-marker entry)
                     'org-hd-marker (org-life-agenda-entry-marker entry)
                     'todo-state (org-life-agenda-entry-todo-keyword entry)
                     'org-todo-regexp org-todo-regexp
                     'org-not-done-regexp org-not-done-regexp
                     'org-complex-heading-regexp org-complex-heading-regexp
                     'org-highest-priority org-highest-priority
                     'org-lowest-priority org-lowest-priority
                     'tags (mapcar 'org-downcase-keep-props (org-life-agenda-entry-tags entry))
                     'format `(() ,prefix)))
        (text
         (concat prefix
                 (if (org-life-agenda-entry-todo-keyword entry)
                     (concat (org-life-agenda-entry-todo-keyword entry) " ")
                   "")
                 (if (org-life-agenda-entry-priority entry)
                     (string ?\[ ?# (org-life-agenda-entry-priority entry) ?\] ? )
                   "")
                 (org-life-agenda-entry-text entry)
                 ;; (let ((deadline (org-life-agenda-entry-deadline entry)))
                 ;;   (or (and deadline
                 ;;            (org-timestamp-format deadline "%Y-%m-%d"))
                 ;;       ""))
                 (if (org-life-agenda-entry-tags entry)
                     (concat " :" (mapconcat #'identity (org-life-agenda-entry-tags entry) ":") ":")
                   ""))))

    (add-text-properties (length prefix) (length text) '(org-heading t) text)
    (setq text (concat (org-add-props text props) "\n"))
    (org-agenda-highlight-todo text)))

(cl-defun org-life-agenda-render-block-separator ()
  (unless (or (bobp) org-agenda-compact-blocks
              (not org-agenda-block-separator))
    (insert "\n"
            (if (stringp org-agenda-block-separator)
                org-agenda-block-separator
              (make-string (window-width) org-agenda-block-separator))
            "\n")))

(cl-defun org-life-agenda-render-entries (&key entries)
  (setq entries (sort entries #'org-life-agenda-sort-by-priority))
  (dolist (entry entries)
    (insert
     (org-life-agenda-format-entry " " entry))))

(cl-defun org-life-agenda-render-block (&key entries title (entries-renderer nil))
  (when entries
    (let ((begin (point))
          (entries-renderer (or entries-renderer
                                #'org-life-agenda-render-entries)))
      (org-life-agenda-render-block-separator)
      (insert (org-add-props title nil 'face 'org-agenda-structure) "\n")
      (funcall entries-renderer :entries entries)
      (add-text-properties begin (point-max) `(org-agenda-type tags)))))

(defmacro org-life-agenda-render-section (var-list process-ast render-blocks)
  `(catch 'exit
     (let ((files (org-agenda-files nil 'ifmode))
           ,@var-list)
       (while (setq file (pop files))
         (org-check-agenda-file file)
         (setq buffer (if (file-exists-p file)
                          (org-get-agenda-file-buffer file)
                        (error "No such file %s" file)))

         (unless org-todo-regexp
           (dolist (variable '(org-todo-regexp org-not-done-regexp org-complex-heading-regexp
                                               org-done-keywords org-done-keywords-for-agenda))
             (set variable (buffer-local-value variable buffer))))

         (with-current-buffer buffer
           (org-with-wide-buffer
            (unless (derived-mode-p 'org-mode) (error "Agenda file %s is not in Org mode" file))
            (setq ast (org-element-parse-buffer 'headline))
            ,@process-ast)))

       (let ((inhibit-read-only t))
         (goto-char (point-max))
         ,@render-blocks))))

(cl-defun org-life-agenda-render-agenda (&key agenda-data schedule-data)
  (org-life-agenda-render-block :entries agenda-data
                                :title "Test"))

;; Agenda processing

(cl-defstruct org-life-agenda-entry
  todo-type
  todo-keyword
  priority
  text
  tags
  scheduled
  deadline
  planned
  effort
  marker
  project-status
  children)

(defun org-life-agenda-entry-new (headline-elem &optional given-tags)
  (let ((todo-type (org-element-property :todo-type headline-elem))
        (todo-keyword (org-element-property :todo-keyword headline-elem))
        (priority (org-element-property :priority headline-elem))
        (text (org-element-property :raw-value headline-elem))
        (tags (org-element-property :tags headline-elem))
        (scheduled (org-element-property :scheduled headline-elem))
        (deadline (org-element-property :deadline headline-elem))
        (effort (org-element-property :EFFORT headline-elem))
        (begin (org-element-property :begin headline-elem)))
    (make-org-life-agenda-entry
     :todo-type todo-type
     :todo-keyword todo-keyword
     :priority priority
     :text text
     :tags (or given-tags tags)
     :scheduled scheduled
     :deadline deadline
     :effort (or (and effort (org-duration-to-minutes effort)) 0)
     :marker (org-agenda-new-marker begin))))

(defun org-life-agenda-get-tasks (headline-elem)
  (let ((children
         (org-element-map
             (org-element-contents headline-elem) ; data
             'headline ; types
           #'org-life-agenda-get-tasks ; fun
           nil ; info
           nil ; first-match
           'headline ; no-recursion
           )))
    
    (if (eq 'todo (org-element-property :todo-type headline-elem))
        (cons
         (org-life-agenda-entry-new headline-elem)
         children)
      children)))

(defun org-life-agenda-process-agenda-files (initial-value ast-processor)
  (let ((files (org-agenda-files nil 'ifmode))
        (acc initial-value))
    
    (while (setq file (pop files))
      (org-check-agenda-file file)
      (setq buffer (if (file-exists-p file)
                       (org-get-agenda-file-buffer file)
                     (error "No such file %s" file)))

      (unless org-todo-regexp
        (dolist (variable '(org-todo-regexp
                            org-not-done-regexp
                            org-complex-heading-regexp
                            org-done-keywords
                            org-done-keywords-for-agenda))
          (set variable (buffer-local-value variable buffer))))

      (with-current-buffer buffer
        (org-with-wide-buffer
         (unless (derived-mode-p 'org-mode) (error "Agenda file %s is not in Org mode" file))
         (let ((ast (org-element-parse-buffer 'headline)))
           (setq acc (funcall ast-processor acc ast))))))
    
    acc))

(defun org-life-agenda-get-agenda-data ()
  (let ((tasks
         (org-life-agenda-process-agenda-files
          '()
          (lambda (acc ast) ; ast-processor
            (append (org-life-agenda-flatten-list
                     (org-element-map
                         ast ; data
                         'headline ; types
                       #'org-life-agenda-get-tasks ; fun
                       nil ; info
                       nil ; first-match
                       'headline ; no-recursion
                       ))
                    acc)))))
    tasks))

(defun org-life-agenda-get-scheduler-request (agenda-data)
  (message "TODO: org-life-agenda-get-scheduler-request not implemented.")
  nil)

(defun org-life-agenda-get-schedule-data (agenda-data)
  (let (request
        response)

    (message "TODO: org-life-agenda-get-schedule-data not implemented.")
    ;; (setq request (org-life-agenda-get-scheduler-request agenda-data))
    ;; (setq response (org-life-send-request request))
    ;; TODO add response to agenda-data and return
    ))

;; Agenda main

(defun org-life-agenda (&rest _)
      (let (agenda-data
          schedule-data)
      
      (setq agenda-data (org-life-agenda-get-agenda-data))
      (setq schedule-data (org-life-agenda-get-schedule-data agenda-data))
      
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (org-life-agenda-render-agenda :agenda-data agenda-data
                                       :schedule-data schedule-data))))

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
