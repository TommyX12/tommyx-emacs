;; org-life.el --- TODO
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
(require 'dash)
(require 'ht)
(require 'json)
(require 'org)
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

(defcustom org-life-config-file-path
  (expand-file-name "org-life-config.org"
                    (file-name-directory org-directory))
  "Path to org-life config file."
  :group 'org-life
  :type 'string)

;;
;; Faces
;;

(defface org-life-agenda-stress-best-face
  '((t (:inherit org-warning)))
  "Face to highlight best stress for org-life-agenda."
  :group 'org-life)

(defface org-life-agenda-stress-normal-face
  '((t (:inherit font-lock-function-name-face)))
  "Face to highlight normal stress for org-life-agenda."
  :group 'org-life)

(defface org-life-agenda-stress-warning-face
  '((t (:inherit success)))
  "Face to highlight warning stress for org-life-agenda."
  :group 'org-life)

;;
;; Variables
;;

(defvar org-life--engine-process nil
  "Engine process.")

(defvar org-life--response nil
  "Temporarily stored engine responses.")

(defvar org-life--temp-id 0)

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
Return the response from engine if arrived before `org-life-wait' seconds.

Only parse plist, not alist."
  (when (null org-life--engine-process)
    (org-life-start-engine))
  (when org-life--engine-process
    (cl-letf (((symbol-function 'json-alist-p) (lambda (list) nil)))
      (let ((json-null nil)
            (json-encoding-pretty-print nil)
            ;; TODO make sure utf-8 encoding works
            (encoded (concat (unicode-escape* (json-encode request)) "\n")))
        (setq org-life--response nil)
        (process-send-string org-life--engine-process encoded)
        (accept-process-output org-life--engine-process org-life-wait)
        org-life--response))))

(defun org-life-schedule ()
  "Query engine for scheduling."
  (org-life-send-request
   (list :command "schedule")))

(defun org-life--decode (msg)
  "Decode engine response MSG, and return the decoded object."
  (let ((json-array-type 'list)
        (json-object-type 'plist))
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

(defun org-life-agenda-timestamp-to-request (timestamp &optional default)
  (or (and timestamp
           (org-timestamp-format timestamp "%Y-%m-%d"))
      default))

(defun org-life-agenda-today-date-to-request ()
  (format-time-string "%Y-%m-%d"))

(defun org-life-agenda-list-to-ht (lst key-func)
  (let ((result (ht-create)))
    (dolist (item lst)
      (ht-set! result (funcall key-func item) item))
    result))

(defun org-life-agenda-date-string-to-time (date-string)
  (org-read-date nil t date-string nil))

(defun org-life-agenda-time-to-weekday (time)
  (cons (format-time-string "%a" time)
        (string-to-number (format-time-string "%u" time))))

(defun org-life-get-buffer (file)
  "Get a buffer visiting FILE."
  (let ((buf (org-find-base-buffer-visiting file)))
    (if buf
        buf
      (setq buf (find-file-noselect file))
      buf)))

(defmacro org-life-with-config-buffer (&rest body)
  `(with-current-buffer
       (org-life-get-buffer ,org-life-config-file-path)
     (org-with-wide-buffer
      ,@body)))

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
            ;; (if (stringp org-agenda-block-separator)
            ;;     org-agenda-block-separator
            ;;   (make-string (window-text-width) org-agenda-block-separator))
            ;; "\n"
            )))

(cl-defun org-life-agenda-render-multi-progress-bar (&key bar-width progress-list (fill-char ?=) (empty-char ? ))
  (insert "|")
  (let ((bar-width (- bar-width 2))
        (cur-progress 0)
        (cur-width 0))
    
    (while progress-list
      (let ((progress (caar progress-list))
            (face (cdar progress-list)))
        (when (> progress cur-progress)
          (let* ((width (round (* (float progress) bar-width)))
                 (diff (- width cur-width)))
            (insert (propertize (make-string diff fill-char)
                                'face face))
            (setq cur-progress progress)
            (setq cur-width width)))

        (setq progress-list (cdr progress-list))))

    (let ((diff (- bar-width cur-width)))
      (insert (make-string diff empty-char))))
  (insert "|"))

(cl-defun org-life-agenda-render-entries (&key entries)
  (setq entries (sort entries #'org-life-agenda-sort-by-priority))
  (dolist (entry entries)
    (insert
     (org-life-agenda-format-entry " " entry))))

(cl-defun org-life-agenda-render-impossible-task (&key impossible-task
                                                       tasks-dict)
  (let* ((id (plist-get impossible-task :id))
         (amount (plist-get impossible-task :amount))
         (entry (ht-get tasks-dict id)))
    (insert (org-life-agenda-format-entry
             (concat " "
                     (propertize
                      (format "%-8s" (org-duration-from-minutes amount))
                      'face 'error)
                     "| ")
             entry))))

(cl-defun org-life-agenda-render-day (&key daily-info
                                           tasks-dict
                                           (today nil))
  (let* ((date-string (plist-get daily-info :date))
         (work-time (plist-get daily-info :work_time))
         (sessions (plist-get daily-info :sessions))
         (free-time (plist-get daily-info :free_time))
         (average-stress (plist-get daily-info :average_stress))
         (time (org-life-agenda-date-string-to-time date-string))
         (weekday (org-life-agenda-time-to-weekday time))
         (weekday-num (cdr weekday))
         (date-string (if today
                          (propertize date-string
                                      'face
                                      'org-agenda-date-today)
                        (propertize date-string
                                    'face
                                    'org-agenda-date)))
         (weekday-text (if (or (= weekday-num 6)
                               (= weekday-num 7))
                           (propertize (car weekday)
                                       'face
                                       'org-agenda-date-weekend)
                         (car weekday)))
         )
    (insert weekday-text " " date-string " | "
            (org-duration-from-minutes work-time) " | "
            (org-duration-from-minutes free-time) " | "
            (format "%.3f" average-stress) " | "
            "\n")
    (dolist (session sessions)
      (let* ((id (plist-get session :id))
             (amount (plist-get session :amount))
             (type (plist-get session :type))
             (entry (ht-get tasks-dict id)))
        (insert (org-life-agenda-format-entry
                 (concat " "
                         (format "%-8s" (org-duration-from-minutes amount))
                         "| ")
                 entry))))))

(cl-defun org-life-agenda-render-block-title (&key title)
  (insert (org-add-props title nil 'face 'org-agenda-structure) "\n"))

(cl-defun org-life-agenda-render-block-sub-title (&key title)
  (insert (org-add-props title nil 'face 'bold) "\n"))

(cl-defun org-life-agenda-render-block (&key entries title (entries-renderer nil))
  (when entries
    (let ((begin (point))
          (entries-renderer (or entries-renderer
                                #'org-life-agenda-render-entries)))
      (org-life-agenda-render-block-separator)
      (org-life-agenda-render-block-title title)
      (funcall entries-renderer :entries entries)
      (add-text-properties begin (point-max) `(org-agenda-type tags)))))

(cl-defun org-life-agenda-render-error (&key message)
  (insert (propertize "Error" 'face 'error)
          "\n"
          message))

(cl-defun org-life-agenda-render-general-info (&key general-info)
  (let ((stress (plist-get general-info :stress))
        (highest-stress-date (plist-get general-info :highest_stress_date))
        (stress-with-fragments (plist-get general-info :stress_with_fragments))
        (stress-without-today (plist-get general-info :stress_without_today)))
    (insert (format "Stress: %.3f | %.3f | %.3f" stress stress-with-fragments stress-without-today) "\n")
    (org-life-agenda-render-multi-progress-bar
     :bar-width (window-text-width)
     :progress-list (list (cons stress 'org-life-agenda-stress-best-face)
                          (cons stress-with-fragments 'org-life-agenda-stress-normal-face)
                          (cons stress-without-today 'org-life-agenda-stress-warning-face)))
    (insert "\n")
    (insert (format (concat "Highest Stress Date: "
                            (propertize "%s"
                                        'face 'org-agenda-date)
                            " (in %d days)")
                    highest-stress-date
                    (- (time-to-days
                        (org-life-agenda-date-string-to-time
                         highest-stress-date))
                       (time-to-days
                        (current-time))))
            "\n")))

(cl-defun org-life-agenda-render-alerts (&key alerts
                                              tasks-dict)
  ;; impossible tasks
  (let ((impossible-tasks (plist-get alerts :impossible_tasks)))
    (when (> (length impossible-tasks) 0)
      (org-life-agenda-render-block-sub-title :title "Impossible Tasks")
      (dolist (impossible-task impossible-tasks)
        (org-life-agenda-render-impossible-task :impossible-task impossible-task
                                                :tasks-dict tasks-dict)))))

(cl-defun org-life-agenda-render-agenda (&key agenda-data schedule-data)
  (let ((status (plist-get schedule-data :status))
        (err (plist-get schedule-data :err))
        (data (plist-get schedule-data :data)))
    (if (string= status "error")
        (org-life-agenda-render-error err)
      ;; (org-life-agenda-render-block :entries (plist-get agenda-data :tasks)
      ;;                               :title "Test")
      (let ((tasks-dict (plist-get agenda-data :tasks-dict))
            (general-info (plist-get data :general))
            (alerts (plist-get data :alerts))
            (daily-infos (plist-get data :daily_infos))

            (today t))
        
        (org-life-agenda-render-block-title :title "General")
        (org-life-agenda-render-general-info :general-info general-info)
        
        (org-life-agenda-render-block-separator)
        
        (org-life-agenda-render-block-title :title "Alerts")
        (org-life-agenda-render-alerts :alerts alerts
                                       :tasks-dict tasks-dict)
        
        (org-life-agenda-render-block-separator)
        
        (org-life-agenda-render-block-title :title "Schedules")
        (dolist (daily-info daily-infos)
          (org-life-agenda-render-day :daily-info daily-info
                                      :tasks-dict tasks-dict
                                      :today today)
          (insert "\n")
          (setq today nil))))))

;; Agenda processing

(cl-defstruct org-life-agenda-entry
  id
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

(defun org-life-agenda-entry-from-headline (id headline-elem &optional given-tags)
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
     :id id
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
        (progn
          (setq org-life--temp-id (1+ org-life--temp-id))
          (cons
           (org-life-agenda-entry-from-headline org-life--temp-id headline-elem)
           children))
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
  (setq org-life--temp-id 0)
  
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
    (list :tasks tasks
          :tasks-dict (org-life-agenda-list-to-ht
                       tasks
                       (lambda (task)
                         (org-life-agenda-entry-id task))))))

(defun org-life-agenda-get-scheduler-request-config ()
  "TODO actually allow config"
  (org-life-with-config-buffer
   (unless (derived-mode-p 'org-mode) (error "Agenda file %s is not in Org mode" file))
   (let* ((ast (org-element-parse-buffer 'headline))
          (children
           (org-element-map
               ast ; data
               'headline ; types
             ;; fun
             (lambda (headline-elem)) ; TODO grab the information
             nil ; info
             nil ; first-match
             'headline ; no-recursion
             )))))
  
  (list
   :today (org-life-agenda-today-date-to-request)
   :scheduling_days 365
   :daily_info_days 14
   :fragmentation_config (list
                          :max_stress 0.8
                          :max_percentage 0.4
                          :preferred_fragment_size 45
                          :min_fragment_size 15)))

(defun org-life-agenda-get-scheduler-request-tasks (agenda-tasks)
  "TODO deal with repeats. deal with non-scheduled tasks. deal with non-effort tasks. deal with amount done. deal with priority."
  (-non-nil
   (-map (lambda (task)
           (when (and
                  (eq 'todo (org-life-agenda-entry-todo-type task)))
             (list
              :id (org-life-agenda-entry-id task)
              :start (org-life-agenda-timestamp-to-request
                      (org-life-agenda-entry-scheduled task)
                      (org-life-agenda-today-date-to-request))
              :end (org-life-agenda-timestamp-to-request
                      (org-life-agenda-entry-deadline task)
                      "max")
              :amount (org-life-agenda-entry-effort task)
              :done 0
              :priority (org-life-agenda-entry-priority task))))
         agenda-tasks)))

(defun org-life-agenda-get-scheduler-request-work-time ()
  "TODO actually parse work time"
  (list
   (list
    :selector "default"
    :duration 480)))

(defun org-life-agenda-get-scheduler-request (agenda-data)
  "TODO"
  (list
   :command "schedule"
   :args (list
          :config (org-life-agenda-get-scheduler-request-config)
          :tasks (org-life-agenda-get-scheduler-request-tasks
                  (plist-get agenda-data :tasks))
          :work_time (org-life-agenda-get-scheduler-request-work-time))))

(defun org-life-agenda-get-schedule-data (agenda-data)
  (let (request
        response)

    (setq request (org-life-agenda-get-scheduler-request agenda-data))
    (setq response (org-life-send-request request))))

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
