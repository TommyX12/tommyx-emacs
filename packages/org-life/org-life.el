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

;;; Dependencies

(require 'cl-lib)
(require 'dash)
(require 'ht)
(require 'json)
(require 'org)
(require 'unicode-escape)

;;; Constants

(defconst org-life--engine-process-name "org-life--engine-process")
(defconst org-life--hooks-alist nil)

;; enums
(defconst org-life--task-status-enum-todo 0)
(defconst org-life--task-status-enum-done 1)
(defconst org-life--task-repeat-type-enum-normal 0)
(defconst org-life--task-repeat-type-enum-restart 1)
(defconst org-life--task-repeat-unit-enum-none 0)
(defconst org-life--task-repeat-unit-enum-day 1)
(defconst org-life--task-repeat-unit-enum-week 2)
(defconst org-life--task-repeat-unit-enum-month 3)
(defconst org-life--task-repeat-unit-enum-year 4)
(defconst org-life--session-type-enum-task 0)
(defconst org-life--session-type-enum-fragment 1)
(defconst org-life--session-type-enum-overlimit 2)
(defconst org-life--session-weakness-enum-strong 0)
(defconst org-life--session-weakness-enum-weak 1)

;;; Macros

;;; Customization

(defgroup org-life nil
  "Options for org-life."
  :link '(url-link :tag "Github" "https://github.com/TommyX12/org-life")
  :group 'org
  :prefix "org-life-")

(defcustom org-life-engine-command
  (list
   (if (not (eq system-type 'windows-nt))
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

(defcustom org-life-echo-store-messages nil
  "Whether to store echo messages to messages buffer."
  :group 'org-life
  :type 'boolean)

(defcustom org-life-config-file-path
  (expand-file-name "org-life-config.org" org-directory)
  "Path to org-life config file."
  :group 'org-life
  :type 'string)

(defcustom org-life-default-config
  (list
   :show_debug_messages nil
   :scheduling_days 365
   :daily_info_days 14
   :random_power 1
   :default_urgency 40
   :fragmentation_config (list
                          :min_extra_time_ratio 1.0
                          :max_percentage 0.5
                          :preferred_fragment_size 45
                          :min_fragment_size 15))
  "Default configuration for the scheduler."
  :group 'org-life
  :type 'list)

(defcustom org-life-default-usable-time-config
  (list
   (list
    :selector "default"
    :duration 480))
  "Default configuration of usable time."
  :group 'org-life
  :type 'list)

(defcustom org-life-agenda-progress-fill-char ?=
  "Char to fill progress bar."
  :group 'org-life
  :type 'integer)

(defcustom org-life-agenda-progress-empty-char 32
  "Char to fill empty part of progress bar."
  :group 'org-life
  :type 'integer)

(defcustom org-life-agenda-visualize-probability nil
  "Whether agenda visualization use failure probability instead of workload."
  :group 'org-life
  :type 'boolean)

;;; Faces

(defface org-life-agenda-secondary-face
  '((t (:inherit font-lock-comment-face)))
  "Face for secondary text (such as certain statistics) for org-life-agenda."
  :group 'org-life)

(defface org-life-agenda-session-face-1
  '((t (:inherit default)))
  "First face for daily sessions for org-life-agenda."
  :group 'org-life)

(defface org-life-agenda-session-face-2
  '((t (:inherit highlight)))
  "Second face for daily sessions for org-life-agenda."
  :group 'org-life)

(defface org-life-agenda-empty-progress-face
  '((t ()))
  "Face to highlight empty progress for org-life-agenda."
  :group 'org-life)

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

(defface org-life-agenda-usable-time-face
  '((t (:inherit org-time-grid)))
  "Face to highlight usable time for org-life-agenda."
  :group 'org-life)

(defface org-life-agenda-actual-usable-time-face
  '((t (:inherit org-agenda-done)))
  "Face to highlight actual usable time for org-life-agenda."
  :group 'org-life)

;;; Variables

(defvar org-life--engine-process nil
  "Engine process.")

(defvar org-life--response nil
  "Temporarily stored engine responses.")

(defvar org-life--temp-id 0)
(defvar org-life--temp-tasks nil)
(defvar org-life--temp-clocks nil)
(defvar org-life--response-buffer "")
(defvar org-life--agenda-data-cache nil)
(defvar org-life--schedule-data-cache nil)
(defvar org-life--agenda-keep-cache nil)
(defvar org-life--agenda-current-view nil)
(defvar org-life--last-org-buffer nil)
(defvar org-life--agenda-keep-markers nil)
(defvar org-life--last-updated-time nil)

;;; Major mode definition

;;; Global methods

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
    (condition-case error-data
        (json-read-from-string msg)
      (error
       (format "error-symbol: %s\nsignal-data: %s\nreceived-message: %s\n"
               (prin1-to-string (car error-data))
               (prin1-to-string (cdr error-data))
               (prin1-to-string msg))))))

(defun org-life--engine-process-sentinel (process event)
  "Sentinel for engine process.
PROCESS is the process under watch, EVENT is the event occurred."
  (when (and org-life--engine-process
             (memq (process-status process) '(exit signal)))
    (message "org-life engine process shutdown.")))

(defun org-life--engine-process-filter (process output)
  "Filter for engine process.
PROCESS is the process under watch, OUTPUT is the output received."
  (setq org-life--response-buffer
        (concat org-life--response-buffer
                output))
  (when (s-ends-with-p "\n" output)
    (setq org-life--response (org-life--decode org-life--response-buffer)
          org-life--response-buffer "")))

;;; Agenda

;; Agenda main

(defun org-life-agenda (&rest _)
  (unless org-life--agenda-keep-cache
    (org-life-invalidate-cache))
  (org-life-agenda-render-view
   (or org-life--agenda-current-view
       'main)))

;; Agenda helper

(defun org-life-agenda-float-to-string (float &optional face use-percent)
  (let ((text (cond
               ((stringp float) float)
               (use-percent (format "%.0f%%" (* 100 float)))
               (t (format "%.2f" float)))))
    (if face
        (propertize text 'face face)
      text)))

(defun org-life-agenda-float-to-number (float)
  (if (numberp float)
      float
    (cond ((string-equal float "inf") 1.0e+INF)
          ((string-equal float "-inf") -1.0e+INF)
          (t 0.0e+NaN))))

(defun org-life-agenda-days-to-string (integer)
  (if (numberp integer)
      (format "%dd" integer)
    ""))

(defun org-life-agenda-short-duration (minutes)
  (if minutes
      (let ((duration (org-duration-from-minutes minutes)))
        (car (split-string duration " ")))
    ""))

(defun org-life-agenda-sort-by-priority (a b)
  (let ((pa (or (org-life-agenda-entry-priority a) org-lowest-priority))
        (pb (or (org-life-agenda-entry-priority b) org-lowest-priority)))
    (< pa pb)))

(defun org-life-agenda-flatten-list (l)
  (cond ((not l) nil)
        ((atom l) (list l))
        (t (append (org-life-agenda-flatten-list (car l)) (org-life-agenda-flatten-list (cdr l))))))

(defun org-life-agenda-timestamp-to-date-string (timestamp &optional default)
  (or (and timestamp
           (org-timestamp-format timestamp "%Y-%m-%d"))
      default))

(defun org-life-agenda-time-to-date-string (time)
  (format-time-string "%Y-%m-%d" time))

(defun org-life-case-fold-string= (a b)
  (eq t (compare-strings a nil nil b nil nil t)))

(defun org-life-agenda-today-date-string ()
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
  "Get a buffer visiting FILE, in org-mode."
  (let ((buf (org-find-base-buffer-visiting file)))
    (unless buf
      (setq buf (find-file-noselect file)))
    (unless (eq (buffer-local-value 'major-mode buf)
                'org-mode)
      (message (prin1-to-string buf))
      (with-current-buffer buf
        (org-mode)))
    buf))

(defun org-life-echo (string &rest args)
  (let ((message-log-max
         (when org-life-echo-store-messages message-log-max)))
    (apply #'message string args)))

(defmacro org-life-with-config-buffer (&rest body)
  `(with-current-buffer
       (org-life-get-buffer org-life-config-file-path)
     (org-with-wide-buffer
      ,@body)))

(defun org-life-deep-copy (object)
  (if (listp object)
      (mapcar #'org-life-deep-copy
              object)
    object))

(defun org-life-nested-plist-get (plist prop-path)
  (if prop-path
      (org-life-nested-plist-get (plist-get plist (car prop-path)) (cdr prop-path))
    plist))

(defun org-life-nested-plist-put (plist prop-path value)
  (if prop-path
      (plist-put plist
                 (car prop-path)
                 (org-life-nested-plist-put
                  (plist-get plist (car prop-path))
                  (cdr prop-path)
                  value))
    value))

(defun org-life-time-diff-minutes (t1 t2)
  (let* ((t1 (time-to-seconds t1))
         (t2 (time-to-seconds t2)))
    (round (/ (- t1 t2) 60))))

(defun org-life-end-of-day (time)
  (let* ((time (apply 'encode-time
                      (append '(0 0 0)
                              (cdddr (decode-time time))))))
    (time-add time (days-to-time 1))))

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
  effort
  stressless
  urgency
  marker)

(defun org-life-agenda-entry-from-headline (id headline-elem &optional given-tags)
  (let ((todo-type (org-element-property :todo-type headline-elem))
        (todo-keyword (org-element-property :todo-keyword headline-elem))
        (priority (org-element-property :priority headline-elem))
        (text (org-element-property :raw-value headline-elem))
        (tags (org-element-property :tags headline-elem))
        (scheduled (org-element-property :scheduled headline-elem))
        (deadline (org-element-property :deadline headline-elem))
        (effort (org-element-property :EFFORT headline-elem))
        (stressless (org-element-property :STRESSLESS headline-elem))
        (urgency (org-element-property :URGENCY headline-elem))
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
     :effort (and effort (org-duration-to-minutes effort))
     :stressless stressless
     :urgency (or urgency "")
     :marker (org-agenda-new-marker begin))))

(defun org-life-agenda-parse-clock (task-id clock-elem)
  (let* ((timestamp (org-element-property :value clock-elem))
         (start (org-timestamp-to-time timestamp))
         (end (if (eq (org-element-property :status clock-elem) 'running)
                  (current-time)
                (org-timestamp-to-time timestamp t)))
         (session-data-list nil))

    (when (time-less-p start end)
      (let ((start-days (time-to-days start))
            (end-days (time-to-days end)))
        (if (= start-days end-days)
            (progn
              (push (cons start
                          (org-life-time-diff-minutes end start))
                    session-data-list))
          
          (while (< start-days end-days)
            (let ((end-of-day (org-life-end-of-day start)))
              (push (cons start
                          (org-life-time-diff-minutes end-of-day start))
                    session-data-list)
              (setq start end-of-day
                    start-days (1+ start-days))))
          (push (cons start
                      (org-life-time-diff-minutes end start))
                session-data-list)))
      
      (mapcar (lambda (session-data)
                (list :date (org-life-agenda-time-to-date-string
                             (car session-data))
                      :session (list :id task-id
                                     :amount (cdr session-data)
                                     :type
                                     org-life--session-type-enum-task
                                     :weakness
                                     org-life--session-weakness-enum-strong)))
              session-data-list))))

(defun org-life-agenda-get-tasks-and-clocks (headline-elem)
  "Writes result into `org-life--temp-tasks' and `org-life--temp-clocks'."

  (when (and (org-element-property :todo-type headline-elem)
             (not (org-element-property :NO_SCHEDULING headline-elem)))
    ;; id
    (setq org-life--temp-id (1+ org-life--temp-id))

    ;; tasks
    (push
     (org-life-agenda-entry-from-headline org-life--temp-id headline-elem)
     org-life--temp-tasks)

    ;; clocks
    (org-element-map
        (org-element-contents headline-elem) ; data
        'clock ; types
      (lambda (clock-elem) ; fun
        (let ((clock (org-life-agenda-parse-clock
                      org-life--temp-id
                      clock-elem)))
          (setq org-life--temp-clocks
                (append clock
                        org-life--temp-clocks))))
      nil ; info
      nil ; first-match
      'headline ; no-recursion
      ))
  
  ;; recurse
  (when (not (org-element-property :NO_SCHEDULING_CHILDREN headline-elem))
    (org-element-map
        (org-element-contents headline-elem) ; data
        'headline ; types
      #'org-life-agenda-get-tasks-and-clocks ; fun
      nil ; info
      nil ; first-match
      'headline ; no-recursion
      )))

(defun org-life-agenda-process-agenda-files (initial-value
                                             ast-processor
                                             &optional granularity)
  (let ((files (org-agenda-files nil 'ifmode))
        (acc initial-value))
    
    (while (setq file (pop files))
      (org-check-agenda-file file)
      (setq buffer (if (file-exists-p file)
                       (org-get-agenda-file-buffer file)
                     (error "No such file %s" file)))

      (setq org-life--last-org-buffer buffer)

      (with-current-buffer buffer
        (org-with-wide-buffer
         (unless (derived-mode-p 'org-mode) (error "Agenda file %s is not in Org mode" file))
         (let ((ast (progn
                      (org-life-echo "Parsing file %s ..." file)
                      (org-element-parse-buffer granularity))))
           (org-life-echo "Running AST processor for file %s ..." file)
           (setq acc (funcall ast-processor acc ast))))))
    
    acc))

(defun org-life-agenda-get-config-elem ()
  (org-life-with-config-buffer
   (unless (derived-mode-p 'org-mode) (error "Config file %s is not in Org mode" (buffer-file-name)))
   (org-element-map
       (org-element-parse-buffer) ; data
       'headline ; types
     #'identity;; fun
     nil ; info
     t ; first-match
     )))

(defun org-life-agenda-get-config-data (config-elem)
  "TODO actually allow config"
  (let* ((config-data (org-life-deep-copy org-life-default-config))
         (properties
          (macrolet ((make-prop (org-prop config-prop-path val)
                                `(-when-let (val
                                             (org-element-property
                                              ,org-prop
                                              config-elem))
                                   (cons ,config-prop-path
                                         ,val))))
            (list
             (make-prop :SHOW_DEBUG_MESSAGES
                        '(:show_debug_messages)
                        (and val (not (string= val "nil")) t))
             (make-prop :SCHEDULING_DAYS
                        '(:scheduling_days)
                        (string-to-number val))
             (make-prop :DAILY_INFO_DAYS
                        '(:daily_info_days)
                        (string-to-number val))
             (make-prop :RANDOM_POWER
                        '(:random_power)
                        (string-to-number val))
             (make-prop :DEFAULT_URGENCY
                        '(:default_urgency)
                        (string-to-number val))
             (make-prop :FRAGMENTATION_CONFIG.MIN_EXTRA_TIME_RATIO
                        '(:fragmentation_config :min_extra_time_ratio)
                        (string-to-number val))
             (make-prop :FRAGMENTATION_CONFIG.MAX_PERCENTAGE
                        '(:fragmentation_config :max_percentage)
                        (string-to-number val))
             (make-prop :FRAGMENTATION_CONFIG.PREFERRED_FRAGMENT_SIZE
                        '(:fragmentation_config :preferred_fragment_size)
                        (org-duration-to-minutes val))
             (make-prop :FRAGMENTATION_CONFIG.MIN_FRAGMENT_SIZE
                        '(:fragmentation_config :min_fragment_size)
                        (org-duration-to-minutes val))))))

    (dolist (property properties)
      (when property
        (setq config-data
              (org-life-nested-plist-put config-data
                                         (car property)
                                         (cdr property)))))
    (setq config-data
          (append (list :today (org-life-agenda-today-date-string))
                  config-data))
    config-data))

(defun org-life-agenda-get-usable-time-config (config-elem)
  (org-life-with-config-buffer
   (let* ((usable-time-config
           org-life-default-usable-time-config)
          (usable-time-drawer
           (org-element-map
               (org-element-contents config-elem) ; data
               'drawer ; types
             (lambda (child) ; fun
               (when (org-life-case-fold-string= (org-element-property :drawer-name child) "usable_time")
                 child))
             nil ; info
             t ; first-match
             )))
     (when usable-time-drawer
       (let* ((entries
               (org-element-map
                   (org-element-contents usable-time-drawer)
                   'item
                 (lambda (item)
                   (let* ((contents
                           (org-element-contents item))
                          (paragraph
                           (org-element-map
                               contents
                               'paragraph
                             #'identity
                             nil
                             t))
                          (paragraph-text
                           (if paragraph
                               (buffer-substring-no-properties
                                (org-element-property
                                 :contents-begin
                                 paragraph)
                                (org-element-property
                                 :contents-end
                                 paragraph))
                             ""))
                          (selector-text
                           (progn
                             (string-match "^\\([^\n]*\\)\n.*"
                                           paragraph-text)
                             (string-trim (match-string
                                           1
                                           paragraph-text))))
                          (duration-text
                           (progn
                             (string-match ".*\n\\([^\n]*\\)$"
                                           paragraph-text)
                             (string-trim (match-string
                                           1
                                           paragraph-text))))
                          (selector
                           (cond
                            ((string-match
                              "\\([0-9]+-[0-9]+-[0-9]+\\).*--\\[\\([0-9]+-[0-9]+-[0-9]+\\)"
                              selector-text)
                             (concat
                              (string-trim (match-string 1 selector-text))
                              " - "
                              (string-trim (match-string 2 selector-text))))
                            ((string-match "[0-9]+-[0-9]+-[0-9]+" selector-text)
                             (string-trim (match-string 0 selector-text)))
                            (t
                             selector-text)))
                          (duration
                           (org-duration-to-minutes duration-text)))
                     
                     (list :selector selector
                           :duration duration)))
                 nil
                 nil)))
         (setq usable-time-config
               (append usable-time-config
                       entries))))
     usable-time-config)))

(defun org-life-agenda-get-agenda-data ()
  (if org-life--agenda-data-cache
      ;; Return cache if non-nil.
      org-life--agenda-data-cache

    (org-life-echo "Computing agenda data ...")
    
    (setq org-life--temp-id 0)
    (setq org-life--temp-tasks nil)
    (setq org-life--temp-clocks nil)

    (org-life-echo "Reading agenda files ...")
    
    (org-life-agenda-process-agenda-files
     '()
     (lambda (acc ast) ; ast-processor
       (org-element-map
           ast ; data
           'headline ; types
         #'org-life-agenda-get-tasks-and-clocks ; fun
         nil ; info
         nil ; first-match
         'headline ; no-recursion
         ))
     'element)
    
    (org-life-echo "Gathering data ...")
    
    (setq
     org-life--agenda-data-cache
     (let* ((config-elem
             (org-life-agenda-get-config-elem))
            (config-data
             (org-life-agenda-get-config-data config-elem))
            (tasks
             org-life--temp-tasks)
            (clocks
             org-life--temp-clocks)
            (usable-time-config
             (org-life-agenda-get-usable-time-config config-elem)))
       
       (list :config config-data
             :usable-time usable-time-config
             :tasks tasks
             :clocks clocks
             :tasks-dict (org-life-agenda-list-to-ht
                          tasks
                          (lambda (task)
                            (org-life-agenda-entry-id task))))))))

(defun org-life-agenda-get-scheduler-request-config (config-data)
  ;; This is in the same format as request.
  config-data)

(defun org-life-agenda-get-scheduler-request-tasks (agenda-tasks)
  "TODO deal with repeats. deal with non-scheduled tasks. deal with non-effort tasks. deal with amount done. deal with priority."
  (-non-nil
   (mapcar
    (lambda (task)
      (when (org-life-agenda-entry-todo-type task)
        (list
         :id (org-life-agenda-entry-id task)
         :start (org-life-agenda-timestamp-to-date-string
                 (org-life-agenda-entry-scheduled task)
                 "min")
         :end (org-life-agenda-timestamp-to-date-string
               (org-life-agenda-entry-deadline task)
               "max")
         :amount (org-life-agenda-entry-effort task)
         :stressless (org-life-agenda-entry-stressless task)
         :urgency (org-life-agenda-entry-urgency task)
         :done 0
         :status (cond
                  ((eq 'done
                       (org-life-agenda-entry-todo-type task))
                   org-life--task-status-enum-done)
                  (t org-life--task-status-enum-todo))
         :priority (- (or (org-life-agenda-entry-priority task)
                          org-lowest-priority)
                      org-highest-priority)
         :repeat (let* ((timestamp
                         (org-life-agenda-entry-deadline task))
                        (repeater-type
                         (org-element-property :repeater-type timestamp))
                        (repeater-unit
                         (org-element-property :repeater-unit timestamp))
                        (repeater-value
                         (org-element-property :repeater-value timestamp)))
                   (when repeater-unit
                     (list
                      :type (cond
                             ((eq repeater-type 'restart)
                              org-life--task-repeat-type-enum-restart)
                             (t
                              org-life--task-repeat-type-enum-normal))
                      :unit (cond
                             ((eq repeater-unit 'day)
                              org-life--task-repeat-unit-enum-day)
                             ((eq repeater-unit 'week)
                              org-life--task-repeat-unit-enum-week)
                             ((eq repeater-unit 'month)
                              org-life--task-repeat-unit-enum-month)
                             ((eq repeater-unit 'year)
                              org-life--task-repeat-unit-enum-year)
                             (t
                              org-life--task-repeat-unit-enum-none))
                      :value (or repeater-value 1)))))))
    agenda-tasks)))

(defun org-life-agenda-get-scheduler-request-usable-time (usable-time-config)
  ;; This is in the same format as request.
  usable-time-config)

(defun org-life-agenda-get-scheduler-request (agenda-data)
  "TODO"
  (list
   :command "schedule"
   :args (list
          :config (org-life-agenda-get-scheduler-request-config
                   (plist-get agenda-data :config))
          :tasks (org-life-agenda-get-scheduler-request-tasks
                  (plist-get agenda-data :tasks))
          :dated_sessions (plist-get agenda-data :clocks)
          :usable_time (org-life-agenda-get-scheduler-request-usable-time
                        (plist-get agenda-data :usable-time)))))

(defun org-life-agenda-get-schedule-data ()
  (if org-life--schedule-data-cache
      ;; Return cache if non-nil.
      org-life--schedule-data-cache

    (setq
     org-life--schedule-data-cache
     (let (request
           response)

       (setq org-life--last-updated-time (current-time))

       (org-life-echo "Preparing server request ...")
       (setq request (org-life-agenda-get-scheduler-request
                      (org-life-agenda-get-agenda-data)))
       
       (org-life-echo "Sending server request ...")
       (setq response (org-life-send-request request))))))

(defun org-life-invalidate-cache ()
  (setq org-life--agenda-data-cache nil)
  (setq org-life--schedule-data-cache nil))

;; Agenda renderer

(defun org-life-agenda-render-view (view)
  (let ((inhibit-read-only t)
        (agenda-data (org-life-agenda-get-agenda-data))
        (schedule-data (org-life-agenda-get-schedule-data)))

    (if (stringp schedule-data) ; error in request
        (org-life-agenda-render-error
         :message schedule-data)

      (when (and org-life--last-org-buffer
                 (null org-todo-regexp))
        (dolist (variable '(org-todo-regexp
                            org-not-done-regexp
                            org-complex-heading-regexp
                            org-done-keywords
                            org-done-keywords-for-agenda))
          (set variable (buffer-local-value variable org-life--last-org-buffer))))

      (goto-char (point-max))
      (cond

       ((eq view 'main)
        (org-life-agenda-render-agenda
         :agenda-data agenda-data
         :schedule-data schedule-data))

       ((eq view 'task-list)
        (org-life-agenda-render-task-list
         :agenda-data agenda-data
         :schedule-data schedule-data))))))

(cl-defun org-life-agenda-render-entry (&key (prefix " ")
                                             entry
                                             (face nil)
                                             (session-duration nil)
                                             (overlay-face nil))
  (insert
   (let ((props (list 'face face
                      'mouse-face 'highlight
                      'undone-face 'nil
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
  (when session-duration
    (save-excursion
      (forward-line -1)
      (let ((line-height (if (< session-duration 30) 1.0 (+ 0.5 (/ session-duration 60))))
            (ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
        (when overlay-face
          (overlay-put ov 'face overlay-face))
        (overlay-put ov 'line-height line-height)
        (overlay-put ov 'line-spacing (1- line-height))))))

(cl-defun org-life-agenda-render-block-separator ()
  (unless (or (bobp) org-agenda-compact-blocks
              (not org-agenda-block-separator))
    (insert "\n"
            ;; (if (stringp org-agenda-block-separator)
            ;;     org-agenda-block-separator
            ;;   (make-string (window-text-width) org-agenda-block-separator))
            ;; "\n"
            )))

(cl-defun org-life-agenda-render-block-sub-separator ()
  (insert "\n"))

(cl-defun org-life-agenda-render-multi-progress-bar
    (&key bar-width
          progress-list
          marker-list
          (fill-char ?=)
          (empty-char ? )
          (left-border-char ?|)
          (right-border-char ?|)
          (min-value 0.0)
          (max-value 1.0))
  (insert (char-to-string left-border-char))
  
  (let ((bar-width (max 1 (- bar-width 2)))
        (cur-progress 0)
        (cur-width 0))
    
    (while progress-list
      (let* ((progress (/ (- (org-life-agenda-float-to-number (caar progress-list))
                             min-value)
                          (- max-value min-value)))
             (progress (min (max progress 0.0) 1.0))
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
      (insert (propertize (make-string diff empty-char)
                          'face 'org-life-agenda-empty-progress-face)))

    (dolist (marker marker-list)
      (let* ((pos (/ (- (org-life-agenda-float-to-number (car marker))
                        min-value)
                     (- max-value min-value)))
             (pos (min (max pos 0.0) 1.0))
             (face (cadr marker))
             (marker-char (caddr marker)))
        (save-excursion
          (backward-char)
          (goto-char (- (point)
                        (round (* (- 1.0 (float pos))
                                  (1- bar-width)))))
          (delete-char 1)
          (insert (propertize (char-to-string marker-char)
                              'face face))))))
  
  (insert (char-to-string right-border-char)))

(cl-defun org-life-agenda-render-entries (&key entries)
  (setq entries (sort entries #'org-life-agenda-sort-by-priority))
  (dolist (entry entries)
    (org-life-agenda-render-entry :prefix " "
                                  :entry entry)))

(cl-defun org-life-agenda-render-impossible-task (&key alert-entry
                                                       tasks-dict)
  (let* ((id (plist-get alert-entry :id))
         (amount (plist-get alert-entry :amount))
         (entry (ht-get tasks-dict id)))
    (org-life-agenda-render-entry
     :prefix (concat "| "
                     (propertize
                      (format "%-8s" (org-duration-from-minutes amount))
                      'face 'error)
                     " | ")
     :entry entry)))

(cl-defun org-life-agenda-render-bad-estimate-task (&key alert-entry
                                                         tasks-dict)
  (let* ((id (plist-get alert-entry :id))
         (amount (plist-get alert-entry :amount))
         (done (plist-get alert-entry :done))
         (entry (ht-get tasks-dict id)))
    (org-life-agenda-render-entry
     :prefix (concat "| "
                     (propertize
                      (format "%s/%s"
                              (org-duration-from-minutes done)
                              (org-duration-from-minutes amount))
                      'face 'error)
                     " | ")
     :entry entry)))

(cl-defun org-life-agenda-render-bad-info-task (&key alert-entry
                                                     tasks-dict)
  (let* ((id (plist-get alert-entry :id))
         (reason (plist-get alert-entry :reason))
         (entry (ht-get tasks-dict id)))
    (org-life-agenda-render-entry
     :prefix (format "| %s | " reason)
     :entry entry)))

(cl-defun org-life-agenda-render-overdue-task (&key alert-entry
                                                     tasks-dict)
  (let* ((id (plist-get alert-entry :id))
         (days (plist-get alert-entry :days))
         (entry (ht-get tasks-dict id)))
    (org-life-agenda-render-entry
     :prefix (format "| %4s | " (org-life-agenda-days-to-string days))
     :entry entry)))

(cl-defun org-life-agenda-render-schedule-legend ()
  (insert (format "| %-5s | %5s | %4s | %s"
                  "Dur." "Rem." "Due" "Task")
          "\n"))

(cl-defun org-life-agenda-render-sessions (&key sessions
                                                tasks-dict)
  (let ((overlay-face-alt nil))
   (dolist (session sessions)
    (let* ((id (plist-get session :id))
           (amount (plist-get session :amount))
           (type (plist-get session :type))
           (weakness (plist-get session :weakness))
           (to-deadline (plist-get session :to_deadline))
           (to-finish (plist-get session :to_finish))
           (entry (ht-get tasks-dict id)))
      (org-life-agenda-render-entry
       :prefix (format "| %-5s | %5s | %4s | "
                       (org-duration-from-minutes amount)
                       (if (and (numberp to-finish)
                                (= to-finish 0))
                           "✓" ; "Final"
                         (org-life-agenda-short-duration to-finish))
                       (if (and (numberp to-deadline)
                                (= to-deadline 0))
                           "!!!" ; "Due"
                         (org-life-agenda-days-to-string to-deadline))
                       ;; (format "%-5s|" (make-string
                       ;;                 (round (* (min 1.0 lateness) 5))
                       ;;                 ?=))
                       )
       :entry entry
       :face (cond ((and (numberp to-deadline)
                         (< to-deadline 0))
                    'org-warning)
                   ((= weakness
                       org-life--session-weakness-enum-strong)
                    'org-agenda-done)
                   ((= type
                       org-life--session-type-enum-fragment)
                    'org-time-grid)
                   ((= type
                       org-life--session-type-enum-overlimit)
                    'org-warning)
                   (t nil))
       :session-duration amount
       :overlay-face (if overlay-face-alt
                         'org-life-agenda-session-face-2
                       'org-life-agenda-session-face-1)))
    (setq overlay-face-alt (not overlay-face-alt)))))

(cl-defun org-life-agenda-render-day (&key daily-info
                                           tasks-dict
                                           (today nil)
                                           (today-optimal-sessions nil))
  (let* ((date-string (plist-get daily-info :date))
         (usable-time (plist-get daily-info :usable_time))
         (actual-usable-time (plist-get daily-info :actual_usable_time))
         (sessions (plist-get daily-info :sessions))
         (free-time (plist-get daily-info :free_time))
         (average-stress (plist-get daily-info :average_stress))
         (average-etr (plist-get daily-info :average_etr))
         (time (org-life-agenda-date-string-to-time date-string))
         (weekday (org-life-agenda-time-to-weekday time))
         (date-string (if today
                          (propertize date-string
                                      'face
                                      'org-agenda-date-today)
                        (propertize date-string
                                    'face
                                    'org-agenda-date)))
         (weekday-num (cdr weekday))
         (weekday-text (propertize
                        (format "● %d-%s"
                                weekday-num
                                (car weekday))
                        'face
                        (if (or (= weekday-num 6)
                                (= weekday-num 7))
                            'org-agenda-date-weekend
                          'org-agenda-date))))
    (insert weekday-text " " date-string "  "
            (propertize
             (concat (org-duration-from-minutes actual-usable-time) "/"
                     (org-duration-from-minutes usable-time))
             'face 'org-life-agenda-secondary-face)
            " ")
    (org-life-agenda-render-multi-progress-bar
     :bar-width 50
     :progress-list (list (cons (/ (float actual-usable-time) 1440.0)
                                'org-life-agenda-actual-usable-time-face)
                          (cons (/ (float usable-time) 1440.0)
                                'org-life-agenda-usable-time-face))
     :fill-char org-life-agenda-progress-fill-char
     :empty-char org-life-agenda-progress-empty-char
     :right-border-char ? )
    (insert "\n")
    (org-life-agenda-render-sessions :sessions sessions
                                     :tasks-dict tasks-dict)
    (insert (propertize
             (format "| Maximum Free Time: %5s | Extra Time Ratio: %5s | Stress: %5s |"
                     (org-duration-from-minutes free-time)
                     (org-life-agenda-float-to-string average-etr)
                     (org-life-agenda-float-to-string average-stress))
             'face 'org-life-agenda-secondary-face)
            "\n")
    (when (and today today-optimal-sessions)
      (insert "\n")
      (org-life-agenda-render-block-sub-title :title "Optimal")
      (org-life-agenda-render-sessions :sessions today-optimal-sessions
                                       :tasks-dict tasks-dict))))

(cl-defun org-life-agenda-render-block-title (&key title)
  (insert (org-add-props title nil 'face 'org-agenda-structure) "\n"))

(cl-defun org-life-agenda-render-block-sub-title (&key title)
  (insert (org-add-props title nil 'face 'bold) "\n"))

;; (cl-defun org-life-agenda-render-block (&key entries title (entries-renderer nil))
;;   (when entries
;;     (let ((begin (point))
;;           (entries-renderer (or entries-renderer
;;                                 #'org-life-agenda-render-entries)))
;;       (org-life-agenda-render-block-separator)
;;       (org-life-agenda-render-block-title title)
;;       (funcall entries-renderer :entries entries)
;;       (add-text-properties begin (point-max) `(org-agenda-type tags)))))

(cl-defun org-life-agenda-render-error (&key message)
  (insert (propertize "Error" 'face 'error)
          "\n\n"
          message))

;; TODO: Bug: last updated will not reflect true update time using the new view system.
(cl-defun org-life-agenda-render-last-updated ()
  (insert (propertize
           (format-time-string "Last Updated: %Y-%m-%d %H:%M\n"
                               org-life--last-updated-time)
           'face 'org-life-agenda-secondary-face)))

(cl-defun org-life-agenda-render-debug (&key message)
  (when message
    (org-life-agenda-render-block-title :title "Debug Messages")
    (insert (prin1-to-string message) "\n")))

(cl-defun org-life-agenda-render-general-info (&key general-info
                                                    tasks-dict)
  (let ((stress (plist-get general-info :stress))
        (pof (plist-get general-info :pof))
        (workload (plist-get general-info :workload))
        (highest-stress-date (plist-get general-info :highest_stress_date))
        (highest-stress-task (plist-get general-info :highest_stress_task))
        (stress-with-optimal (plist-get general-info :stress_with_optimal))
        (stress-with-suggested (plist-get general-info :stress_with_suggested))
        (stress-without-today (plist-get general-info :stress_without_today))
        (etr-with-optimal (plist-get general-info :etr_with_optimal))
        (etr-with-suggested (plist-get general-info :etr_with_suggested))
        (etr-without-today (plist-get general-info :etr_without_today))
        (pof-with-optimal (plist-get general-info :pof_with_optimal))
        (pof-with-suggested (plist-get general-info :pof_with_suggested))
        (pof-without-today (plist-get general-info :pof_without_today))
        (workload-with-optimal (plist-get general-info :workload_with_optimal))
        (workload-with-suggested (plist-get general-info :workload_with_suggested))
        (workload-without-today (plist-get general-info :workload_without_today)))
    (insert (format "Stress: %5s | Failure Probability: %5s | Workload: %5s"
                    (propertize (org-life-agenda-float-to-string stress)
                                'face 'bold)
                    (propertize (org-life-agenda-float-to-string pof)
                                'face 'bold)
                    (propertize (org-life-agenda-float-to-string workload nil t)
                                'face 'bold))
            "\n")
    (let (value-1 value-2 value-3 marker-list min-value max-value)
      (if org-life-agenda-visualize-probability
          (setq value-1 pof-with-optimal
                value-2 pof-with-suggested
                value-3 pof-without-today
                marker-list (list (list 0.25 'org-life-agenda-secondary-face ?|)
                                  (list 0.5 'org-life-agenda-secondary-face ?|)
                                  (list 0.75 'org-life-agenda-secondary-face ?|)
                                  (list pof nil ?+))
                min-value 0.0
                max-value 1.0)
        (setq value-1 workload-with-optimal
              value-2 workload-with-suggested
              value-3 workload-without-today
              marker-list (list (list 0.25 'org-life-agenda-secondary-face ?|)
                                (list 0.5 'org-life-agenda-secondary-face ?|)
                                (list 0.75 'org-life-agenda-secondary-face ?|)
                                (list 1.0 'org-life-agenda-secondary-face ?!)
                                (list workload nil ?+))
              min-value 0.0
              max-value 1.5))
      (org-life-agenda-render-multi-progress-bar
       :bar-width (window-text-width)
       :progress-list (list (cons value-1
                                  'org-life-agenda-stress-best-face)
                            (cons value-2
                                  'org-life-agenda-stress-normal-face)
                            (cons value-3
                                  'org-life-agenda-stress-warning-face))
       :marker-list marker-list
       :fill-char org-life-agenda-progress-fill-char
       :empty-char org-life-agenda-progress-empty-char
       :min-value min-value
       :max-value max-value))
    (insert "\n")
    (insert (format "| %-20s %10s | %10s | %10s |"
                    (propertize "Today's Schedule:"
                                'face 'bold)
                    "Optimal"
                    "Suggested"
                    "Worst")
            "\n")
    (insert (format "| %-20s %10s | %10s | %10s |"
                    "Failure Probability:"
                    (org-life-agenda-float-to-string
                     pof-with-optimal
                     'org-life-agenda-stress-best-face)
                    (org-life-agenda-float-to-string
                     pof-with-suggested
                     'org-life-agenda-stress-normal-face)
                    (org-life-agenda-float-to-string
                     pof-without-today
                     'org-life-agenda-stress-warning-face))
            "\n")
    (insert (format "| %-20s %10s | %10s | %10s |"
                    "Workload:"
                    (org-life-agenda-float-to-string
                     workload-with-optimal
                     'org-life-agenda-stress-best-face t)
                    (org-life-agenda-float-to-string
                     workload-with-suggested
                     'org-life-agenda-stress-normal-face t)
                    (org-life-agenda-float-to-string
                     workload-without-today
                     'org-life-agenda-stress-warning-face t))
            "\n")
    ;; (insert (format "| %-20s %10s | %10s | %10s |"
    ;;                 "Extra Time Ratio:"
    ;;                 (org-life-agenda-float-to-string
    ;;                  stress-with-optimal
    ;;                  'org-life-agenda-stress-best-face)
    ;;                 (org-life-agenda-float-to-string
    ;;                  stress-with-suggested
    ;;                  'org-life-agenda-stress-normal-face)
    ;;                 (org-life-agenda-float-to-string
    ;;                  stress-without-today
    ;;                  'org-life-agenda-stress-warning-face))
    ;;         "\n")
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
            "\n")
    (when highest-stress-task
      (let ((entry (ht-get tasks-dict highest-stress-task)))
        (org-life-agenda-render-entry :prefix "Highest Stress Task: "
                                      :entry entry)))))

(cl-defun org-life-agenda-render-alert-entries (&key title
                                                     alert-entries
                                                     renderer
                                                     tasks-dict
                                                     extra-info-renderer)
  (when (> (length alert-entries) 0)
    (org-life-agenda-render-block-sub-separator)
    (org-life-agenda-render-block-sub-title :title
                                            (format "%s (%d)"
                                                    title
                                                    (length alert-entries)))
    (dolist (alert-entry alert-entries)
      (funcall renderer
               :alert-entry alert-entry
               :tasks-dict tasks-dict))
    (when extra-info-renderer
      (funcall extra-info-renderer
               :alert-entries alert-entries))))

(cl-defun org-life-agenda-render-impossible-task-extra-info
    (&key alert-entries)
  (let ((sum (-reduce-from (lambda (acc entry)
                             (+ acc (plist-get entry :amount)))
                           0 alert-entries)))
    (insert (format "Total Duration: %s\n" (org-duration-from-minutes sum)))))

(cl-defun org-life-agenda-render-alerts (&key alerts
                                              tasks-dict)
  (let ((impossible-tasks (plist-get alerts :impossible_tasks))
        (bad-estimate-tasks (plist-get alerts :bad_estimate_tasks))
        (bad-info-tasks (plist-get alerts :bad_info_tasks))
        (overdue-tasks (plist-get alerts :overdue_tasks)))
    
    (org-life-agenda-render-alert-entries :title "Impossible Tasks"
                                          :alert-entries impossible-tasks
                                          :renderer #'org-life-agenda-render-impossible-task
                                          :tasks-dict tasks-dict
                                          :extra-info-renderer
                                          #'org-life-agenda-render-impossible-task-extra-info)

    (org-life-agenda-render-alert-entries :title "Tasks with Bad Estimate"
                                          :alert-entries bad-estimate-tasks
                                          :renderer #'org-life-agenda-render-bad-estimate-task
                                          :tasks-dict tasks-dict)

    
    (org-life-agenda-render-alert-entries :title "Tasks with Bad Info"
                                          :alert-entries bad-info-tasks
                                          :renderer #'org-life-agenda-render-bad-info-task
                                          :tasks-dict tasks-dict)

    (org-life-agenda-render-alert-entries :title "Overdue Tasks"
                                          :alert-entries overdue-tasks
                                          :renderer #'org-life-agenda-render-overdue-task
                                          :tasks-dict tasks-dict)))

(cl-defun org-life-agenda-render-agenda (&key agenda-data
                                              schedule-data)
  (org-life-echo "Rendering agenda ...")
  
  (let ((status (plist-get schedule-data :status))
        (err (plist-get schedule-data :error))
        (data (plist-get schedule-data :data)))
    (if (string= status "error")
        (org-life-agenda-render-error :message err)
      (let ((tasks-dict (plist-get agenda-data :tasks-dict))
            (general-info (plist-get data :general))
            (alerts (plist-get data :alerts))
            (daily-infos (plist-get data :daily_infos))
            (today-optimal-sessions (plist-get data :today_optimal_sessions))
            (debug (plist-get data :debug))

            (today t))

        (org-life-agenda-render-debug :message debug)
        
        (org-life-agenda-render-last-updated)
        (org-life-agenda-render-block-sub-separator)
        
        (org-life-agenda-render-block-title :title "General")
        (org-life-agenda-render-general-info :general-info general-info
                                             :tasks-dict tasks-dict)
        
        (org-life-agenda-render-block-separator)
        
        (org-life-agenda-render-block-title :title "Alerts")
        (org-life-agenda-render-alerts :alerts alerts
                                       :tasks-dict tasks-dict)
        
        (org-life-agenda-render-block-separator)
        
        (org-life-agenda-render-block-title :title "Schedules")
        ;; (org-life-agenda-render-schedule-legend)
        (dolist (daily-info daily-infos)
          (org-life-agenda-render-day :daily-info daily-info
                                      :tasks-dict tasks-dict
                                      :today today
                                      :today-optimal-sessions
                                      today-optimal-sessions)
          (insert "\n")
          (setq today nil))))))

(cl-defun org-life-agenda-render-task-list (&key agenda-data schedule-data)
  (org-life-echo "Rendering task list ...")
  (let ((status (plist-get schedule-data :status))
        (err (plist-get schedule-data :error))
        (data (plist-get schedule-data :data)))
    (if (string= status "error")
        (org-life-agenda-render-error :message err)
      
      (let* ((tasks-dict (plist-get agenda-data :tasks-dict))
             (general-info (plist-get data :general))
             (alerts (plist-get data :alerts))
             (daily-infos (plist-get data :daily_infos))
             (task-infos (plist-get data :task_infos))
             (task-infos
              (sort (copy-sequence task-infos)
                    (lambda (a b)
                      (<
                       (org-life-agenda-float-to-number
                        (plist-get a :current_score))
                       (org-life-agenda-float-to-number
                        (plist-get b :current_score))))))
             (debug (plist-get data :debug)))

      (org-life-agenda-render-debug :message debug)

      (org-life-agenda-render-last-updated)
      (org-life-agenda-render-block-sub-separator)

      (org-life-agenda-render-block-title :title "Task List")

      (insert (propertize (format "| %-8s | Task \n" "Score")
                          'face 'bold))

      (dolist (task-info task-infos)
        (let* ((id (plist-get task-info :id))
               (entry (ht-get tasks-dict id))
               (current-score (plist-get task-info :current_score)))
        (org-life-agenda-render-entry
         :prefix (format "| %-8s | "
                         (org-life-agenda-float-to-string
                          current-score))
         :entry entry)))))))

(defun org-life-agenda-show-view (view)
  (setq org-life--agenda-current-view view)
  (let ((org-life--agenda-keep-markers t)
        (org-life--agenda-keep-cache t))
    (org-agenda-redo t)
    (goto-char (point-min))))

;;; Interactive functions

(defun org-life-restart-engine ()
  "Start/Restart org-life scheduling engine."
  (interactive)
  (org-life-start-engine))

(defun ivy-org-life-agenda-show-view ()
  (interactive)
  ;; TODO: Add check for when not in agenda buffer
  (let* ((candidates
          (mapcar
           (lambda (item) (propertize (car item) 'property (cdr item)))
           '(("Main" . main) ("Task List" . task-list)))))
    (ivy-read
     "Select view: "
     candidates
     :action
     (lambda (str)
       (org-life-agenda-show-view (get-text-property 0 'property str))))))

(defun org-life-agenda-show-main ()
  (interactive)
  (org-life-agenda-show-view 'main))

(defun org-life-agenda-show-task-list ()
  (interactive)
  (org-life-agenda-show-view 'task-list))

;;; Advices

(defun org-life-org-agenda-reset-markers-advice (func &rest args)
	(unless org-life--agenda-keep-markers
    (apply func args)))

(advice-add #'org-agenda-reset-markers
            :around #'org-life-org-agenda-reset-markers-advice)

;;; Hooks



(provide 'org-life)

;;; org-life.el ends here
