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

(defcustom org-life-default-config
  (list
   :scheduling_days 365
   :daily_info_days 14
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

;;
;; Faces
;;

(defface org-life-agenda-secondary-face
  '((t (:inherit font-lock-comment-face)))
  "Face for secondary text (such as certain statistics) for org-life-agenda."
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

;;
;; Variables
;;

(defvar org-life--engine-process nil
  "Engine process.")

(defvar org-life--response nil
  "Temporarily stored engine responses.")

(defvar org-life--temp-id 0)
(defvar org-life--temp-tasks nil)
(defvar org-life--temp-clocks nil)
(defvar org-life--response-buffer "")

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
  (setq org-life--response-buffer
        (concat org-life--response-buffer
                output))
  (when (s-ends-with-p "\n" output)
    (setq org-life--response (org-life--decode org-life--response-buffer)
          org-life--response-buffer "")))

;; Agenda

;; Agenda helper

(defun org-life-agenda-ratio-to-string (ratio &optional face)
  (let ((text (if (stringp ratio)
                  ratio
                (format "%.2f" ratio))))
    (if face
        (propertize text 'face face)
      text)))

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
  "Get a buffer visiting FILE."
  (let ((buf (org-find-base-buffer-visiting file)))
    (if buf
        buf
      (setq buf (find-file-noselect file))
      buf)))

(defun org-life-echo (string &rest args)
  (let (message-log-max)
    (apply #'message string args)))

(defmacro org-life-with-config-buffer (&rest body)
  `(with-current-buffer
       (org-life-get-buffer ,org-life-config-file-path)
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

;; Agenda renderer

(cl-defun org-life-agenda-render-entry (&key (prefix " ")
                                             entry
                                             (face nil))
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
     (org-agenda-highlight-todo text))))

(cl-defun org-life-agenda-render-block-separator ()
  (unless (or (bobp) org-agenda-compact-blocks
              (not org-agenda-block-separator))
    (insert "\n"
            ;; (if (stringp org-agenda-block-separator)
            ;;     org-agenda-block-separator
            ;;   (make-string (window-text-width) org-agenda-block-separator))
            ;; "\n"
            )))

(cl-defun org-life-agenda-render-multi-progress-bar
    (&key bar-width
          progress-list
          marker-list
          (fill-char ?=)
          (empty-char ? )
          (marker-char ?|)
          (left-border-char ?|)
          (right-border-char ?|))
  (insert (char-to-string left-border-char))
  
  (let ((bar-width (max 1 (- bar-width 2)))
        (cur-progress 0)
        (cur-width 0))
    
    (while progress-list
      (let ((progress (min (max (caar progress-list) 0.0) 1.0))
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
      (let ((pos (min (max (car marker) 0.0) 1.0))
            (face (cdr marker)))
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

(cl-defun org-life-agenda-render-impossible-task (&key impossible-task
                                                       tasks-dict)
  (let* ((id (plist-get impossible-task :id))
         (amount (plist-get impossible-task :amount))
         (entry (ht-get tasks-dict id)))
    (org-life-agenda-render-entry
     :prefix (concat "| "
                     (propertize
                      (format "%-8s" (org-duration-from-minutes amount))
                      'face 'error)
                     " ")
     :entry entry)))

(cl-defun org-life-agenda-render-bad-estimate-task (&key bad-estimate-task
                                                         tasks-dict)
  (let* ((id (plist-get bad-estimate-task :id))
         (amount (plist-get bad-estimate-task :amount))
         (done (plist-get bad-estimate-task :done))
         (entry (ht-get tasks-dict id)))
    (org-life-agenda-render-entry
     :prefix (concat "| "
                     (propertize
                      (format "%s/%s"
                              (org-duration-from-minutes done)
                              (org-duration-from-minutes amount))
                      'face 'error)
                     " ")
     :entry entry)))

(cl-defun org-life-agenda-render-day (&key daily-info
                                           tasks-dict
                                           (today nil))
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
     ;; :marker-list (list (cons (/ 8.0 24.0) nil))
     :fill-char org-life-agenda-progress-fill-char
     :empty-char org-life-agenda-progress-empty-char
     :right-border-char ? )
    (insert "\n")
    (dolist (session sessions)
      (let* ((id (plist-get session :id))
             (amount (plist-get session :amount))
             (type (plist-get session :type))
             (weakness (plist-get session :weakness))
             (last (plist-get session :last))
             (lateness (plist-get session :lateness))
             (entry (ht-get tasks-dict id)))
        (org-life-agenda-render-entry
         :prefix (concat "| "
                         (format "%-5s" (org-duration-from-minutes amount))
                         " "
                         (format "%-5s|" (make-string
                                         (round (* (min 1.0 lateness) 5))
                                         ?=))
                         (if (eq last t)
                             (propertize "X "
                                         'face 'org-agenda-done)
                           "  "))
         :entry entry
         :face (cond ((= weakness 0) 'org-agenda-done)
                     ((= type 1) 'org-time-grid)
                     ((= type 2) 'org-warning)
                     (t nil)))))
    (insert (propertize
             (format "| Maximum Free Time: %5s | Extra Time Ratio: %5s | Stress: %5s |"
                     (org-duration-from-minutes free-time)
                     (org-life-agenda-ratio-to-string average-etr)
                     (org-life-agenda-ratio-to-string average-stress))
             'face 'org-life-agenda-secondary-face)
            "\n")))

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
          "\n\n"
          message))

(cl-defun org-life-agenda-render-debug (&key message)
  (when message
    (org-life-agenda-render-block-title :title "Debug Messages")
    (insert (prin1-to-string message) "\n")))

(cl-defun org-life-agenda-render-general-info (&key general-info)
  (let ((stress (plist-get general-info :stress))
        (highest-stress-date (plist-get general-info :highest_stress_date))
        (stress-with-optimal (plist-get general-info :stress_with_optimal))
        (stress-with-suggested (plist-get general-info :stress_with_suggested))
        (stress-without-today (plist-get general-info :stress_without_today))
        (etr-with-optimal (plist-get general-info :etr_with_optimal))
        (etr-with-suggested (plist-get general-info :etr_with_suggested))
        (etr-without-today (plist-get general-info :etr_without_today)))
    (org-life-agenda-render-multi-progress-bar
     :bar-width (window-text-width)
     :progress-list (list (cons stress-with-optimal
                                'org-life-agenda-stress-best-face)
                          (cons stress-with-suggested
                                'org-life-agenda-stress-normal-face)
                          (cons stress-without-today
                                'org-life-agenda-stress-warning-face))
     :marker-list (list (cons 0.5 nil)
                        (cons 0.25 nil))
     :fill-char org-life-agenda-progress-fill-char
     :empty-char org-life-agenda-progress-empty-char)
    (insert "\n")
    (insert (format "| %-20s %10s | %10s | %10s |"
                    (propertize "Analysis:"
                                'face 'bold)
                    "Optimal"
                    "Suggested"
                    "Worst")
            "\n")
    (insert (format "| %-20s %10s | %10s | %10s |"
                    "Stress:"
                    (org-life-agenda-ratio-to-string
                     stress-with-optimal
                     'org-life-agenda-stress-best-face)
                    (org-life-agenda-ratio-to-string
                     stress-with-suggested
                     'org-life-agenda-stress-normal-face)
                    (org-life-agenda-ratio-to-string
                     stress-without-today
                     'org-life-agenda-stress-warning-face))
            "\n")
    (insert (format "| %-20s %10s | %10s | %10s |"
                    "Extra Time Ratio:"
                    (org-life-agenda-ratio-to-string
                     etr-with-optimal
                     'org-life-agenda-stress-best-face)
                    (org-life-agenda-ratio-to-string
                     etr-with-suggested
                     'org-life-agenda-stress-normal-face)
                    (org-life-agenda-ratio-to-string
                     etr-without-today
                     'org-life-agenda-stress-warning-face))
            "\n")
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
  (let ((impossible-tasks (plist-get alerts :impossible_tasks))
        (bad-estimate-tasks (plist-get alerts :bad_estimate_tasks)))
    (when (> (length impossible-tasks) 0)
      (org-life-agenda-render-block-sub-title :title "Impossible Tasks")
      (dolist (impossible-task impossible-tasks)
        (org-life-agenda-render-impossible-task :impossible-task impossible-task
                                                :tasks-dict tasks-dict)))
    (when (> (length bad-estimate-tasks) 0)
      (org-life-agenda-render-block-sub-title :title "Tasks with Bad Estimate")
      (dolist (bad-estimate-task bad-estimate-tasks)
        (org-life-agenda-render-bad-estimate-task :bad-estimate-task bad-estimate-task
                                                  :tasks-dict tasks-dict)))))

(cl-defun org-life-agenda-render-agenda (&key agenda-data schedule-data)
  (org-life-echo "Rendering agenda ...")
  
  (let ((status (plist-get schedule-data :status))
        (err (plist-get schedule-data :error))
        (data (plist-get schedule-data :data)))
    (if (string= status "error")
        (org-life-agenda-render-error :message err)
      ;; (org-life-agenda-render-block :entries (plist-get agenda-data :tasks)
      ;;                               :title "Test")
      (let ((tasks-dict (plist-get agenda-data :tasks-dict))
            (general-info (plist-get data :general))
            (alerts (plist-get data :alerts))
            (daily-infos (plist-get data :daily_infos))
            (debug (plist-get data :debug))

            (today t))

        (org-life-agenda-render-debug :message debug)
        
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

(defun org-life-agenda-parse-clock (task-id clock-elem)
  (let* ((timestamp (org-element-property :value clock-elem))
         (start (org-timestamp--to-internal-time timestamp))
         (end (if (eq (org-element-property :status clock-elem) 'running)
                  (current-time)
                (org-timestamp--to-internal-time timestamp t)))
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
                                     :type 0
                                     :weakness 0)))
              session-data-list))))

(defun org-life-agenda-get-tasks-and-clocks (headline-elem)
  "Writes result into `org-life--temp-tasks' and `org-life--temp-clocks'."

  (when (org-element-property :todo-type headline-elem)
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
  (org-element-map
      (org-element-contents headline-elem) ; data
      'headline ; types
    #'org-life-agenda-get-tasks-and-clocks ; fun
    nil ; info
    nil ; first-match
    'headline ; no-recursion
    ))

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
         (let ((ast (org-element-parse-buffer granularity)))
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
             (make-prop :SCHEDULING_DAYS
                        '(:scheduling_days)
                        (string-to-number val))
             (make-prop :SCHEDULING_DAYS
                        '(:scheduling_days)
                        (string-to-number val))
             (make-prop :DAILY_INFO_DAYS
                        '(:daily_info_days)
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
                         (org-life-agenda-entry-id task))))))

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
         :done 0
         :status (cond
                  ((eq 'done (org-life-agenda-entry-todo-type task)) 1)
                  (t 0))
         :priority (org-life-agenda-entry-priority task)
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
                             ((eq repeater-type 'restart) 1)
                             (t 0))
                      :unit (cond
                             ((eq repeater-unit 'day) 1)
                             ((eq repeater-unit 'week) 2)
                             ((eq repeater-unit 'month) 3)
                             ((eq repeater-unit 'year) 4)
                             (t 0))
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

(defun org-life-agenda-get-schedule-data (agenda-data)
  (let (request
        response)

    (org-life-echo "Preparing server request ...")
    (setq request (org-life-agenda-get-scheduler-request agenda-data))
    
    (org-life-echo "Sending server request ...")
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
