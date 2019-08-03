
;;; companion.el --- TODO description

;; TODO license here

;;; Commentary:

;; TODO

;;; Code:

;;
;; Dependencies
;;

(require 'alert)
(require 'all-the-icons)
(require 'battery)
(require 'dash)
(require 'spaceline)
(require 'type-break)
(require 'url)

;; (require 'symon)

;;
;; Constants
;;

(defconst companion-buffer-name " *companion*"
  "Name of the buffer.")

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

(defcustom companion-notif-default-duration 10
  "*The default notification duration."
  :type 'integer
  :group 'companion)

(defcustom companion-qod-refresh-time (* 8 60 60)
  "*Quote-of-the-day refresh interval in seconds."
  :type 'float
  :group 'companion)

(defcustom companion-qod-max-string-length 120
  "*Quote-of-the-day max string length in companion notification."
  :type 'integer
  :group 'companion)

;;
;; Faces
;;

(defface companion-face
  '((t (:inherit header-line)))
  "*Face used for the companion buffer."
  :group 'companion :group 'font-lock-highlighting-faces)
(defvar companion-face 'companion-face)
(defface companion-secondary
  '((t (:inherit header-line)))
  "*Face used for the companion buffer (secondary)."
  :group 'companion :group 'font-lock-highlighting-faces)
(defvar companion-secondary 'companion-secondary)
(defface companion-notif-icon-info
  '((t (:foreground "#67b11d" :inherit companion-face)))
  "*Face used for the icon in companion buffer for info notifications."
  :group 'companion :group 'font-lock-highlighting-faces)
(defvar companion-notif-icon-info 'companion-notif-icon-info)
(defface companion-notif-icon-warn
  '((t (:foreground "#67b11d" :inherit companion-face)))
  "*Face used for the icon in companion buffer for critical notifications."
  :group 'companion :group 'font-lock-highlighting-faces)
(defvar companion-notif-icon-warn 'companion-notif-icon-warn)

;;
;; Variables
;;

(defvar companion--buffer nil)

(defvar companion--window nil)

(defvar companion-notif--current nil)
(defvar companion-notif--stack nil)
(defvar companion-notif--streams nil)
(defvar companion-notif--screen-time 0)

(defvar companion-qod--last-displayed nil)
(defvar companion-qod--last-quote "")

;;
;; Major mode definition
;;

(define-derived-mode companion-mode special-mode "Companion"
  "The major mode for companion."
  (setq indent-tabs-mode nil
        buffer-read-only t
        truncate-lines t)
  (setq-local show-trailing-whitespace nil))

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
       (eql (window-buffer companion--window) (companion--get-buffer t))))

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
  (setq-local scroll-margin 0)
  (buffer-disable-undo)
  (current-buffer))

(defun companion-default-display-fn (buffer _alist)
  "Display BUFFER at the top of the root window.
The root window is the root window of the selected frame.
_ALIST is ignored."
  (display-buffer-in-side-window buffer '((side . top))))

(defun companion--get-buffer (&optional inhibit-creation)
  "Return the global companion buffer if it exists.
If global companion buffer not exists, create it."
  (unless (equal (buffer-name companion--buffer)
                 companion-buffer-name)
    (setq companion--buffer nil))
  (when (and (null companion--buffer) (null inhibit-creation))
    (save-window-excursion
      (setq companion--buffer
            (companion-buffer--create))))
  companion--buffer)

(defun companion-window--init (window buffer)
  "Make WINDOW a Companion window.
Companion buffer is BUFFER."
  (companion--with-resizable-window
   (switch-to-buffer buffer)
   (setq-local face-remapping-alist
               '((default companion-face)
                 (powerline-active2 companion-secondary)))
   (set-window-parameter window 'no-delete-other-windows t)
   (set-window-parameter window 'no-other-window t)
   (set-window-margins window 0 0)
   (set-window-fringes window 1 1)
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
  "Attach the global companion buffer."
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
  "Renders the companion buffer."
  (companion--with-window
    (companion--with-editing-buffer
     (let
         ((content
           (format-mode-line (let ((powerline-selected-window (selected-window)))
                               (spaceline-ml-companion)))))
       (when (> (length content) 0)
         (goto-char 0)
         (save-excursion
           (erase-buffer)
           (insert content)))))))

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

(defun companion--idle-update (&rest _)
  "Update the mode-line if idling."
  (when (and (current-idle-time) (>= (nth 1 (current-idle-time)) 0.5))
    (companion-update)))

(defun companion-compile ()
  "Compile the companion spaceline segments."
  (interactive)
  (let ((powerline-default-separator 'bar)
        (spaceline-separator-dir-left '(left . left))
        (spaceline-separator-dir-right '(right . right)))
    (spaceline-compile 'companion
      companion-segments-left
      companion-segments-right)))

(defun companion-notif--stream-update (name)
  "Updates a notification stream."
  (let* (
         (stream (plist-get companion-notif--streams name))
         (queue (plist-get stream :queue))
         )
    (when-let ((notif (car queue)))
      (companion-notif--alert-notifier notif)
      (setq companion-notif--streams (plist-put companion-notif--streams name (plist-put stream :queue (cdr queue))))
      )
    )
  )

(defun companion-notif-create-stream (name interval)
  "Create a new notification stream."

  ;; delete when stream exists
  (when (plist-get companion-notif--streams name)
    (companion-notif-delete-stream name)
    )

  (let ((stream `(
                  :timer
                  ,(run-at-time 0 interval 'companion-notif--stream-update name)
                  :queue
                  nil
                  )))
    (setq companion-notif--streams (plist-put companion-notif--streams name stream))
    )
  )

(defun companion-notif-delete-stream (name)
  "Delete a notification stream."
  (when-let ((stream (plist-get companion-notif--streams name)))
    (cancel-timer (plist-get stream :timer))
    (setq companion-notif--streams (plist-put companion-notif--streams name nil))
    )
  )

(defun companion-notif--alert-notifier (info)
  "Notifier function for alert.el using companion's notification system."
  ;; The message text is :message
  ;; (plist-get info :message)
  ;; The :title of the alert
  ;; (plist-get info :title)
  ;; The :category of the alert
  ;; (plist-get info :category)
  ;; The major-mode this alert relates to
  ;; (plist-get info :mode)
  ;; The buffer the alert relates to
  ;; (plist-get info :buffer)
  ;; Severity of the alert.  It is one of:
  ;;   `urgent'
  ;;   `high'
  ;;   `moderate'
  ;;   `normal'
  ;;   `low'
  ;;   `trivial'
  ;; (plist-get info :severity)
  ;; Whether this alert should persist, or fade away
  ;; (plist-get info :persistent)
  ;; Data which was passed to `alert'.  Can be
  ;; anything.
  ;; (plist-get info :data)

  (let* (
         (content (plist-get info :message))
         (severity (plist-get info :severity))
         (data (plist-get info :data))
         (duration (and
                    (not (plist-get info :persistent))
                    (or (when (listp data) (plist-get data :duration))
                        companion-notif-default-duration)))
         (id (plist-get info :id))
         (stream-name (when (listp data) (plist-get data :stream)))
         (stream (when stream-name (plist-get companion-notif--streams stream-name)))
         (queue (when stream (plist-get stream :queue)))
         )
    (if stream
        (progn
                                        ; remove :stream attribute
          (setq info (plist-put info :data (plist-put data :stream nil)))
                                        ; set :id to stream-id if :id is 'stream
          (when (eq id 'stream)
            (setq info (plist-put info :id stream-name))
            )
                                        ; add to notification stream
          (setq companion-notif--streams (plist-put companion-notif--streams stream-name (plist-put stream :queue (-snoc queue info))))
          )
                                        ; show notification
      (progn
        (when id (companion-notif--dismiss-id id))
        (push
         `(
           :content ,content
           :severity ,severity
           :duration ,duration
           :id ,id
           )
         companion-notif--stack)
        (companion-notif--update)
        )
      )
    )
  )


(defun companion-notif--update ()
  "Update shown notification."
  (setq companion-notif--screen-time 0)
  (setq companion-notif--current (car companion-notif--stack))
  (companion-update)
  )

(defun companion-notif--dismiss-id (id)
  "Dismiss all notifications with id ID."
  (setq companion-notif--stack
        (-remove
         (lambda (item) (eq (plist-get item :id) id))
         companion-notif--stack))
  )

(defun companion-notif--tick ()
  "Called periodically to perform tasks related to notification, such as timing."
  (let ((duration (plist-get companion-notif--current :duration)))
    (when (and duration (> duration 0))
      (if (> companion-notif--screen-time duration)
          (companion-notif-dismiss)
        (setq companion-notif--screen-time (1+ companion-notif--screen-time))
        )
      )
    )
  )

;;
;; Advices
;;

;; (defadvice save-some-buffers
;;    (after companion-save-buffer-fix activate)
;;  "Reset companion size."
;;  (when (companion--window-exists-p)
;;    (companion-reopen)))

;;
;; Hooks
;;

;;
;; Util methods
;;

(defun companion-util--set-window-height (n)
  "Make selected window N row height."
  (let ((window-safe-min-height 0) (window-resize-pixelwise t))
    (message (number-to-string (* n (or powerline-height (frame-char-height)))))
    (window-resize
     (selected-window)
     (round (-
             (* n (or powerline-height (frame-char-height)))
             (window-pixel-height)
             (if (and
                  window-divider-mode
                  (or
                   (eq window-divider-default-places 't)
                   (eq window-divider-default-places 'bottom-only)))
                 (- window-divider-default-bottom-width)
               0)
             -2))
     nil 'safe t)))

(defun companion-qod-callback (status)
  "Callback for ‘companion-fetch-qod’ command.
Argument STATUS is the http status of the request."
  (search-forward "\n\n")
  (ignore-errors
    (if (not status)
        (let* ((quote-json (json-read))
               (quotes (assoc-default
                        'quotes (assoc-default
                                 'contents quote-json)))
               (q (aref quotes 0))
               (quote-string (assoc-default 'quote q))
               (quote-author (assoc-default 'author q))
               (quote* (format "\"%s\" - %s"
                               quote-string
                               quote-author)))
          (setq companion-qod--last-quote quote*)
          (companion-show-qod quote*))
      (message "Error fetching quote: %s"
               (assoc-default 'message
                              (assoc-default 'error (json-read)))))))

(defun companion-show-qod (q)
  (message q)
  (companion-notif--alert-notifier
   (list :message (substring q
                             0 (min companion-qod-max-string-length
                                    (length q)))
         :severity 'trivial
         :persistent t)))

(defun companion-show-last-qod ()
  (interactive)
  (companion-show-qod companion-qod--last-quote))

(defun companion-copy-qod ()
  (interactive)
  (kill-new companion-qod--last-quote))

(defun companion-fetch-qod ()
  "Fetches quote of the day from theysaidso.com.
Taken from https://github.com/narendraj9/quoted-scratch."
  (interactive)
  (with-temp-buffer
    (let ((url-request-method "GET")
          (qod-service-url "http://quotes.rest/qod.json"))
      (ignore-errors
        (url-retrieve (url-generic-parse-url qod-service-url)
                      'companion-qod-callback)))))

;; TODO: bug: set-buffer nil when calling this without internet.
(defun companion-qod--tick ()
  (let ((time (time-to-seconds)))
    (when (or (null companion-qod--last-displayed)
              (> (- time companion-qod--last-displayed)
                 companion-qod-refresh-time))
      (companion-fetch-qod)
      (setq companion-qod--last-displayed time))))

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
    (companion-compile)
    (companion--render)
    (select-window cw)))

(defun companion-reopen ()
  "Close then open the Companion window again."
  (interactive)
  (when (companion--window-exists-p)
    (companion-close)
    (companion-open)))

(defun companion-update()
  "Update the companion buffer."
  (interactive)
  (unless (and (boundp 'evil-state)
               (eq evil-state 'insert))
    (with-demoted-errors "Compaion error: %S"
      (when (companion--window-exists-p)
        (companion--render)))))

(defun companion-notif-dismiss()
  "Dismiss one active notification in the companion buffer."
  (interactive)
                                        ; TODO
  (pop companion-notif--stack)

  (companion-notif--update)
  )

;;
;; Segments definition
;;

(spaceline-define-segment companion-emacs-version
  "A spaceline segment to display emacs version."
  (concat
   (propertize (all-the-icons-fileicon "elisp") 'face `(:height 0.8 :inherit mode-line-buffer-id :family ,(all-the-icons-fileicon-family)) 'display '(raise 0))
   (propertize (concat " Emacs " emacs-version) 'face 'bold)))

(spaceline-define-segment companion-time
  "A spaceline segment to display date and time."
  (concat
   (format-time-string "%Y-%m-%d")
   (propertize
    (format-time-string " %H:%M")
    'face 'mode-line-buffer-id)))

(spaceline-define-segment companion-notification
  "A spaceline segment to display notifications."
  (when companion-notif--current
    (let
        ((icon-face
          (if (or
               (eq (plist-get companion-notif--current :severity) 'low)
               (eq (plist-get companion-notif--current :severity) 'trivial)
               )
              'companion-notif-icon-info
            'companion-notif-icon-warn)))

      (concat
       (propertize "●" 'face icon-face)
       " "
       (plist-get companion-notif--current :content)
       ))))

(spaceline-define-segment companion-battery
  "A spaceline segment to display battery status."
  (if battery-status-function
      (propertize
       (battery-format "[%p]%b %t | "
                       (funcall battery-status-function))
       'face 'bold)
    nil))

(spaceline-define-segment companion-system-load
  "A spaceline segment to display system load."
  (let ((value (car (load-average))))
    (propertize
     (if value (format "%3d" value) "--")
     'face 'font-lock-function-name-face)))

(spaceline-define-segment companion-type-break
  "A spaceline segment to display `type-break' information."
  (if (and type-break-mode type-break-time-next-break)
      (let* ((secs (type-break-time-difference
                    (current-time)
                    type-break-time-next-break))
             (mins (/ secs 60))
             (secs (- secs (* mins 60)))
             (text (format "%02d:%02d" mins secs)))
        (cond
         ((< secs 0)
          (propertize text
                      'face 'error))
         ((< mins 5)
          (propertize text
                      'face 'warning))
         (t
          text)))
    (propertize
     "No Break"
     'face 'error)))

(spaceline-define-segment companion-emms
  "A spaceline segment to display EMMS information."
  (when (and emms-player-playing-p
             (emms-playlist-current-selected-track))
    (let* ((total-playing-time (emms-track-get
                                (emms-playlist-current-selected-track)
                                'info-playing-time))
           (playing-time emms-playing-time))

      (propertize
       (format "%s[%s/%s] %s"
               (if emms-player-paused-p
                   ""
                 "♫ ")
               (if playing-time
                   (format "%02d:%02d"
                           (/ playing-time 60)
                           (% playing-time 60))
                 "--:--")
               (if total-playing-time
                   (format "%02d:%02d"
                           (/ total-playing-time 60)
                           (% total-playing-time 60))
                 "--:--")
               (file-name-base
                (emms-track-name
                 (emms-playlist-current-selected-track))))
       'face 'font-lock-comment-face))))



;; (spaceline-define-segment companion-symon
;;   "A spaceline segment to display symon system monitor."
;;  (when-let ((display-fn (car symon--display-fns)))
;;    (replace-regexp-in-string "%" "%%" (apply 'concat (mapcar 'funcall display-fn))))
;; )

(setq companion-segments-left
      `(((companion-emacs-version
          companion-notification)
         :separator " | "
         :face 'companion-face
         :priority 1
         )
        ))
(setq companion-segments-right
      `((org-pomodoro)
        (org-clock)
        (persp-name)
        (workspace-number)
        (companion-emms :tight-right t :face 'companion-face
                        :priority 99)
        (" | " :tight t :face 'companion-face)
        ;; (companion-type-break :tight t :face 'companion-face)
        ;; (" | " :tight t :face 'companion-face)
        (companion-battery :tight t :face 'companion-face)
        ;; (companion-symon :face 'companion-face :tight t)
        (companion-system-load :face 'companion-face :tight t)
        (" | " :tight t :face 'companion-face)
        (companion-time :face 'companion-face :tight t)
        (" " :face 'companion-face :tight t)
        ))
(companion-compile)

;;
;; alert.el integration
;; 

(alert-define-style
 'companion :title "Companion"
 :notifier
 #'companion-notif--alert-notifier

 ;; Removers are optional.  Their job is to remove
 ;; the visual or auditory effect of the alert.
 :remover
 (lambda (info)
   ;; It is the same property list that was passed to
   ;; the notifier function.
   ))

;;
;; Setup
;; 

(run-with-idle-timer 0.5 t 'companion-update)
(run-at-time 0 3 'companion--idle-update)
(add-hook 'window-configuration-change-hook 'companion-update)

(run-at-time 0 3 'companion-notif--tick)
(run-at-time 0 60 'companion-qod--tick)


(provide 'companion)
;;; companion.el ends here
