;;; spellcaster.el --- Emacs client for spellcaster -*- lexical-binding: t -*-

;; Author: TommyX
;; Maintainer: TommyX
;; Version: 0.0.10.01
;; Package-Requires: (TODO)
;; Homepage: https://github.com/TommyX12/spellcaster.el
;; Keywords: TODO


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; TODO

;;; Code:

;;; Dependencies

(require 'cl-lib)
(require 'dash)
(require 'f)
(require 'ht)
(require 'json)
(require 's)

;;; Customizations

(defgroup spellcaster nil
  "Options for spellcaster."
  :link '(url-link :tag "Github" "https://github.com/TommyX12/spellcaster.el")
  :prefix "spellcaster-")

(defcustom spellcaster-use-native-json t
  "Whether to use native JSON when possible."
  :group 'spellcaster
  :type 'boolean)

(defcustom spellcaster-config-path (expand-file-name "~/caster.json")
  "Path to spellcaster config file."
  :group 'spellcaster
  :type 'string)

(defcustom spellcaster-command "spellcaster"
  "Command to start spellcaster engine."
  :group 'spellcaster
  :type 'string)

(defcustom spellcaster-display-buffer-function #'display-buffer
  "The function used display a Spellcaster buffer."
  :group 'spellcaster
  :type '(radio (function-item display-buffer)
                (function :tag "Function")))

(defcustom spellcaster-buffer-name "*Spellcaster*"
  "Name of Spellcaster status buffer."
  :group 'spellcaster
  :type 'string)

(defcustom spellcaster-restore-windows-on-quit nil
  "Whether to restore window configurations when quitting the spellcaster status buffer."
  :group 'spellcaster
  :type 'boolean)

;;; Constants

;; TODO make these customizable
(defconst spellcaster--update-prefix "@update: ")
(defconst spellcaster--hooks-alist nil)
(defconst spellcaster--process-name "spellcaster--process")
(defconst spellcaster--log-buffer-name "*spellcaster-log*")
(defconst spellcaster--key-bindings
  (list (list (kbd "q") 'spellcaster-status-quit)
        (list (kbd "r") 'spellcaster-status-refresh)
        (list (kbd "u") 'spellcaster-update)
        (list (kbd "U") 'spellcaster-update-forced)
        (list (kbd "d") 'spellcaster-kill-spell-at-point)
        (list (kbd "c") 'spellcaster-auto-cast-spell-at-point)
        (list (kbd "C") 'spellcaster-cast-spell-at-point)))
(defconst spellcaster--status-faces
  (ht<-plist
   (list "standby" 'default
         "running" 'spellcaster-running-face
         "warning" 'warning
         "error" 'error
         "success" 'success)))

;;; Variables

(defvar spellcaster--spaceline-content nil)
(defvar spellcaster--status-changed nil)
(defvar spellcaster--timer nil)
(defvar spellcaster--last-updated-time nil)
(defvar spellcaster-executable-args nil
  "Extra arguments passed to Spellcaster.")
(defvar-local spellcaster--prev-window-conf nil
  "Saved window configuration for restore.")
(put 'spellcaster--prev-window-conf 'permanent-local t)
(defvar spellcaster--process nil
  "Spellcaster server process.")
(defvar spellcaster--response nil
  "Temporarily stored Spellcaster server responses.")
(defvar spellcaster--response-chunks nil
  "The string to store response chunks from Spellcaster server.")
(defvar spellcaster--spells (ht-create)
  "The collection of spells.")

;;; Faces

(defface spellcaster-section-heading-face
  '((t :inherit org-agenda-structure))
  "Face for Spellcaster section heading."
  :group 'spellcaster)

(defface spellcaster-item-face
  '((t :inherit font-lock-keyword-face))
  "Face for Spellcaster item."
  :group 'spellcaster)

(defface spellcaster-running-face
  '((t :inherit font-lock-function-name-face))
  "Face for Spellcaster running spells."
  :group 'spellcaster)

(defface spellcaster-secondary-face
  '((t :inherit font-lock-comment-face))
  "Face for Spellcaster secondary text."
  :group 'spellcaster)

;;; Functions and Macros

(defun spellcaster--clear-spells ()
  "Clear the spells collection."
  (setq spellcaster--spells (ht-create))
  (setq spellcaster--status-changed t))

(defun spellcaster-start-process ()
  "Start Spellcaster process."
  (spellcaster-kill-process)
  (spellcaster--clear-spells)
  (spellcaster--with-log-buffer
   (erase-buffer))
  (condition-case err
      (setq spellcaster--process
            (make-process
             :name spellcaster--process-name
             :command (append
                       (list spellcaster-command
                             (expand-file-name
                              spellcaster-config-path))
                       spellcaster-executable-args)
             :filter #'spellcaster--process-filter
             :sentinel #'spellcaster--process-sentinel))
    (error (let ((msg "Failed to start Spellcaster."))
             (spellcaster--with-log-buffer
              (insert msg))
             (display-warning 'spellcaster msg))))
  ;; hook setup
  (message "Spellcaster server started.")
  (dolist (hook spellcaster--hooks-alist)
    (add-hook (car hook) (cdr hook))))

(defun spellcaster-kill-process ()
  "Kill Spellcaster process."
  (when spellcaster--process
    (let ((process spellcaster--process))
      (delete-process process)
      ;; this happens last so sentinel catch the kill
      (setq spellcaster--process nil))
    ;; hook remove
    (dolist (hook spellcaster--hooks-alist)
      (remove-hook (car hook) (cdr hook)))))

(defun spellcaster-send-request (request)
  "Send REQUEST to Spellcaster server.
REQUEST needs to be a JSON-serializable object."
  (if spellcaster--process
      ;; TODO make sure utf-8 encoding works
      (let ((encoded (concat
                      (if (and spellcaster-use-native-json
                               (fboundp 'json-serialize))
                          (json-serialize request
                                          :null-object nil
                                          :false-object json-false)
                        (let ((json-null nil)
                              (json-encoding-pretty-print nil))
                          (json-encode-list request)))
                      "\n")))
        (setq spellcaster--response nil)
        (process-send-string spellcaster--process encoded))
    (error "Spellcaster process not started\n")))

(defmacro spellcaster--with-log-buffer (&rest body)
  "Run BODY with Spellcaster log buffer.

Point will be positioned at the end of the buffer."
  `(with-current-buffer (get-buffer-create spellcaster--log-buffer-name)
     (save-excursion
       (goto-char (point-max))
       ,@body)))

(defun spellcaster--handle-update (update)
  "Process the parsed JSON object UPDATE."
  (let ((spell-path (plist-get update :spell_path))
        (spell-name (plist-get update :spell_name))
        (status (plist-get update :status))
        (message (plist-get update :message)))
    (ht-set spellcaster--spells
            spell-path
            (list :name spell-name
                  :status status
                  :message message))
    (setq spellcaster--status-changed t)))

(defun spellcaster--handle-response (msg)
  "Decode Spellcaster server response MSG, and return the decoded object."
  ;; (if (and spellcaster-use-native-json
  ;;          (fboundp 'json-parse-string))
  ;;     (ignore-errors
  ;;       (json-parse-string msg :object-type 'alist))
  ;;   (let ((json-array-type 'list)
  ;;         (json-object-type 'alist))
  ;;     (json-read-from-string msg)))
  (if (s-starts-with-p spellcaster--update-prefix msg)
      (if (and spellcaster-use-native-json
               (fboundp 'json-parse-string))
          (ignore-errors
            (json-parse-string msg :object-type 'plist))
        (let ((json-array-type 'list)
              (json-object-type 'plist))
          (spellcaster--handle-update
           (json-read-from-string
            (substring msg (length spellcaster--update-prefix))))))
    (spellcaster--with-log-buffer
     (unless (equal msg "")
       (insert msg "\n")))))

(defun spellcaster--process-sentinel (process event)
  "Sentinel for Spellcaster server process.
PROCESS is the process under watch, EVENT is the event occurred."
  
  (when (and spellcaster--process
             (memq (process-status process) '(exit signal)))
    (let ((msg "Spellcaster process terminated."))
      (spellcaster--with-log-buffer
       (insert msg "\n"))
      (message msg))
    (setq spellcaster--process nil)))

(defun spellcaster--process-filter (process output)
  "Filter for Spellcaster server process.
PROCESS is the process under watch, OUTPUT is the output received."
  (push output spellcaster--response-chunks)
  (when (s-ends-with-p "\n" output)
    (let ((response
           (mapconcat #'identity
                      (nreverse spellcaster--response-chunks)
                      nil)))
      (dolist (line (s-split "\n" response))
        (spellcaster--handle-response line))
      (setq spellcaster--response-chunks nil))))

(defun spellcaster-auto-cast-spell (spell-id)
  "Cast the spell with SPELL-ID."
  (spellcaster-send-request
   (list :action "auto_cast"
         :spell_id spell-id)))

(defun spellcaster-cast-spell (spell-id)
  "Cast the spell with SPELL-ID."
  (spellcaster-send-request
   (list :action "cast"
         :spell_id spell-id)))

(defun spellcaster-kill-spell (spell-id)
  "Kill the spell with SPELL-ID."
  (spellcaster-send-request
   (list :action "kill"
         :spell_id spell-id)))

(defun spellcaster--with-face (text face)
  "Return TEXT with FACE."
  (propertize text 'face face))

(defun spellcaster-setup-status-bindings (&optional evil map)
  "Bind default key bindings for the status window.

If EVIL is non-nil, bind to evil motion and normal state instead.

If MAP is non-nil, bind to that keymap."
  (let ((map (or map spellcaster-mode-map)))
    (dolist (binding spellcaster--key-bindings)
      (if evil
          (apply #'evil-define-key* '(motion normal) map binding)
        (apply #'define-key map binding)))))

(defun spellcaster-setup-evil-status-bindings ()
  "Bind default key bindings for the status window using evil."
  (spellcaster-setup-status-bindings t))

(defun spellcaster-safe-get (table key default)
  "Get the value for KEY in TABLE.

Fallback to DEFAULT if TABLE is nil, or if KEY does not exist."
  (if table
      (ht-get table key default)
    default))

(defun spellcaster-safe-get-chain (table default &rest keys)
  "Get the value for a chain of KEYS in TABLE (presumably nested hash tables).

Fallback to DEFAULT if any table in the chain is nil, or if KEYS does not exist."
  (let ((result table)
        (loop t))
    (while (and keys loop)
      (let ((key (pop keys)))
        (if result
            (setq result
                  (ht-get result key (and (null keys) default)))
          (setq result default
                loop nil))))
    result))

(defun spellcaster-safe-update (table key default mapper)
  "Update TABLE's entry with KEY using MAPPER.

MAPPER will take one argument, which is the value before update.
It should return the value after update.

If KEY doesn't exist in TABLE or value is nil, DEFAULT will be passed to MAPPER."
  (ht-set table key
          (funcall mapper (ht-get table key default))))

(defun spellcaster-has-running-spells ()
  "TODO: Return whether there are spells that are currently running."
  nil)

(defun spellcaster--emacs-quit-query-function ()
  "Ask the user for game save before quitting Emacs."
  (or
   (not (spellcaster-has-running-spells))
   (y-or-n-p "One or more spells are running.  Force quit? "))) ;; only fails on keyboard quit or error

(defun spellcaster--deep-copy (object)
  "Make a deep copy of OBJECT.
This is similar to `copy-tree', but handles hash tables as well."

  (cond
   ((consp object)
    (let (result)
      (while (consp object)
        (let ((newcar (car object)))
          (if (or (consp (car object))
                  (vectorp (car object)))
              (setq newcar (spellcaster--deep-copy (car object))))
          (push newcar result))
        (setq object (cdr object)))
      (nconc (nreverse result) object)))

   ((hash-table-p object)
    ;; Reference:
    ;; http://www.splode.com/~friedman/software/emacs-lisp/src/deep-copy.el
    (let ((new-table
           (make-hash-table
            :test             (hash-table-test             object)
            :size             (hash-table-size             object)
            :rehash-size      (hash-table-rehash-size      object)
            :rehash-threshold (hash-table-rehash-threshold object)
            :weakness         (hash-table-weakness         object))))
      (maphash (lambda (key value)
                 (puthash (spellcaster--deep-copy key)
                          (spellcaster--deep-copy value)
                          new-table))
               object)
      new-table))

   ((vectorp object)
    (let ((i (length (setq object (copy-sequence object)))))
      (while (>= (setq i (1- i)) 0)
        (aset object i (spellcaster--deep-copy (aref object i))))
      object))

   (t
    object)))

(defun spellcaster--save-window-configuration ()
  "Save the current window configuration.
Note that the configuration is saved locally to the current buffer."
  (unless (or (get-buffer-window (current-buffer) (selected-frame))
              spellcaster--prev-window-conf)
    (setq-local spellcaster--prev-window-conf
                (current-window-configuration))))

(defun spellcaster--restore-window-configuration ()
  "Restore previous window configuration."
  (let ((winconf spellcaster--prev-window-conf)
        (buffer (current-buffer))
        (frame (selected-frame)))
    ;; TODO: this allows killing buffer by setting second arg to t
    (quit-window nil (selected-window))
    (when (and winconf (equal frame (window-configuration-frame winconf)))
      (set-window-configuration winconf)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq-local spellcaster--prev-window-conf nil))))))

(defmacro spellcaster--define-renderer (name inputs &rest body)
  "Define a Spellcaster renderer with NAME.

Define a function with BODY that accepts INPUTS as keyword argument.
The first element of BODY can be the docstring."
  (declare (indent 2) (doc-string 3))
  `(cl-defun ,name (&key ,@inputs &allow-other-keys)
     ,@body))

(defmacro spellcaster--inline-renderer (inputs &rest body)
  "Define an inline Spellcaster renderer.

Return a function with BODY that accepts INPUTS as keyword argument."
  (declare (indent 1))
  `(cl-function
    (lambda (&key ,@inputs &allow-other-keys)
      ,@body)))

(defmacro spellcaster--partial-renderer (renderer &rest inputs)
  "Define an inline partial function of RENDERER.

Return RENDERER with INPUTS already fed as argument.

Note: DO NOT quote RENDERER."
  `(lambda (&rest rest)
     (apply (quote ,renderer) ,@inputs rest)))

(defmacro spellcaster--define-partial-renderer (name renderer &rest inputs)
  "Define a function with NAME as a partial function of RENDERER.

Return RENDERER with INPUTS already fed as argument.

Note: DO NOT quote RENDERER."
  `(defun ,name (&rest rest)
     (apply (quote ,renderer) ,@inputs rest)))

(defmacro spellcaster--with-status-buffer-if-open (&rest body)
  "Run BODY with Spellcaster status buffer selected.
If no Spellcaster buffer active, do nothing."
  `(when (and (get-buffer spellcaster-buffer-name)
              (get-buffer-window spellcaster-buffer-name))
     (with-current-buffer spellcaster-buffer-name
       ,@body)))

(defun spellcaster-status-refresh-if-dirty ()
  "Refresh active Spellcaster buffer if dirty."
  ;; TODO: optimize to not refresh entire buffer
  (when spellcaster--status-changed
    (spellcaster--refresh-spaceline)
    (spellcaster--with-status-buffer-if-open
     (spellcaster-status-refresh))))

(defun spellcaster--get-id-at-point ()
  "Return the spell-id at point."
  (get-text-property (point) 'spell-id))

(defun spellcaster-start-timer ()
  "Start a timer to refresh Spellcaster buffer."
  (unless spellcaster--timer
    (setq spellcaster--timer
          (run-at-time 0 5 #'spellcaster-status-refresh-if-dirty))))

(spellcaster--define-renderer spellcaster--render-status
    ()
  (insert
   (spellcaster--with-face
    (format-time-string "Last Updated: %Y-%m-%d %H:%M:%S\n"
                        spellcaster--last-updated-time)
    'spellcaster-secondary-face)
   "\n")
  (let ((spell-ids
         (sort (ht-keys spellcaster--spells)
               #'string<)))
    (dolist (spell-id spell-ids)
      (let* ((spell (ht-get spellcaster--spells spell-id))
             (spell-name (plist-get spell :name))
             (spell-status (plist-get spell :status))
             (msg (plist-get spell :message)))
        (insert
         (propertize
          (format "%-10s %-30s %s\n"
                  (spellcaster--with-face
                   spell-status
                   (ht-get spellcaster--status-faces spell-status))
                  (spellcaster--with-face
                   spell-name 'spellcaster-item-face)
                  (spellcaster--with-face
                   spell-id 'spellcaster-secondary-face))
          'spell-id spell-id))
        (when msg
          (let* ((msg (if (stringp msg)
                          msg
                        (prin1-to-string msg)))
                 (msg (s-trim-right msg)))
            (when (not (equal msg ""))
              (insert msg "\n"))))))))

(defun spellcaster--refresh-spaceline ()
  (setq
   spellcaster--spaceline-content
   (let ((spell-ids
          (sort (ht-keys spellcaster--spells)
                #'string<)))
     (s-join
      " "
      (mapcar
       (lambda (spell-id)
         (let* ((spell (ht-get spellcaster--spells spell-id))
                (spell-status (plist-get spell :status)))
           (spellcaster--with-face
            "●"
            (ht-get spellcaster--status-faces spell-status))))
       spell-ids)))))

(defun spellcaster--render ()
  ;; TODO
  "Render status window."
  (setq spellcaster--last-updated-time
        (current-time))
  (setq spellcaster--status-changed nil)
  (spellcaster--render-status))

(defun spellcaster--in-status-buffer ()
  "Return t if currently in Spellcaster status buffer."
  (eq (get-buffer spellcaster-buffer-name)
      (current-buffer)))

;;; Commands

(defun spellcaster-start ()
  "Start Spellcaster server."
  (interactive)
  (if spellcaster--process
      (error "Spellcaster already started\n")
    (spellcaster-start-process)))

(defun spellcaster-restart ()
  "Start/Restart Spellcaster server."
  (interactive)
  (spellcaster-start-process))

(defun spellcaster-stop ()
  "Stop Spellcaster server."
  (interactive)
  (when (or
         (not (spellcaster-has-running-spells))
         (y-or-n-p "One or more spells are running.  Force quit? "))
    (spellcaster-kill-process)))

(defun spellcaster-status-refresh ()
  "Refresh Spellcaster window."
  (interactive)
  (when (called-interactively-p)
    (spellcaster--refresh-spaceline))
  (if (spellcaster--in-status-buffer)
      (let ((inhibit-read-only t)
            (pos (point)))
        (erase-buffer)
        (spellcaster--render)
        (goto-char pos))
    (error "Not in Spellcaster buffer\n")))

(defun spellcaster-status-quit ()
  "Quit Spellcaster window."
  (interactive)
  (if (spellcaster--in-status-buffer)
      (if spellcaster-restore-windows-on-quit
          (spellcaster--restore-window-configuration)
        (setq-local spellcaster--prev-window-conf nil)
        (quit-window))
    (error "Not in Spellcaster buffer\n")))

(defun spellcaster-status ()
  "Open Spellcaster status window."
  (interactive)
  ;; TODO make quit window and start window customizable
  (let ((buffer (get-buffer-create spellcaster-buffer-name)))
    (unless buffer
      (error "Open Spellcaster buffer failed\n"))

    (unless (eq buffer (current-buffer))
      (with-current-buffer buffer
        (spellcaster--save-window-configuration))

      (let ((window (funcall spellcaster-display-buffer-function buffer)))
        (let* ((old-frame (selected-frame))
               (new-frame (window-frame window)))
          (select-window window)
          (unless (eq old-frame new-frame)
            (select-frame-set-input-focus new-frame))

          (spellcaster-mode))))
    (spellcaster-status-refresh)))

(defun spellcaster-show-log ()
  "Open Spellcaster log buffer."
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create spellcaster--log-buffer-name)))

(defun spellcaster-update ()
  "Signal Spellcaster to run an update."
  (interactive)
  (message "Running Spellcaster update.")
  (spellcaster-send-request
   (list :action "update"))
  (run-at-time 0.5 nil #'spellcaster-status-refresh-if-dirty))

(defun spellcaster-update-forced ()
  "Signal Spellcaster to run a forced update."
  (interactive)
  (when (y-or-n-p "Force update all spells? ")
    (message "Running Spellcaster update (forced).")
    (spellcaster-send-request
     (list :action "update"
           :force_run t))
    (run-at-time 0.5 nil #'spellcaster-status-refresh-if-dirty)))

(defun spellcaster-auto-cast-spell-at-point ()
  "Cast the spell's auto-command at point."
  (interactive)
  ;; TODO: generalize this
  (if (spellcaster--in-status-buffer)
      (let ((spell-id (spellcaster--get-id-at-point)))
        (if spell-id
            (when (y-or-n-p (format "Auto-cast the spell [%s]? "
                                    (plist-get
                                     (ht-get spellcaster--spells spell-id)
                                     :name)))
              (spellcaster-auto-cast-spell spell-id)
              (run-at-time 0.5 nil #'spellcaster-status-refresh-if-dirty))
          (error "No spell found at point\n")))
    (error "Not in Spellcaster buffer\n")))

(defun spellcaster-cast-spell-at-point ()
  "Cast the spell at point."
  (interactive)
  (if (spellcaster--in-status-buffer)
      (let ((spell-id (spellcaster--get-id-at-point)))
        (if spell-id
            (when (y-or-n-p (format "Cast the spell [%s]? "
                                    (plist-get
                                     (ht-get spellcaster--spells spell-id)
                                     :name)))
              (spellcaster-cast-spell spell-id))
          (error "No spell found at point\n")))
    (error "Not in Spellcaster buffer\n")))

(defun spellcaster-kill-spell-at-point ()
  "Kill the spell at point."
  (interactive)
  (if (spellcaster--in-status-buffer)
      (let ((spell-id (spellcaster--get-id-at-point)))
        (if spell-id
            (when (y-or-n-p (format "Kill the spell [%s]? "
                                    (plist-get
                                     (ht-get spellcaster--spells spell-id)
                                     :name)))
              (spellcaster-kill-spell spell-id)
              (run-at-time 0.5 nil #'spellcaster-status-refresh-if-dirty))
          (error "No spell found at point\n")))
    (error "Not in Spellcaster buffer\n")))

;;; Minor modes

;;; Major modes

;; TODO: decide between `make-sparse-keymap' and `make-keymap'
;; TODO: evil integration. maybe motion only (will need to make sure q is not used in motion).
;; or manually set every evil key.
(defvar spellcaster-mode-map
  (let ((map (make-sparse-keymap)))
    (spellcaster-setup-status-bindings nil map)
    map))

(define-derived-mode spellcaster-mode
  special-mode "Spellcaster"
  "Major mode for Spellcaster status."
  (toggle-truncate-lines 1)
  (font-lock-mode -1)
  (setq-local tab-width 4))

;;; Hooks

;;; Init

(spellcaster-start-timer)

;;; Spaceline segment

(defun spellcaster-define-spaceline-segment ()
  (spaceline-define-segment spellcaster
    "A spaceline segment to display Spellcaster spells."
    spellcaster--spaceline-content))

;;; Footer

(provide 'spellcaster)

;;; spellcaster.el ends here
