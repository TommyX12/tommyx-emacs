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
  (list (list (kbd "q") 'spellcaster-status-quit)))

;;; Variables

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

;;; Faces

(defface spellcaster-section-heading-face
  '((t :inherit org-agenda-structure))
  "Face for Spellcaster section heading."
  :group 'spellcaster)

;;; Functions and Macros

(defun spellcaster-start-process ()
  "Start Spellcaster process."
  (spellcaster-kill-process)
  (spellcaster--with-log-buffer
   (erase-buffer))
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
  (when (null spellcaster--process)
    (spellcaster-start-process))
  (when spellcaster--process
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
      (process-send-string spellcaster--process encoded)
      (accept-process-output spellcaster--process spellcaster-wait))))

(defmacro spellcaster--with-log-buffer (&rest body)
  "Run BODY with Spellcaster log buffer.

Point will be positioned at the end of the buffer."
  `(with-current-buffer (get-buffer-create spellcaster--log-buffer-name)
     (save-excursion
       (goto-char (point-max))
       ,@body)))

(defun spellcaster--handle-response (msg)
  "Decode Spellcaster server response MSG, and return the decoded object."
  ;; (if (and spellcaster-use-native-json
  ;;          (fboundp 'json-parse-string))
  ;;     (ignore-errors
  ;;       (json-parse-string msg :object-type 'alist))
  ;;   (let ((json-array-type 'list)
  ;;         (json-object-type 'alist))
  ;;     (json-read-from-string msg)))
  (spellcaster--with-log-buffer
   (insert msg)))

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
      (spellcaster--handle-response response)
      (setq spellcaster--response-chunks nil))))

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

(defun spellcaster--number-to-string (number)
  "Convert NUMBER to a display string (at most 1 decimal point).

If the value of NUMBER is an integer, no decimal point will be displayed."
  (let ((rounded (round number)))
    (if (= rounded number)
        (number-to-string rounded)
      (let ((number (round number 0.1)))
        (cond ((= (% number 10) 0)
               (number-to-string (round (* number 0.1))))
              (t
               (format "%.1f" (* number 0.1)))))))
  ;; ;; Two decimal point implementation
  ;; (let ((rounded (round number)))
  ;;   (if (= rounded number)
  ;;       (number-to-string rounded)
  ;;     (let ((number (round number 0.01)))
  ;;       (cond ((= (% number 100) 0)
  ;;              (number-to-string (round (* number 0.01))))
  ;;             ((= (% number 10) 0)
  ;;              (format "%.1f" (* number 0.01)))
  ;;             (t
  ;;              (format "%.2f" (* number 0.01)))))))
  )

(defun spellcaster--get-ui-data (config)
  "Extract data from CONFIG for use in status window."
  (let ((attribute-to-items (ht-create))
        (attribute-to-states (ht-create))
        (days-to-timestamps (ht-create)))

    (ht-each
     (lambda (item-id item-config)
       (let ((attributes (ht-get item-config "attributes")))
         (dolist (attribute attributes)
           (ht-set attribute-to-items
                   attribute
                   (cons item-id
                         (ht-get attribute-to-items
                                 attribute))))))

     (plist-get config :all-item-config))

    (ht-each
     (lambda (state-name state-config)
       (let ((attributes (ht-get state-config "attributes")))
         (dolist (attribute attributes)
           (ht-set attribute-to-states
                   attribute
                   (cons state-name
                         (ht-get attribute-to-states
                                 attribute))))))

     (plist-get config :all-state-config))

    (ht-each
     (lambda (item-id timestamps)
       (dolist (timestamp timestamps)
         (let ((timestamp-days (car timestamp))
               (timestamp-type-name (cdr timestamp)))
           (spellcaster-safe-update
            days-to-timestamps timestamp-days (ht-create)
            (lambda (prev)
              (spellcaster-safe-update
               prev item-id nil
               (lambda (prev)
                 (cons timestamp-type-name prev)))
              prev)))))
     (plist-get config :timestamps))

    (ht<-alist (list
                (cons "attribute-to-items" attribute-to-items)
                (cons "attribute-to-states" attribute-to-states)
                (cons "days-to-timestamps" days-to-timestamps)))))

(defun spellcaster--number-to-delta-string (delta)
  "Convert DELTA to a readable string representation with appropriate face."
  (spellcaster--with-face
   (cond
    ((or (equal delta "inf")
         (equal delta 1.0e+INF))
     "inf")
    ((= delta 0)
     "")
    (t
     (concat
      (if (< delta 0)
          ""
        "+")
      (spellcaster--number-to-string delta))))
   (if (> delta 0)
       'spellcaster-state-delta-face
     'spellcaster-state-negative-delta-face)))

(defun spellcaster--get-item-attribute (snapshot item-id prop &optional default)
  "Return the attribute with name PROP for item with ITEM-ID in SNAPSHOT.

Use DEFAULT when the value is not found."
  (let* ((item-attributes (spellcaster-safe-get
                           snapshot "item-attributes" nil))
         (item-attr (spellcaster-safe-get
                     item-attributes item-id nil)))
    (spellcaster-safe-get item-attr prop default)))

(defun spellcaster--inf-to-number (number)
  "Convert NUMBER to 1.0e+INF if NUMBER is \"inf\", otherwise return NUMBER."
  (if (equal number "inf")
      1.0e+INF
    number))

(defun spellcaster--stringify-inf (number)
  "Convert NUMBER to \"inf\" if NUMBER is 1.0e+INF, otherwise return NUMBER."
  (if (equal number 1.0e+INF)
      "inf"
    number))

(defun spellcaster--difference (value1 value2)
  "Compute VALUE1 - VALUE2, while taking care of the \"inf\" special case."
  (spellcaster--stringify-inf (- (spellcaster--inf-to-number value1)
                                 (spellcaster--inf-to-number value2))))

(defun spellcaster--get-state-attribute (snapshot state-name prop &optional default prev-snapshot)
  "Return the attribute with name PROP for state with STATE-NAME in SNAPSHOT.

Use DEFAULT when the value is not found.

If PREV-SNAPSHOT is non-nil, return a cons cell where car is the value in
PREV-SNAPSHOT, and cdr is the value in SNAPSHOT."
  (if prev-snapshot
      (cons
       (spellcaster--get-state-attribute
        prev-snapshot state-name prop default)
       (spellcaster--get-state-attribute
        snapshot state-name prop default))
    (let* ((state-attributes (ht-get snapshot "state-attributes"))
           (state-attr (ht-get state-attributes state-name)))
      (or (and state-attr
               (ht-get state-attr prop))
          default))))

(spellcaster--define-renderer spellcaster--render-status
    ()
  (insert "Hello World!"))

(defun spellcaster--render ()
  ;; TODO
  "Render status window."
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
  (if (spellcaster--in-status-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (spellcaster--render))
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

          (spellcaster-mode)
          (spellcaster-status-refresh))))))

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

;;; Footer

(provide 'spellcaster)

;;; spellcaster.el ends here
