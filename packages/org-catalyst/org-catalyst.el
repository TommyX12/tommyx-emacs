;;; org-catalyst.el --- TODO description -*- lexical-binding: t -*-

;; Author: TommyX
;; Maintainer: TommyX
;; Version: 0.0.1
;; Package-Requires: (TODO)
;; Homepage: TODO
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

;; TODO commentary

;;; Code:

;;; Dependencies

(require 'cl-lib)
(require 'calendar)
(require 'f)
(require 'ht)
(require 'json)
(require 'org)
(require 'org-id)
(require 's)

;;; LRU Library

;; NOTE:
;; Reference:
;; https://stackoverflow.com/questions/6652956/how-can-i-implement-an-expiring-lru-cache-in-elisp

(defstruct org-catalyst--lru-cache size newest oldest table)
(defstruct org-catalyst--lru-item key value next prev)

(defun org-catalyst--lru-remove-item (item lru)
  "Remove ITEM from LRU doubly linked list."
  (let ((next (org-catalyst--lru-item-next item))
        (prev (org-catalyst--lru-item-prev item)))
    (if next (setf (org-catalyst--lru-item-prev next) prev)
      (setf (org-catalyst--lru-cache-newest lru) prev))
    (if prev (setf (org-catalyst--lru-item-next prev) next)
      (setf (org-catalyst--lru-cache-oldest lru) next))))

(defun org-catalyst--lru-insert-item (item lru)
  "Insert ITEM into earliest of LRU doubly linked list."
  (let ((newest (org-catalyst--lru-cache-newest lru)))
    (setf (org-catalyst--lru-item-next item) nil (org-catalyst--lru-item-prev item) newest)
    (if newest (setf (org-catalyst--lru-item-next newest) item)
      (setf (org-catalyst--lru-cache-oldest lru) item))
    (setf (org-catalyst--lru-cache-newest lru) item)))

(defun org-catalyst--lru-create ()
  "Create a new least-recently-used cache and return it."
  (make-org-catalyst--lru-cache
   :size 0
   :newest nil
   :oldest nil
   :table (ht-create)))

(defun org-catalyst--lru-get (lru key &optional default)
  "Look up KEY in least-recently-used cache LRU and return its associated value.
If KEY is not found, return DEFAULT which defaults to nil.
The accessed entry is updated to be the earliest entry of the cache."
  (let ((item (ht-get (org-catalyst--lru-cache-table lru) key)))
    (if item
        (progn
          (org-catalyst--lru-remove-item item lru)
          (org-catalyst--lru-insert-item item lru)
          (org-catalyst--lru-item-value item))
      default)))

(defun org-catalyst--lru-remove (lru key)
  "Remove KEY from least-recently-used cache LRU."
  (let ((item (ht-get (org-catalyst--lru-cache-table lru) key)))
    (when item
      (ht-remove (org-catalyst--lru-cache-table lru)
                 (org-catalyst--lru-item-key item))
      (org-catalyst--lru-remove-item item lru)
      (decf (org-catalyst--lru-cache-size lru)))))

(defun org-catalyst--lru-set (lru key value)
  "Associate KEY with VALUE in least-recently-used cache LRU.
If KEY is already present in LRU, replace its current value with VALUE.
The accessed entry is updated to be the earliest entry of the cache."
  (let ((item (ht-get (org-catalyst--lru-cache-table lru) key)))
    (if item
        (progn
          (setf (org-catalyst--lru-item-value item) value)
          (org-catalyst--lru-remove-item item lru)
          (org-catalyst--lru-insert-item item lru))
      (let ((newitem (make-org-catalyst--lru-item :key key :value value)))
        (org-catalyst--lru-insert-item newitem lru)
        (ht-set (org-catalyst--lru-cache-table lru) key newitem)
        (incf (org-catalyst--lru-cache-size lru))))))

(defun org-catalyst--lru-evict (lru max-size)
  "Evict least recently used entries until LRU has MAX-SIZE."
  (while (> (org-catalyst--lru-cache-size lru)
            max-size)
    (org-catalyst--lru-remove lru
                              (org-catalyst--lru-item-key
                               (org-catalyst--lru-cache-oldest lru)))))

(defun org-catalyst--lru-contains (lru key)
  "Return t if LRU contain KEY."
  (ht-contains-p (org-catalyst--lru-cache-table lru) key))

;;; Constants

;; TODO make this customizable
(defconst org-catalyst--snapshot-suffix "-snapshot.json")
(defconst org-catalyst--history-suffix "-history.json")
(defconst org-catalyst--state-delta-prefix "delta_")
(defconst org-catalyst--param-prefix "param_")
(defconst org-catalyst--key-bindings
  (list (list (kbd "q") 'org-catalyst-status-quit)
        (list (kbd "r") 'org-catalyst-status-refresh)))

;;; Variables

(defvar org-catalyst--state-systems (ht-create)
  "List of all installed state systems.")
(defvar org-catalyst--item-systems (ht-create)
  "List of all installed item systems.")
(defvar org-catalyst--buffered-snapshots (org-catalyst--lru-create)
  "In-memory buffer for all snapshot objects.")
(defvar org-catalyst--buffered-histories (org-catalyst--lru-create)
  "In-memory buffer for all history objects.")
(defvar org-catalyst--modified-snapshot-keys (ht-create)
  "Keys of modified snapshots.")
(defvar org-catalyst--modified-history-keys (ht-create)
  "Keys of modified histories.")
(defvar org-catalyst--auto-save-timer nil
  "Timer for `org-catalyst-auto-save-mode'.")
(defvar org-catalyst--earliest-month-day nil
  ;; NOTE: make this better.
  "Earliest month-day of all snapshots.")
(defvar org-catalyst--earliest-modified-month-day nil
  "Earliest month-day of modification.")
(defvar org-catalyst--computing-snapshot nil
  "Flag to avoid infinite recurison.")
(defvar-local org-catalyst--prev-window-conf nil
  "Saved window configuration for restore.")
(put 'org-catalyst--prev-window-conf 'permanent-local t)

;;; Customizations

(defgroup org-catalyst nil
  "Options for org-catalyst."
  :link '(url-link :tag "Github" "https://github.com/TommyX12/org-catalyst")
  :group 'org
  :prefix "org-catalyst-")

(defcustom org-catalyst-save-path
  (f-join org-directory "org-catalyst")
  "Path to org-catalyst config file."
  :group 'org-catalyst
  :type 'string)

(defcustom org-catalyst-display-buffer-function
  'org-catalyst--display-buffer-fullframe
  "The function used display a Catalyst buffer."
  :group 'org-catalyst
  :type '(radio (function-item org-catalyst--display-buffer-fullframe)
                (function :tag "Function")))

(defcustom org-catalyst-buffer-name "*Catalyst*"
  "Name of Catalyst status buffer."
  :group 'org-catalyst
  :type 'string)

(defcustom org-catalyst-auto-save-idle-delay 5
  "The idle delay to automatically save org-catalyst game when `org-catalyst-auto-save-mode' is on."
  :group 'org-catalyst
  :type 'float)

(defcustom org-catalyst-snapshot-cache-size 64
  "The number of snapshots to cache in memory."
  :group 'org-catalyst
  :type 'integer)

(defcustom org-catalyst-history-cache-size 64
  "The number of histories to cache in memory."
  :group 'org-catalyst
  :type 'integer)

;; TODO: make this more customizable
(defcustom org-catalyst-status-sections nil
  "The sections to render for status window."
  :group 'org-catalyst
  :type `(repeat
          (symbol :tag "Section name")))

;;; Macros

(defmacro org-catalyst--update-actions (month-day &rest body)
  "Evaluate BODY, which should update the actions at MONTH-DAY.
The actions object at MONTH-DAY will be bound to `actions'.
This marks the history containing MONTH-DAY as modified, and will perform
update on all cache after MONTH-DAY."
  `(let ((actions (org-catalyst--get-actions-at ,month-day)))
     ,@body
     (org-catalyst--maybe-set-earliest-month-day ,month-day)
     (org-catalyst--maybe-set-earliest-modified-month-day ,month-day)
     (ht-set org-catalyst--modified-history-keys
             (org-catalyst--month-day-to-key ,month-day) t)))

;;; Functions

(defun org-catalyst-setup-status-bindings (&optional evil map)
  "Bind default key bindings for the status window.

If EVIL is non-nil, bind to evil motion and normal state instead.

If MAP is non-nil, bind to that keymap."
  (let ((map (or map org-catalyst-mode-map)))
    (dolist (binding org-catalyst--key-bindings)
      (if evil
          (apply #'evil-define-key* '(motion normal) map binding)
        (apply #'define-key map binding)))))

(defun org-catalyst-setup-evil-status-bindings ()
  "Bind default key bindings for the status window using evil."
  (org-catalyst-setup-status-bindings t))

(defun org-catalyst-install-system (attribute type continuous update-func)
  "Install a system with UPDATE-FUNC for ATTRIBUTE on TYPE.
If CONTINUOUS is non-nil, the system runs consecutively through every day."
  (let ((system (ht-create)))
    (ht-set system "type" type)
    (ht-set system "continuous" continuous)
    (ht-set system "update-func" update-func)
    (cond
     ((eq type 'state)
      (ht-set org-catalyst--state-systems attribute system))
     ((eq type 'item)
      (ht-set org-catalyst--item-systems attribute system))
     (t
      (error "Invalid system type %s" (prin1-to-string type))))))

(defun org-catalyst-install-default-systems ()
  "Install a set of default systems."

  (org-catalyst-install-system
   "accumulate" 'state nil
   (lambda (state-attr action delta month-day params)
     (ht-set state-attr "total"
             (+ (ht-get state-attr "total" 0)
                (* action delta (ht-get params "mul" 1))))
     state-attr))

  (org-catalyst-install-system
   "count" 'item nil
   (lambda (item-attr action month-day params)
     (ht-set item-attr
             "count"
             (+ (ht-get item-attr "count" 0)
                action))
     item-attr))

  (org-catalyst-install-system
   "chain" 'item t
   ;; NOTE: inverse chain might be different since we assume "done"
   (lambda (item-attr action month-day params)
     (let* ((action (if (> action 0) 1 0))
            (last-chain-day-index
             (ht-get item-attr "last-chain-day-index" 1))
            (day-index
             (time-to-days
              (org-catalyst--month-day-to-time month-day)))
            (days-since-last
             (- day-index
                last-chain-day-index))
            (chain-interval
             (ht-get params "chain_interval" 1))
            (last-chain
             (if (> days-since-last chain-interval)
                 0
               (ht-get item-attr "chain" 0))))
       (ht-set item-attr "chain" (+ last-chain action))
       (ht-set item-attr "last-chain-day-index"
               (if (> action 0)
                   day-index
                 last-chain-day-index)))
     item-attr)))

(defun org-catalyst--get-earliest-month-day ()
  "Return earliest month-day of all histories.
If undefined, will attempt to calculate from files in save directory,
and fallback to current month-day."
  (or org-catalyst--earliest-month-day
      (setq
       org-catalyst--earliest-month-day
       (cons
        (let ((snapshot-month-days
               (mapcar
                (lambda (item)
                  (car
                   (org-catalyst--key-to-month-day
                    (substring (f-filename item) 0 7))))
                (f-glob (concat "*" org-catalyst--history-suffix)
                        org-catalyst-save-path))))
          (or
           (and snapshot-month-days
                (apply #'min snapshot-month-days))
           (car (org-catalyst--time-to-month-day))))
        0))))

(defun org-catalyst--maybe-set-earliest-month-day (month-day)
  "Set known earliest month-day to MONTH-DAY if existing value is later."
  (setq org-catalyst--earliest-month-day
        (cons
         (min (car (org-catalyst--get-earliest-month-day))
              (car month-day))
         0)))

(defun org-catalyst--maybe-set-earliest-modified-month-day (month-day)
  "Set known earliest-modified month-day to MONTH-DAY if existing value is later."
  (setq org-catalyst--earliest-modified-month-day
        (cons
         (if org-catalyst--earliest-modified-month-day
             (min (car org-catalyst--earliest-modified-month-day)
                  (car month-day))
           (car month-day))
         0)))

(defun org-catalyst--save-object (object file)
  "Save OBJECT to FILE."
  (unless (file-exists-p org-catalyst-save-path)
    (make-directory org-catalyst-save-path t))

  (unless (file-writable-p file)
    (error "File %s is not writable" file))

  (let ((json-object-type 'hash-table)
        (json-encoding-pretty-print t))
    (f-write-text (json-encode object) 'utf-8 file)))

(defun org-catalyst--load-object (file)
  "Load OBJECT from FILE."
  ;; NOTE: this is not efficient, but might handle encoding better.
  (when (file-exists-p file)
    (unless (file-readable-p file)
      (error "File %s is not readable" file))

    (let ((json-object-type 'hash-table))
      (json-read-from-string (f-read-text file 'utf-8)))))

(defun org-catalyst-has-unsaved-changes ()
  "Return if Catalyst has unsaved modifications."
  (or (not (ht-empty? org-catalyst--modified-snapshot-keys))
      (not (ht-empty? org-catalyst--modified-history-keys))))

(defun org-catalyst--time-to-month-day (&optional time)
  "Convert Emacs internal time representation TIME to month-day index representation."
  (let* ((decoded-time (decode-time time))
         (year (nth 5 decoded-time))
         (month (nth 4 decoded-time))
         (day-index (1- (nth 3 decoded-time)))
         (month-index (+ (* year 12) (1- month))))
    (cons month-index day-index)))

(defun org-catalyst--month-day-to-time (month-day &optional zone)
  "Convert MONTH-DAY index representation to Emacs internal time representation in time zone ZONE."
  (let* ((month-index (car month-day))
         (year (/ month-index 12))
         (month (1+ (- month-index (* year 12))))
         (day (1+ (cdr month-day))))
    (encode-time 0 0 0 day month year zone)))

(defun org-catalyst--month-day-days-delta (month-day-1 month-day-2)
  "Find the number of days elapsed between MONTH-DAY-1 and MONTH-DAY-2.
The return value is positive if MONTH-DAY-1 is before MONTH-DAY-2, and vice versa."
  (- (time-to-days (org-catalyst--month-day-to-time month-day-2))
     (time-to-days (org-catalyst--month-day-to-time month-day-1))))

(defun org-catalyst--read-month-day ()
  "Use org to read a month-day."
  (interactive)
  (org-catalyst--time-to-month-day
   (org-time-string-to-time (org-read-date))))

(defun org-catalyst--month-day-to-key (month-day)
  "Convert a MONTH-DAY to key."
  (let* ((month-index (car month-day))
         (year (/ month-index 12))
         (month (1+ (- month-index (* year 12)))))
    (format "%04d-%02d" year month)))

(defun org-catalyst--get-days-in-month (month-index)
  "Get the number of days in the month at MONTH-INDEX."
  (let* ((year (/ month-index 12))
         (month (1+ (- month-index (* year 12)))))
    (calendar-last-day-of-month month year)))

(defun org-catalyst--month-day-to-day-key (month-day)
  "Convert a MONTH-DAY to key."
  (let* ((day (1+ (cdr month-day))))
    (format "%02d" day)))

(defun org-catalyst--key-to-month-day (key)
  "Convert a KEY to month-day."
  (let ((year (string-to-number (substring key 0 4)))
        (month (string-to-number (substring key 5 7))))
    (cons (+ (* year 12) (1- month)) 0)))

(defun org-catalyst--key-to-snapshot-path (key)
  "Return file path of the snapshot at time denoted by KEY."
  (f-join org-catalyst-save-path
          (concat key org-catalyst--snapshot-suffix)))

(defun org-catalyst--key-to-history-path (key)
  "Return file path of the history at time denoted by KEY."
  (f-join org-catalyst-save-path
          (concat key org-catalyst--history-suffix)))

(defun org-catalyst--map-entries (filter func)
  "TODO make the filter better"
  (org-map-entries func filter 'agenda))

(defun org-catalyst--get-config ()
  "TODO"
  ;; TODO: allow only getting parts of the things to optimize
  (let ((all-item-config (ht-create))
        (all-state-config (ht-create))
        (state-update-funcs (ht-create))
        (item-update-funcs (ht-create))
        (continuous-state-update-funcs (ht-create))
        (continuous-item-update-funcs (ht-create)))
    (org-catalyst--map-entries
     "item|state"
     (lambda ()
       (let ((tags org-scanner-tags))
         (cond

          ((member "item" tags)
           (let ((display-name (org-no-properties
                                (org-get-heading t t t t)))
                 (item-id (org-id-get-create))
                 (item-config (ht-create))
                 (state-deltas (ht-create))
                 (attributes nil)
                 (params (ht-create)))
             (dolist (prop (let ((org-trust-scanner-tags t))
                             (org-entry-properties (point)
                                                   'standard)))
               (let ((prop-name (downcase (car prop)))
                     (prop-value (cdr prop)))
                 (cond
                  ((string= prop-name "attributes")
                   (dolist (attribute
                            (split-string prop-value
                                          "[ \f\t\n\r\v]+"
                                          t))
                     (let* ((attribute (downcase
                                        (string-trim attribute)))
                            (system (ht-get org-catalyst--item-systems
                                            attribute)))
                       (setq attributes (cons attribute attributes))
                       (when system
                         (let ((continuous (ht-get system "continuous"))
                               (update-func (ht-get system "update-func")))
                           (let ((funcs-map (if continuous
                                                continuous-item-update-funcs
                                              item-update-funcs)))
                             (ht-set
                              funcs-map
                              item-id
                              (cons update-func
                                    (ht-get funcs-map item-id)))))))))

                  ((s-starts-with-p org-catalyst--state-delta-prefix
                                    prop-name)
                   ;; TODO: state names are stored in lower case
                   (ht-set state-deltas
                           (substring prop-name
                                      (length org-catalyst--state-delta-prefix))
                           (string-to-number prop-value)))

                  ((s-starts-with-p org-catalyst--param-prefix
                                    prop-name)
                   (ht-set params
                           (substring
                            prop-name
                            (length org-catalyst--param-prefix))
                           (car (read-from-string prop-value)))))))

             (ht-set item-config "display-name" display-name)
             (ht-set item-config "state-deltas" state-deltas)
             (ht-set item-config "attributes" attributes)
             (ht-set item-config "params" params)
             (ht-set all-item-config item-id
                     item-config)))

          ((member "state" tags)
           (let* ((display-name (org-no-properties
                                 (org-get-heading t t t t)))
                  (state-name (downcase display-name))
                  (state-config (ht-create))
                  (attributes nil)
                  (params (ht-create)))
             (dolist (prop (let ((org-trust-scanner-tags t))
                             (org-entry-properties (point)
                                                   'standard)))
               (let ((prop-name (downcase (car prop)))
                     (prop-value (cdr prop)))
                 (cond

                  ((string= prop-name "attributes")
                   (dolist (attribute
                            (split-string prop-value
                                          "[ \f\t\n\r\v]+"
                                          t))
                     (let* ((attribute (downcase
                                        (string-trim attribute)))
                            (system (ht-get org-catalyst--state-systems
                                            attribute)))
                       (setq attributes (cons attribute attributes))
                       (when system
                         (let ((continuous (ht-get system "continuous"))
                               (update-func (ht-get system "update-func")))
                           (let ((funcs-map (if continuous
                                                continuous-state-update-funcs
                                              state-update-funcs)))
                             (ht-set
                              funcs-map
                              state-name
                              (cons update-func
                                    (ht-get funcs-map state-name)))))))))

                  ((s-starts-with-p org-catalyst--param-prefix
                                    prop-name)
                   (ht-set params
                           (substring
                            prop-name
                            (length org-catalyst--param-prefix))
                           (car (read-from-string prop-value)))))))

             (ht-set state-config "display-name" display-name)
             (ht-set state-config "attributes" attributes)
             (ht-set state-config "params" params)
             (ht-set all-state-config state-name state-config)))))))

    (list :all-item-config all-item-config
          :all-state-config all-state-config
          :state-update-funcs state-update-funcs
          :item-update-funcs item-update-funcs
          :continuous-state-update-funcs continuous-state-update-funcs
          :continuous-item-update-funcs continuous-item-update-funcs)))

(defun org-catalyst--update-function (snapshot
                                      actions
                                      month-day
                                      all-item-config
                                      all-state-config
                                      state-update-funcs
                                      item-update-funcs
                                      continuous-state-update-funcs
                                      continuous-item-update-funcs)
  "TODO
NOTE: This function can mutate SNAPSHOT."
  (let ((state-attributes (ht-get snapshot "state-attributes"))
        (item-attributes (ht-get snapshot "item-attributes"))
        (empty-params (ht-create))
        (state-updates (ht-create)))
    ;; TODO: consider the amount of action done
    ;; reactive update
    (dolist (item-id (ht-keys actions))
      (let* ((action (ht-get actions item-id 0))
             (item-config (ht-get all-item-config item-id))
             (state-deltas
              (and item-config
                   (ht-get item-config "state-deltas")))
             (item-params
              (or (and item-config
                       (ht-get item-config "params"))
                  empty-params)))
        (when state-deltas
          (dolist (state-name (ht-keys state-deltas))
            (let* ((state-config (ht-get all-state-config state-name))
                   (state-params (or (and
                                      state-config
                                      (ht-get state-config "params"))
                                     empty-params))
                   (delta (ht-get state-deltas state-name)))
              (ht-set state-updates state-name
                      (cons (cons action delta)
                            (ht-get state-updates state-name)))
              (dolist (state-update-func (ht-get state-update-funcs
                                                 state-name))
                (ht-set state-attributes
                        state-name
                        (funcall
                         state-update-func
                         (or (ht-get state-attributes state-name)
                             (ht-create))
                         action
                         delta
                         month-day
                         state-params))))))
        (dolist (item-update-func (ht-get item-update-funcs item-id))
          (ht-set item-attributes
                  item-id
                  (funcall
                   item-update-func
                   (or (ht-get item-attributes item-id)
                       (ht-create))
                   action
                   month-day
                   item-params)))))

    ;; continuous update

    (dolist (item-id (ht-keys continuous-item-update-funcs))
      (let* ((action (ht-get actions item-id 0))
             (item-config (ht-get all-item-config item-id))
             (item-params
              (or (and item-config
                       (ht-get item-config "params"))
                  empty-params)))
        (dolist (item-update-func (ht-get continuous-item-update-funcs
                                          item-id))
          (ht-set item-attributes
                  item-id
                  (funcall
                   item-update-func
                   (or (ht-get item-attributes item-id)
                       (ht-create))
                   action
                   month-day
                   item-params)))))

    (dolist (state-name (ht-keys continuous-state-update-funcs))
      (let* ((state-config (ht-get all-state-config state-name))
             (state-params
              (or (and state-config
                       (ht-get state-config "params"))
                  empty-params)))
        (dolist (state-update-func (ht-get continuous-state-update-funcs
                                           state-name))
          (ht-set state-attributes
                  state-name
                  (funcall
                   state-update-func
                   (or (ht-get state-attributes state-name)
                       (ht-create))
                   (ht-get state-updates state-name)
                   month-day
                   state-params)))))

    snapshot))

(defun org-catalyst--new-snapshot ()
  "Construct a new empty snapshot."
  (let ((snapshot (ht-create)))
    (ht-set snapshot "state-attributes" (ht-create))
    (ht-set snapshot "item-attributes" (ht-create))
    snapshot))

(defun org-catalyst--new-actions ()
  "Construct a new empty actions."
  (ht-create))

(defun org-catalyst--new-history ()
  "Construct a new empty history."
  (ht-create))

(defun org-catalyst--save-snapshot (key snapshot)
  "Save the given KEY and SNAPSHOT to disk."
  (org-catalyst--save-object
   snapshot
   (org-catalyst--key-to-snapshot-path key)))

(defun org-catalyst--save-history (key history)
  "Save the given KEY and HISTORY to disk."
  (org-catalyst--save-object
   history
   (org-catalyst--key-to-history-path key)))

(defun org-catalyst--emacs-quit-query-function ()
  "Ask the user for game save before quitting Emacs."
  (if (and
       (org-catalyst-has-unsaved-changes)
       (y-or-n-p "You have unsaved changes in org-catalyst.  Save game? "))
      (progn
        (org-catalyst-save-game)
        t)
    t)) ;; only fails on keyboard quit or error

(defun org-catalyst--next-month-day (month-day)
  "Return the day after MONTH-DAY."
  (let ((month-index (car month-day))
        (day-index (cdr month-day)))
    (if (>= day-index (org-catalyst--get-days-in-month month-index))
        (cons (1+ month-index) 0)
      (cons month-index (1+ day-index)))))

(defun org-catalyst--get-history-containing (month-day)
  "Return the history object containing MONTH-DAY."
  (let ((key (org-catalyst--month-day-to-key month-day)))
    (if (org-catalyst--lru-contains org-catalyst--buffered-histories key)
        (org-catalyst--lru-get org-catalyst--buffered-histories key)
      (let ((history (or
                      (org-catalyst--load-object
                       (org-catalyst--key-to-history-path key))
                      (org-catalyst--new-history))))
        (org-catalyst--lru-set
         org-catalyst--buffered-histories key
         history)
        history))))

(defun org-catalyst--get-actions-at (month-day)
  "Return the actions at MONTH-DAY."
  (let ((day-key (org-catalyst--month-day-to-day-key month-day))
        (history (org-catalyst--get-history-containing month-day)))
    (if (ht-contains-p history day-key)
        (ht-get history day-key)
      (let ((actions (org-catalyst--new-actions)))
        (ht-set history day-key actions)
        actions))))

(defun org-catalyst--deep-copy (object)
  "Make a deep copy of OBJECT.
This is similar to `copy-tree', but handles hash tables as well."

  (cond
   ((consp object)
    (let (result)
      (while (consp object)
        (let ((newcar (car object)))
          (if (or (consp (car object))
                  (vectorp (car object)))
              (setq newcar (org-catalyst--deep-copy (car object))))
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
                 (puthash (org-catalyst--deep-copy key)
                          (org-catalyst--deep-copy value)
                          new-table))
               object)
      new-table))

   ((vectorp object)
    (let ((i (length (setq object (copy-sequence object)))))
      (while (>= (setq i (1- i)) 0)
        (aset object i (org-catalyst--deep-copy (aref object i))))
      object))

   (t
    object)))

(defun org-catalyst--compute-snapshot-at (month-day &optional config)
  "Return the snapshot value at the start of MONTH-DAY.
It is ensured that the snapshot returned is a deep copy.

If CONFIG is non-nil, it is used instead of computing another."
  (org-catalyst--ensure-snapshots-correctness config)
  (org-catalyst--deep-copy
   (let ((month-index (car month-day))
         (day-index (cdr month-day))
         (config (or config
                     (org-catalyst--get-config))))
     (if (= day-index 0) ; start of the month
         (if (>= month-index (car (org-catalyst--get-earliest-month-day)))
             (let ((key (org-catalyst--month-day-to-key month-day)))
               (if (org-catalyst--lru-contains
                    org-catalyst--buffered-snapshots
                    key)
                   (org-catalyst--lru-get org-catalyst--buffered-snapshots
                                          key)
                 (let ((snapshot (org-catalyst--load-object
                                  (org-catalyst--key-to-snapshot-path key))))
                   (if snapshot
                       (org-catalyst--update-cached-snapshot
                        month-day snapshot t)
                     (setq snapshot (org-catalyst--compute-snapshot-at
                                     (cons
                                      (1- month-index)
                                      (org-catalyst--get-days-in-month
                                       (1- month-index)))
                                     config))
                     (org-catalyst--update-cached-snapshot month-day
                                                           snapshot))
                   snapshot)))
           (org-catalyst--new-snapshot))
       ;; other days of the month
       (let* ((all-item-config (plist-get config :all-item-config))
              (all-state-config (plist-get config :all-state-config))
              (state-update-funcs (plist-get config :state-update-funcs))
              (item-update-funcs (plist-get config :item-update-funcs))
              (continuous-state-update-funcs
               (plist-get config :continuous-state-update-funcs))
              (continuous-item-update-funcs
               (plist-get config :continuous-item-update-funcs))
              (cur-day-index 0)
              (cur-snapshot (org-catalyst--compute-snapshot-at
                             (cons month-index 0)
                             config)))
         (while (< cur-day-index day-index)
           (let ((cur-month-day (cons month-index cur-day-index)))
             (setq cur-snapshot
                   (org-catalyst--update-function
                    cur-snapshot
                    (org-catalyst--get-actions-at
                     cur-month-day)
                    cur-month-day
                    all-item-config
                    all-state-config
                    state-update-funcs
                    item-update-funcs
                    continuous-state-update-funcs
                    continuous-item-update-funcs)))
           (setq cur-day-index (1+ cur-day-index)))
         cur-snapshot)))))

(defun org-catalyst--compute-snapshot-after (month-day &optional config)
  "Return the snapshot value at the end of MONTH-DAY.

If CONFIG is non-nil, it is used instead of computing another."
  (let ((config (or config
                    (org-catalyst--get-config))))
    (org-catalyst--compute-snapshot-at
     (org-catalyst--next-month-day month-day)
     config)))

(defun org-catalyst--update-cached-snapshot (month-day snapshot &optional no-mark-modified)
  "Set cached snapshot object at MONTH-DAY to SNAPSHOT.
If NO-MARK-MODIFIED is nil, KEY will be marked as modified,
 and will be saved when `org-catalyst-save-game' is called."
  (let ((key (org-catalyst--month-day-to-key month-day)))
    (org-catalyst--lru-set org-catalyst--buffered-snapshots
                           key snapshot)
    (unless no-mark-modified
      (ht-set org-catalyst--modified-snapshot-keys key t))))

(defun org-catalyst--update-cached-snapshots (month-day &optional config)
  "Update all cached snapshots as if edits were made during MONTH-DAY.

If CONFIG is non-nil, it is used instead of computing another."
  (let ((config (or config
                    (org-catalyst--get-config)))
        (cur-month-index (1+ (car month-day)))
        (now-month-index (car (org-catalyst--time-to-month-day))))
    (while (<= cur-month-index now-month-index)
      (org-catalyst--update-cached-snapshot
       (cons cur-month-index 0)
       (org-catalyst--compute-snapshot-at
        (cons (1- cur-month-index)
              (org-catalyst--get-days-in-month
               (1- cur-month-index)))
        config))
      (setq cur-month-index (1+ cur-month-index)))))

(defun org-catalyst--ensure-snapshots-correctness (&optional config)
  "Update all snapshots to ensure they are correct.
This is provided that all modifications were made in org-catalyst.

If CONFIG is non-nil, it is used instead of computing another."
  (when (and org-catalyst--earliest-modified-month-day
             (not org-catalyst--computing-snapshot))
    (let ((earliest-modified-month-day
           org-catalyst--earliest-modified-month-day)
          (org-catalyst--computing-snapshot t)
          (config (or config
                      (org-catalyst--get-config))))
      (org-catalyst--update-cached-snapshots
       earliest-modified-month-day
       config)
      (setq org-catalyst--earliest-modified-month-day nil))))

(defun org-catalyst--save-window-configuration ()
  "Save the current window configuration.
Note that the configuration is saved locally to the current buffer."
  (unless (get-buffer-window (current-buffer) (selected-frame))
    (setq-local org-catalyst--prev-window-conf
                (current-window-configuration))))

(defun org-catalyst--restore-window-configuration ()
  "Restore previous window configuration."
  (let ((winconf org-catalyst--prev-window-conf)
        (buffer (current-buffer))
        (frame (selected-frame)))
    ;; TODO: this allows killing buffer by setting second arg to t
    (quit-window nil (selected-window))
    (when (and winconf (equal frame (window-configuration-frame winconf)))
      (set-window-configuration winconf)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq-local org-catalyst--prev-window-conf nil))))))

(defun org-catalyst--display-action-fullframe (buffer alist)
  "Action for `display-buffer' that displays in fullframe."
  (when-let ((window (or (display-buffer-reuse-window buffer alist)
                         (display-buffer-same-window buffer alist)
                         (display-buffer-pop-up-window buffer alist)
                         (display-buffer-use-some-window buffer alist))))
    (delete-other-windows window)
    window))

(defun org-catalyst--display-buffer-fullframe (buffer)
  "Display BUFFER in fullframe."
  (display-buffer buffer '(org-catalyst--display-action-fullframe)))

(defun org-catalyst--get-ui-data (config)
  "Extract data from CONFIG for use in status window."
  (let ((attribute-to-items (ht-create)))

    (ht-map
     (lambda (item-id item-config)
       (let ((attributes (ht-get item-config "attributes")))
         (dolist (attribute attributes)
           (ht-set attribute-to-items
                   attribute
                   (cons item-id
                         (ht-get attribute-to-items
                                 attribute))))))

     (plist-get config :all-item-config))

    (list :attribute-to-items attribute-to-items)))

(defun org-catalyst--get-item-attribute (snapshot item-id prop &optional default)
  "Return the attribute with name PROP for item with ITEM-ID in SNAPSHOT.

Use DEFAULT when the value is not found."
  (let* ((item-attributes (ht-get snapshot "item-attributes"))
         (item-attr (ht-get item-attributes item-id)))
    (or (and item-attr
             (ht-get item-attr prop))
        default)))

(cl-defun org-catalyst-section-separator (&key
                                          &allow-other-keys)
  (insert "\n"))

(cl-defun org-catalyst-section-overview (&key
                                         &allow-other-keys)
  (insert "Placeholder overview.\n"))

(cl-defun org-catalyst-section-highest-chain (&key
                                              config
                                              ui-data
                                              snapshot
                                              &allow-other-keys)
  (let* ((attribute-to-items (plist-get ui-data :attribute-to-items))
         (item-ids (ht-get attribute-to-items "chain"))
         (all-item-config (plist-get config :all-item-config))
         (item-attributes (ht-get snapshot "item-attributes")))
    (dolist (item-id item-ids)
      (let ((item-config (ht-get all-item-config item-id)))
        (when item-config
          (insert (format "%-8s | %s\n"
                          (org-catalyst--get-item-attribute
                           snapshot item-id "chain" 0)
                          (ht-get item-config "display-name"))))))))

(cl-defun org-catalyst-section-highest-count (&key
                                              config
                                              ui-data
                                              snapshot
                                              &allow-other-keys)
  (let* ((attribute-to-items (plist-get ui-data :attribute-to-items))
         (item-ids (ht-get attribute-to-items "count"))
         (all-item-config (plist-get config :all-item-config))
         (item-attributes (ht-get snapshot "item-attributes")))
    (dolist (item-id item-ids)
      (let ((item-config (ht-get all-item-config item-id)))
        (when item-config
          (insert (format "%-8s | %s\n"
                          (org-catalyst--get-item-attribute
                           snapshot item-id "count" 0)
                          (ht-get item-config "display-name"))))))))

(defun org-catalyst-setup-default-sections ()
  "Set `org-catalyst-status-sections' to be a good default."
  ;; TODO
  (setq org-catalyst-status-sections
        '(overview
          separator
          highest-chain
          separator
          highest-count)))

(defun org-catalyst--render-status ()
  ;; TODO
  "Render status window."
  (let* ((config (org-catalyst--get-config))
         (ui-data (org-catalyst--get-ui-data config))
         (snapshot (org-catalyst--compute-snapshot-after
                    ;; TODO: compute at any day
                    (org-catalyst--time-to-month-day)
                    config)))
    (dolist (section org-catalyst-status-sections)
      (funcall
       ;; TODO pull to constant
       (intern (concat "org-catalyst-section-"
                       (symbol-name section)))
       :config config
       :ui-data ui-data
       :snapshot snapshot))))

;;; Commands

(defun org-catalyst-complete-item (&optional arg)
  "TODO"
  ;; TODO: reward buffer
  ;; TODO: do some caching. this is slow
  (interactive "P")
  (let ((item-id
         (get-text-property
          0
          'property
          (completing-read
           "Complete item: "
           (org-catalyst--map-entries
            "item"
            (lambda ()
              (propertize (org-no-properties
                           (org-get-heading t t t t))
                          'property
                          (org-id-get-create))))
           nil ; predicate
           t ; require-match
           nil ; initial-input
           'org-catalyst--complete-item-history))))
    (let ((month-day (org-catalyst--read-month-day)))
      (org-catalyst--update-actions
       month-day
       (ht-set actions item-id 1)))))

(defun org-catalyst-recompute-history ()
  "Recompute all snapshots from the entire history."
  (interactive)
  ;; NOTE: probably update some view?
  (org-catalyst--update-cached-snapshots
   (org-catalyst--get-earliest-month-day)))

(defun org-catalyst-update-inline-info ()
  "TODO"
  (interactive))

(defun org-catalyst-save-game ()
  "Save the current game state."
  (interactive)
  (let ((game-saved nil))

    (unless (ht-empty? org-catalyst--modified-history-keys)
      (dolist (key (ht-keys org-catalyst--modified-history-keys))
        (let ((history (or (org-catalyst--lru-get
                            org-catalyst--buffered-histories key)
                           (org-catalyst--new-history))))
          (org-catalyst--save-history key history)))
      (ht-clear org-catalyst--modified-history-keys)
      (org-catalyst--lru-evict org-catalyst--buffered-histories
                               org-catalyst-history-cache-size)
      (org-catalyst--ensure-snapshots-correctness)
      (setq game-saved t))

    (unless (ht-empty? org-catalyst--modified-snapshot-keys)
      (dolist (key (ht-keys org-catalyst--modified-snapshot-keys))
        (let ((snapshot (or (org-catalyst--lru-get
                             org-catalyst--buffered-snapshots key)
                            (org-catalyst--new-snapshot))))
          (org-catalyst--save-snapshot key snapshot)))
      (ht-clear org-catalyst--modified-snapshot-keys)
      (org-catalyst--lru-evict org-catalyst--buffered-snapshots
                               org-catalyst-snapshot-cache-size)
      (setq game-saved t))

    (when game-saved
      (message "org-catalyst game saved."))))

(defun org-catalyst-status-refresh ()
  "Refresh Catalyst window."
  (interactive)
  ;; TODO: is this the best idea
  (let ((buffer (get-buffer-create org-catalyst-buffer-name)))
    (if (eq buffer (current-buffer))
        (save-excursion
          (let ((inhibit-read-only t))
            (erase-buffer)
            (org-catalyst--render-status)))
      (error "Not in Catalyst buffer"))))

(defun org-catalyst-status-quit ()
  "Quit Catalyst window."
  (interactive)
  (let ((buffer (get-buffer-create org-catalyst-buffer-name)))
    (if (eq buffer (current-buffer))
        (org-catalyst--restore-window-configuration)
      (error "Not in Catalyst buffer"))))

(defun org-catalyst-status ()
  "Open Catalyst status window."
  (interactive)
  ;; TODO make quit window and start window customizable
  (let ((buffer (get-buffer-create org-catalyst-buffer-name)))
    (unless buffer
      (error "Open Catalyst buffer failed"))

    (unless (eq buffer (current-buffer))
      (with-current-buffer buffer
        (org-catalyst--save-window-configuration))

      (let ((window (funcall org-catalyst-display-buffer-function buffer)))
        (let* ((old-frame (selected-frame))
               (new-frame (window-frame window)))
          (select-window window)
          (unless (eq old-frame new-frame)
            (select-frame-set-input-focus new-frame))

          (org-catalyst-mode)
          (goto-char (point-min))
          (org-catalyst-status-refresh))))))

;;; Minor modes

(define-minor-mode org-catalyst-auto-save-mode
  "Automatically save Catalyst game."
  :init-value nil
  :global t
  :group 'org-catalyst

  (when (not org-catalyst-auto-save-mode) ;; OFF
    (cancel-timer org-catalyst--auto-save-timer)
    (remove-hook 'kill-emacs-query-functions
                 #'org-catalyst--emacs-quit-query-function))

  (when org-catalyst-auto-save-mode ;; ON
    (setq org-catalyst--auto-save-timer
          (run-with-idle-timer org-catalyst-auto-save-idle-delay t
                               #'org-catalyst-save-game))
    (add-hook 'kill-emacs-query-functions
              #'org-catalyst--emacs-quit-query-function)))

;;; Major modes

;; TODO: decide between `make-sparse-keymap' and `make-keymap'
;; TODO: evil integration. maybe motion only (will need to make sure q is not used in motion).
;; or manually set every evil key.
(defvar org-catalyst-mode-map
  (let ((map (make-sparse-keymap)))
    (org-catalyst-setup-status-bindings nil map)
    map))

(define-derived-mode org-catalyst-mode
  special-mode "Org Catalyst"
  "Major mode for Catalyst."
  (toggle-truncate-lines 1))

;;; Footer

(provide 'org-catalyst)

;;; org-catalyst.el ends here
