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
(require 'f)
(require 'ht)
(require 'json)
(require 'org)
(require 's)

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

;;; Structs

;; NOTE:
;; Reference:
;; https://stackoverflow.com/questions/6652956/how-can-i-implement-an-expiring-lru-cache-in-elisp
(defstruct org-catalyst--lru-cache size newest oldest table)
(defstruct org-catalyst--lru-item key value next prev)

;;; LRU Library

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

(defconst org-catalyst--days-per-month 31)
(defconst org-catalyst--snapshot-suffix "-snapshot.json")
(defconst org-catalyst--history-suffix "-history.json")

;;; Variables

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

;;; Macros

(defmacro org-catalyst--update-actions (month-day &rest body)
  "Evaluate BODY, which should update the actions at MONTH-DAY.
The actions object at MONTH-DAY will be bound to `actions'.
This marks the history containing MONTH-DAY as modified, and will perform
update on all cache after MONTH-DAY."
  `(let ((actions (org-catalyst--get-actions-at ,month-day)))
     ,@body
     (org-catalyst--maybe-set-earliest-month-day ,month-day)
     (ht-set org-catalyst--modified-history-keys
             (org-catalyst--month-day-to-key ,month-day) t)
     (org-catalyst--update-cached-snapshots ,month-day)))

;;; Functions

(defun org-catalyst--get-earliest-month-day ()
  "Return earliest month-day of all histories.
If undefined, will attempt to calculate from files in save directory,
and fallback to current month-day."
  (or org-catalyst--earliest-month-day
      (setq
       org-catalyst--earliest-month-day
       (cons
        (or
         (apply #'min
                (mapcar
                 (lambda (item)
                   (car
                    (org-catalyst--key-to-month-day
                     (substring (f-filename item) 0 7))))
                 (f-glob (concat "*" org-catalyst--history-suffix)
                         org-catalyst-save-path)))
         (car (org-catalyst--time-to-month-day)))
        0))))

(defun org-catalyst--maybe-set-earliest-month-day (month-day)
  "Set known earliest month-day to MONTH-DAY if existing value is later."
  (setq org-catalyst--earliest-month-day
        (cons
         (min (car (org-catalyst--get-earliest-month-day))
              (car MONTH-DAY))
         0)))

(defun org-catalyst--save-object (object file)
  "Save OBJECT to FILE."
  (unless (file-writable-p file)
    (error "File %s is not writable" file))

  (let ((json-object-type 'hash-table))
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
  "Return if catalyst has unsaved modifications."
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

(defun org-catalyst--month-day-to-key (month-day)
  "Convert a MONTH-DAY to key."
  (let* ((month-index (car month-day))
         (year (/ month-index 12))
         (month (1+ (- month-index (* year 12)))))
    (format "%04d-%02d" year month)))

(defun org-catalyst--key-to-month-day (key)
  "Convert a KEY to month-day."
  (let ((year (substring key 0 4))
        (month (substring key 5 7)))
    (cons (+ (* year 12) (1- month)) 0)))

(defun org-catalyst--key-to-snapshot-path (key)
  "Return file path of the snapshot at time denoted by KEY."
  (f-join org-catalyst-save-path
          (concat key org-catalyst--snapshot-suffix)))

(defun org-catalyst--key-to-history-path (key)
  "Return file path of the history at time denoted by KEY."
  (f-join org-catalyst-save-path
          (concat key org-catalyst--history-suffix)))

(defun org-catalyst--update-function (snapshot actions)
  "TODO")

(defun org-catalyst--new-snapshot ()
  "Construct a new empty snapshot."
  (ht-create))

(defun org-catalyst--new-action ()
  "Construct a new empty action."
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
    (if (>= day-index org-catalyst--days-per-month)
        (cons (1+ month-index) 0)
      (cons month-index (1+ day-index)))))

(defun org-catalyst--get-history-containing (month-day)
  "Return the history object containing MONTH-DAY."
  (let ((key (org-catalyst--month-day-to-key month-day)))
    (if (org-catalyst--lru-contains org-catalyst--buffered-histories key)
        (org-catalyst--lru-get org-catalyst--buffered-histories key)
      (let ((history (org-catalyst--load-object
                      (org-catalyst--key-to-history-path key))))
        (org-catalyst--lru-set
         org-catalyst--buffered-histories key
         (or history
             (org-catalyst--new-history)))
        history))))

(defun org-catalyst--get-actions-at (month-day)
  "Return the actions at MONTH-DAY."
  (let ((key (org-catalyst--month-day-to-key month-day))
        (history (org-catalyst--get-history-containing month-day)))
    (if (ht-contains-p history key)
        (ht-get history key)
      (org-catalyst--new-action))))

(defun org-catalyst--compute-snapshot-at (month-day)
  "Return the snapshot value at the start of MONTH-DAY."
  (let ((month-index (car month-day))
        (day-index (cdr month-day)))
    (if (= day-index 0) ; start of the month
        (if (>= month-index (org-catalyst--get-earliest-month-day))
            (let ((key (org-catalyst--month-day-to-key month-day)))
              (if (org-catalyst--lru-contains org-catalyst--buffered-snapshots
                                              key)
                  (org-catalyst--lru-get org-catalyst--buffered-snapshots
                                         key)
                (let ((snapshot (org-catalyst--load-object
                                 (org-catalyst--key-to-snapshot-path key))))
                  (if snapshot
                      (org-catalyst--update-cached-snapshot
                       month-day snapshot t)
                    (setq snapshot (org-catalyst--compute-snapshot-at
                                    (cons (1- month-index)
                                          org-catalyst--days-per-month)))
                    (org-catalyst--update-cached-snapshot month-day snapshot))
                  snapshot)))
          (org-catalyst--new-snapshot))
      ;; other days of the month
      (let ((cur-day-index 0)
            (cur-snapshot (org-catalyst--compute-snapshot-at
                           (cons month-index 0))))
        (while (< cur-day-index day-index)
          (setq cur-snapshot
                (org-catalyst--update-function
                 cur-snapshot
                 (org-catalyst--get-actions-at (cons month-index cur-day-index)))))
        cur-snapshot))))

(defun org-catalyst--compute-snapshot-after (month-day)
  "Return the snapshot value at the end of MONTH-DAY."
  (org-catalyst--compute-snapshot-at
   (org-catalyst--next-month-day month-day)))

(defun org-catalyst--update-cached-snapshot (month-day snapshot &optional no-mark-modified)
  "Set cached snapshot object at MONTH-DAY to SNAPSHOT.
If NO-MARK-MODIFIED is nil, KEY will be marked as modified,
 and will be saved when `org-catalyst-save-game' is called."
  (let ((key (org-catalyst--month-day-to-key month-day)))
    (org-catalyst--lru-set org-catalyst--buffered-snapshots
                           key snapshot)
    (unless no-mark-modified
      (ht-set org-catalyst--modified-snapshot-keys key t))))

(defun org-catalyst--update-cached-snapshots (from-month-day)
  "Update all cached snapshots as if edits were made during FROM-MONTH-DAY."
  (let ((cur-month-index (1+ (car month-day)))
        (now-month-index (car (org-catalyst--time-to-month-day))))
    (while (<= cur-month-index now-month-index)
      (org-catalyst--update-cached-snapshot
       (cons cur-month-index 0)
       (org-catalyst--compute-snapshot-at
        (cons (1- cur-month-index)
              org-catalyst--days-per-month))))))

;;; Commands

(defun org-catalyst-complete-item (&optional arg)
  "TODO"
  (interactive "P"))

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

    (unless (ht-empty? org-catalyst--modified-history-keys)
      (dolist (key (ht-keys org-catalyst--modified-history-keys))
        (let ((history (or (org-catalyst--lru-get
                            org-catalyst--buffered-histories key)
                           (org-catalyst--new-history))))
          (org-catalyst--save-history key history)))
      (ht-clear org-catalyst--modified-history-keys)
      (org-catalyst--lru-evict org-catalyst--buffered-histories
                               org-catalyst-history-cache-size)
      (setq game-saved t))

    (when game-saved
      (message "org-catalyst game saved."))))

;;; Minor modes

(define-minor-mode org-catalyst-auto-save-mode
  "Automatically save org-catalyst game."
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

;;; Footer

(provide 'org-catalyst)

;;; org-catalyst.el ends here
