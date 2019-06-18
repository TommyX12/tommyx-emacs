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

;;; Constants

(defconst org-catalyst--days-per-month 31)

;;; Variables

(defvar org-catalyst--buffered-snapshots (ht-create)
  "In-memory buffer for all snapshot objects.")
(defvar org-catalyst--buffered-histories (ht-create)
  "In-memory buffer for all history objects.")
(defvar org-catalyst--modified-snapshot-keys (ht-create)
  "Keys of modified snapshots.")
(defvar org-catalyst--modified-history-keys (ht-create)
  "Keys of modified histories.")
(defvar org-catalyst--auto-save-timer nil
  "Timer for `org-catalyst-auto-save-mode'.")

;;; Functions

(defun org-catalyst--save-object (object file)
  "Save OBJECT to FILE."
  (unless (file-writable-p file)
    (error "File %s is not writable" file))

  (let ((json-object-type 'hash-table))
    (f-write-text (json-encode object) 'utf-8 file)))

(defun org-catalyst--load-object (file)
  "Load OBJECT from FILE."
  ;; TODO: this is not efficient, but might handle encoding better.
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
  (let* ((month-index (car month-day))
         (year (/ month-index 12))
         (month (1+ (- month-index (* year 12)))))
    (format "%04d-%02d" year month)))

(defun org-catalyst--key-to-snapshot-path (key)
  "Return file path of the snapshot at time denoted by KEY."
  (f-join org-catalyst-save-path
          (concat key "-snapshot.json")))

(defun org-catalyst--key-to-history-path (key)
  "Return file path of the history at time denoted by KEY."
  (f-join org-catalyst-save-path
          (concat key "-history.json")))

(defun org-catalyst--update-function (snapshot actions)
  "TODO")

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

(defun org-catalyst--get-actions-at (month-day)
  "Return the actions at MONTH-DAY. TODO")

(defun org-catalyst--compute-snapshot-at (month-day)
  "Return the snapshot value at the start of MONTH-DAY."
  ;; TODO: base case. need a way to identify the first month. either read from the directory, or set it yourself.
  (let ((month-index (car month-day))
        (day-index (cdr month-day)))
    (if (= day-index 0) ; start of the month
        (let ((key (org-catalyst--month-day-to-key month-day)))
          (if (ht-contains-p org-catalyst--buffered-snapshots key)
              (ht-get org-catalyst--buffered-snapshots key)
            (let ((snapshot (org-catalyst--load-object
                             (org-catalyst--key-to-snapshot-path key))))
              (if snapshot
                  (org-catalyst--update-cached-snapshot
                   key snapshot t)
                (setq snapshot (org-catalyst--compute-snapshot-at
                                (cons (1- month-index)
                                      (1- org-catalyst--days-per-month))))
                (org-catalyst--update-cached-snapshot key snapshot))
              snapshot)))
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

(defun org-catalyst--update-cached-snapshot (key snapshot &optional no-mark-modified)
  "TODO"
  (ht-set org-catalyst--buffered-snapshots key snapshot)
  (unless no-mark-modified
    (ht-set org-catalyst--modified-snapshot-keys key t)))

(defun org-catalyst--compute-snapshot-after (month-day)
  "Return the snapshot value at the end of MONTH-DAY."
  (org-catalyst--compute-snapshot-at
   (org-catalyst--next-month-day month-day)))

(defun org-catalyst--update-cached-snapshots (from-month-day)
  "Update all cached snapshots as if edits were made during FROM-MONTH-DAY."
  ;; TODO
  (let ((cur-month-index (1+ (car month-day)))
        (now-month-index (car (org-catalyst--time-to-month-day))))
    (while (<= cur-month-index now-month-index)
      (org-catalyst--update-cached-snapshot
       (org-catalyst--month-day-to-key (cons cur-month-index 0))
       (org-catalyst--compute-snapshot-at
        (cons (1- cur-month-index)
              (1- org-catalyst--days-per-month)))))))

;;; Commands

(defun org-catalyst-complete-item (&optional arg)
  "TODO"
  (interactive "P"))

(defun org-catalyst-update-history ()
  "TODO"
  (interactive))

(defun org-catalyst-update-inline-info ()
  "TODO"
  (interactive))

(defun org-catalyst-save-game ()
  "TODO"
  (interactive)
  (let ((game-saved nil))
    (unless (ht-empty? org-catalyst--modified-snapshot-keys)
      (dolist (key (ht-keys org-catalyst--modified-snapshot-keys))
        (let ((snapshot (ht-get org-catalyst--buffered-snapshots key)))
          (org-catalyst--save-snapshot key snapshot)))
      (ht-clear org-catalyst--modified-snapshot-keys)
      (ht-clear org-catalyst--buffered-snapshots)
      (setq game-saved t))
    (unless (ht-empty? org-catalyst--modified-history-keys)
      (dolist (key (ht-keys org-catalyst--modified-history-keys))
        (let ((history (ht-get org-catalyst--buffered-histories key)))
          (org-catalyst--save-history key history)))
      (ht-clear org-catalyst--modified-history-keys)
      (ht-clear org-catalyst--buffered-histories)
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
