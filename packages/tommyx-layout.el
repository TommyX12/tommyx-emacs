;;; tommyx-layout.el --- Easy layout system -*- lexical-binding: t -*-

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

;; TODO

;;; Code:

(require 'window-layout)
(require 'eon)
(require 'ht)

(defvar easy-layout-active-layout nil)
(defvar easy-layout-active-wset nil)

(defconst easy-layout-view-configs
  (eon 'terminal
       (eon :buffer-name "*terminal*"
            :constructor
            (lambda ()
              (save-window-excursion
                (ansi-term
                 (read-from-minibuffer
                  "Start terminal: "
                  (or explicit-shell-file-name
                      (getenv "ESHELL")
                      shell-file-name))
                 "terminal"))))
       'compilation
       (eon :buffer-name "*compilation*"
            :constructor
            (lambda () (get-buffer-create "*compilation*")))
       'code
       (eon :buffer-name nil)))

(defconst easy-layout-layout-configs
  (eon 'project-with-terminal
       (eon :name "Project with terminal"
            :properties
            (eon 'persist-compilation-window t)
            :views
            '(code terminal compilation)
            :layout
            '(- (:lower-size 12)
                code
                (| (:left-size-ratio 0.5)
                   terminal
                   compilation)))
       'free-editing
       (eon :name "Free editing"
            :views '(code)
            :layout 'full-screen)))

(defun easy-layout-get-property (property &optional default layout)
  (setq layout (or layout easy-layout-active-layout))
  (if layout
      (eon-get easy-layout-layout-configs
               (layout :properties property)
               default)
    default))

(defun easy-layout-switch (layout)
  (interactive
   (list (get-text-property
          0
          'property
          (completing-read
           "Enter layout: "
           (mapcar
            (lambda (layout)
              (let ((layout-config
                     (eon-get easy-layout-layout-configs (layout))))
                (propertize
                 (or (eon-get layout-config (:name))
                     (symbol-name layout))
                 'property layout)))
            (eon-keys easy-layout-layout-configs))
           nil ; predicate
           t ; require-match
           nil ; initial-input
           'easy-layout--select-layout-history))))
  ;; TODO
  (let* ((buffer (current-buffer))
         (layout-config (eon-get easy-layout-layout-configs (layout))))
    (unless layout-config
      (error "Layout %s not found" (prin1-to-string layout)))
    (let* ((views (eon-get layout-config (:views)))
           (main-view (car views))
           wset)
      (dolist (view (eon-get layout-config (:views)))
        (let ((view-config (eon-get easy-layout-view-configs (view))))
          (unless view-config
            (error "View %s not found" (prin1-to-string view)))
          (when (let ((buffer-name (eon-get view-config (:buffer-name))))
                  (and buffer-name
                       (not (get-buffer buffer-name))))
            (funcall (eon-get view-config (:constructor))))))
      (let ((recipe (eon-get layout-config (:layout))))
        (cond
         ((eq recipe 'full-screen)
          (delete-other-windows))
         (t
          (setq wset
                (wlf:layout
                 recipe
                 (mapcar (lambda (view)
                           (list :name view
                                 :buffer (eon-get easy-layout-view-configs
                                                  (view :buffer-name))))
                         views))))))
      (when wset
        (wlf:select wset main-view))
      (setq easy-layout-active-layout layout
            easy-layout-active-wset wset))))

(provide 'tommyx-layout)

;;; tommyx-layout.el ends here
