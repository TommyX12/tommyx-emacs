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
(require 'imenu-list)
(require 'eon)
(require 'ht)

(defvar easy-layout-active-layout-config nil)
(defvar easy-layout-active-wset nil)

(defmacro easy-layout-constructor (&rest body)
  `(lambda (buffer-name)
     (with-current-buffer (get-buffer-create buffer-name)
       ,@body)))

(defconst easy-layout-view-configs
  (eon 'terminal
       (eon :buffer-name "*terminal*"
            :constructor
            (lambda (buffer-name)
              (save-window-excursion
                (ansi-term
                 (read-from-minibuffer
                  "Start terminal: "
                  (or explicit-shell-file-name
                      (getenv "ESHELL")
                      shell-file-name))
                 (substring buffer-name 1
                            (1- (length buffer-name)))))))
       ;; TODO: this has bugs and issues
       ;; 'files
       ;; (eon :buffer-name "*Files*"
       ;;      :constructor
       ;;      (lambda (buffer-name)
       ;;        (save-window-excursion
       ;;          (setq neo-buffer-name buffer-name)
       ;;          (neotree-show))))
       'compilation
       (eon :buffer-name "*compilation*"
            :constructor
            (easy-layout-constructor
             (compilation-mode)))
       'outline
       (eon :buffer-name "*Outline*"
            :constructor
            (lambda (buffer-name)
              (setq imenu-list-buffer-name buffer-name)
              (imenu-list-get-buffer-create)
              (imenu-list-start-timer)))
       'code
       (eon :buffer-name nil)
       'help
       (eon :buffer-name "*Help*"
            :constructor
            (easy-layout-constructor
             (help-mode)))
       'elisp-scratch
       (eon :buffer-name "*scratch*"
            :constructor
            (easy-layout-constructor
             (lisp-interaction-mode)))
       'messages
       (eon :buffer-name "*Messages*"
            :constructor
            (easy-layout-constructor
             (message-mode)))))

(defconst easy-layout-layout-configs
  (eon 'ide
       (eon :name "Integrated Development Environment"
            :properties
            (eon 'persist-compilation-window t)
            :views
            (list (eon :id 'code
                       :main t)
                  ;; (eon :id 'files
                  ;;      :dedicated t)
                  (eon :id 'outline
                       :dedicated t)
                  (eon :id 'terminal
                       :dedicated t)
                  (eon :id 'compilation
                       :dedicated t))
            :layout
            '(- (:lower-size 12)
                (| (:right-size 25)
                   code
                   outline)
                (| (:left-size-ratio 0.5)
                   terminal
                   compilation)))
       'Outline
       (eon :name "Outline"
            :properties
            (eon 'persist-compilation-window nil)
            :views
            (list (eon :id 'code
                       :main t)
                  (eon :id 'outline
                       :dedicated t))
            :layout
            '(| (:right-size 25)
                code outline))
       'emacs-lisp-debug
       (eon :name "Emacs Lisp Debug"
            :properties
            (eon 'persist-compilation-window nil)
            :views
            (list (eon :id 'code
                       :main t)
                  (eon :id 'elisp-scratch
                       :dedicated t
                       :on-init
                       (lambda ()
                         (goto-char (point-max))
                         (recenter)))
                  (eon :id 'help
                       :dedicated t)
                  (eon :id 'messages
                       :dedicated t
                       :on-init
                       (lambda ()
                         (goto-char (point-max))
                         (recenter))))
            :layout
            '(| (:left-size-ratio 0.5)
                code
                (- (:upper-size-ratio 0.6666)
                   (- (:upper-size-ratio 0.5)
                      elisp-scratch
                      help)
                   messages)))
       'free-editing
       (eon :name "Free Editing"
            :properties
            (eon 'persist-compilation-window nil)
            :views (list (eon :id 'code))
            :layout 'full-screen)))

(defun easy-layout-setup-sidebar ()
  "TODO"
  )

(defun easy-layout-finalize (layout-config wset)
  (when (and layout-config wset)
    (dolist (view (eon-get layout-config (:views)))
      (let* ((view-id (eon-get view (:id)))
             (window (wlf:get-window wset view-id))
             (buffer (wlf:get-buffer wset view-id))
             (on-init (eon-get view (:on-init))))
        (when (eon-get view (:main))
          (wlf:select wset view-id))
        (when window
          (set-window-dedicated-p window
                                  (eon-get view (:dedicated))))
        (when (and window buffer on-init)
          (with-selected-window window
            (with-current-buffer buffer
              (funcall on-init))))))))

(defun easy-layout-get-property (property &optional default layout)
  (let ((layout-config
         (if layout
             (eon-get easy-layout-layout-configs (layout))
           easy-layout-active-layout-config)))
    (if layout-config
        (eon-get layout-config
                 (:properties property)
                 default)
      default)))

(defun easy-layout-switch (layout)
  (interactive
   (list (get-text-property
          0
          'property
          (completing-read
           "Switch to layout: "
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
  (let* ((inhibit-redisplay t)
         (buffer (current-buffer))
         (layout-config (eon-get easy-layout-layout-configs (layout))))
    (unless layout-config
      (error "Layout %s not found" (prin1-to-string layout)))
    (let* ((views (eon-get layout-config (:views)))
           wset)
      (dolist (view (eon-get layout-config (:views)))
        (let* ((view-id (eon-get view (:id)))
               (view-config (eon-get easy-layout-view-configs (view-id))))
          (unless view-config
            (error "View %s not found" (prin1-to-string view-id)))
          (let ((buffer-name (eon-get view-config (:buffer-name))))
            (when (and buffer-name
                       (not (get-buffer buffer-name)))
              (funcall (eon-get view-config (:constructor))
                       buffer-name)))))
      (let ((recipe (eon-get layout-config (:layout))))
        (cond
         ((eq recipe 'full-screen)
          (delete-other-windows))
         (t
          (setq wset
                (wlf:layout
                 recipe
                 (mapcar (lambda (view)
                           (let ((view-id (eon-get view (:id))))
                             (list :name view-id
                                   :buffer (eon-get easy-layout-view-configs
                                                    (view-id :buffer-name)))))
                         views))))))
      (easy-layout-finalize layout-config wset)
      (setq easy-layout-active-layout-config layout-config
            easy-layout-active-wset wset))))

(defun easy-layout-refresh ()
  (interactive)
  (if easy-layout-active-wset
      (progn
        (wlf:refresh easy-layout-active-wset)
        (easy-layout-finalize easy-layout-active-layout-config
                              easy-layout-active-wset))
    (message "No active layout")))

(provide 'tommyx-layout)

;;; tommyx-layout.el ends here
