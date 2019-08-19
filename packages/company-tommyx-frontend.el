;;; company-tommyx-frontend.el --- Use a posframe as company candidate menu

;; Copyright (C) 2017-2018 Free Software Foundation, Inc.

;; Author: Clément Pit-Claudel, Feng Shu
;; Maintainer: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/company-tommyx-frontend
;; Package-Version: 20190626.759
;; Version: 0.1.0
;; Keywords: abbrev, convenience, matching
;; Package-Requires: ((emacs "26.0")(company "0.9.0")(posframe "0.1.0"))

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

;; This is modified from `company-posframe'.

;; ** Tips
;; *** Work better with desktop.el
;; The below code let desktop.el not record the company-tommyx-frontend-mode
;; #+BEGIN_EXAMPLE
;; (require 'desktop) ;this line is needed.
;; (push '(company-tommyx-frontend-mode . nil)
;;       desktop-minor-mode-table)
;; #+END_EXAMPLE

;; ** Note
;; company-tommyx-frontend.el is derived from Clément Pit-Claudel's
;; company-tooltip.el, which can be found at:

;; https://github.com/company-mode/company-mode/issues/745#issuecomment-357138511


;;; Code:
;; * company-tommyx-frontend's code
(require 'cl-lib)
(require 'company)
(require 'posframe)

(defgroup company-tommyx-frontend nil
  "Use a child-frame as company candidate menu"
  :group 'company
  :prefix "company-tommyx-frontend")

(defcustom company-tommyx-frontend-font nil
  "The font used by company-tommyx-frontend's frame.
Using current frame's font if it it nil."
  :group 'company-tommyx-frontend)

(defcustom company-tommyx-frontend-width 50
  "The width of the completion window."
  :group 'company-tommyx-frontend
  :type 'integer)

(defcustom company-tommyx-frontend-height 10
  "The height of the completion window."
  :group 'company-tommyx-frontend
  :type 'integer)

(defcustom company-tommyx-frontend-lighter " company-tommyx-frontend"
  "The lighter string used by `company-tommyx-frontend-mode'."
  :group 'company-tommyx-frontend)

(defvar company-tommyx-frontend-buffer " *company-tommyx-frontend-buffer*"
  "company-tommyx-frontend's buffer which used by posframe.")
(defvar company-tommyx-frontend-notification "")
(defvar company-tommyx-frontend--last-status nil)
(defvar company-tommyx-frontend--hide-timer nil)
(defvar company-tommyx-frontend--active nil)

(defvar company-tommyx-frontend-active-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [mouse-1] 'ignore)
    (define-key keymap [mouse-3] 'ignore)
    (define-key keymap [down-mouse-1] 'ignore)
    (define-key keymap [down-mouse-3] 'ignore)
    (define-key keymap [up-mouse-1] 'ignore)
    (define-key keymap [up-mouse-3] 'ignore)
    (define-key keymap [wheel-down] 'ignore)
    (define-key keymap [wheel-up] 'ignore)
    keymap)
  "Keymap that is enabled during an active completion in posframe.")

(defun company-tommyx-frontend-show ()
  "Show company-tommyx-frontend candidate menu."
  (setq company-tommyx-frontend--active t)
  (company-tommyx-frontend-cancel-hide)
  (let* ((company-tooltip-minimum-width company-tommyx-frontend-width)
         (company-tooltip-maximum-width company-tommyx-frontend-width)
         (company-tooltip-limit company-tommyx-frontend-height)
         (height (min company-tooltip-limit company-candidates-length))
         (lines (company--create-lines company-selection height))
         ;; (width (length (car lines)))
         (width (+ 1 company-tooltip-margin company-tooltip-margin
                   company-tommyx-frontend-width))
         (contents (mapconcat #'identity lines "\n"))
         (buffer (get-buffer-create company-tommyx-frontend-buffer)))
    ;; FIXME: Do not support mouse at the moment, so remove mouse-face
    (setq contents (copy-sequence contents))
    (remove-text-properties 0 (length contents) '(mouse-face nil) contents)
    (with-current-buffer buffer
      (setq-local overriding-local-map company-tommyx-frontend-active-map))
    (posframe-show buffer
                   :internal-border-width 1
                   :string contents
                   :width width ;; TODO
                   :position (point)
                   :height company-tooltip-limit
                   :x-pixel-offset (* -1 company-tooltip-margin (default-font-width))
                   :font company-tommyx-frontend-font
                   :min-width company-tooltip-minimum-width
                   :background-color (face-attribute 'company-tooltip :background))
    (with-current-buffer buffer
      (unless truncate-lines
        (setq truncate-lines t)))))

(defun company-tommyx-frontend-hide ()
  "Hide company-tommyx-frontend candidate menu."
  (setq company-tommyx-frontend--active nil)
  (company-tommyx-frontend-cancel-hide)
  (posframe-hide company-tommyx-frontend-buffer))

(defun company-tommyx-frontend-delayed-hide ()
  (setq company-tommyx-frontend--active nil)
  (company-tommyx-frontend-cancel-hide)
  (setq company-tommyx-frontend--hide-timer
        (run-with-idle-timer 0.5 nil #'company-tommyx-frontend-hide)))

(defun company-tommyx-frontend-cancel-hide ()
  (when company-tommyx-frontend--hide-timer
    (cancel-timer company-tommyx-frontend--hide-timer)
    (setq company-tommyx-frontend--hide-timer nil)))

(defun company-tommyx-frontend (command)
  "TODO"
  (setq company-tommyx-frontend--last-status
        (list (selected-window)
              (current-buffer)))
  (cl-case command
    (pre-command nil)
    (hide (company-tommyx-frontend-delayed-hide))
    (post-command
     (unless company-tommyx-frontend--active
       (company-tommyx-frontend-show)))
    (update (company-tommyx-frontend-show))))

(defun company-tommyx-frontend-window-change ()
  "Hide posframe on window change."
  (unless (or (string= (buffer-name) company-tommyx-frontend-buffer)
              (equal company-tommyx-frontend--last-status
                     (list (selected-window)
                           (current-buffer))))
    (company-tommyx-frontend-hide)))

;;; Hooks

(add-hook #'evil-insert-state-exit-hook #'company-tommyx-frontend-hide)

(provide 'company-tommyx-frontend)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; company-tommyx-frontend.el ends here
