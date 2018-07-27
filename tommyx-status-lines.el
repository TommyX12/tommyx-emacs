;; imports
(require 'powerline)
(require 'powerline-evil)
(require 'spaceline)
(require 'spaceline-config)
(require 'spaceline-all-the-icons)
(require 'which-func)

;; misc config
(setq powerline-default-separator 'slant)
(setq spaceline-byte-compile t)
(spaceline-toggle-which-function-off)
(spaceline-toggle-minor-modes-off)
(spaceline-toggle-hud-on)
(spaceline-spacemacs-theme)
(spaceline-helm-mode)
(spaceline-info-mode)
(spaceline-compile)

;; delayed update trigger for performance
(setq delayed-mode-line-updating nil)
(defun delayed-mode-line-format (func cache-name)
  "The mode-line format function with auto caching."
  (when delayed-mode-line-updating
    (set-window-parameter nil cache-name (funcall func)))
  (window-parameter nil cache-name))
(defun delayed-mode-line-update (&rest _)
  "Update the mode-line."
  (setq delayed-mode-line-updating t)
    (force-mode-line-update t)
    (run-at-time 0.01 nil (lambda () (setq delayed-mode-line-updating nil))))
(defun delayed-mode-line-update-if-idle (&rest _)
  "Update the mode-line if idling."
  (when (and (current-idle-time) (>= (nth 1 (current-idle-time)) 0.5))
    (delayed-mode-line-update)))
; update event triggers
(run-with-idle-timer 0.5 t 'delayed-mode-line-update)
(run-at-time 0 1 'delayed-mode-line-update-if-idle)
(add-hook 'window-configuration-change-hook 'delayed-mode-line-update)
;; (advice-add 'select-window :after #'delayed-mode-line-update) ; TODO avy-jump calls this too much

;; header line definition
(setq status-lines-header-segments-left `(
  (window-number :face highlight-face)
  (buffer-modified buffer-id remote-host)
  (persp-name)
  (workspace-number)
  (purpose :priority 94)
))
(setq status-lines-header-segments-right `(
  (which-function)
  (anzu :priority 95)
  (mu4e-alert-segment :when active)
  (erc-track :when active)
  (org-pomodoro :when active)
  (org-clock :when active)
  (nyan-cat)
  (python-pyvenv :fallback python-pyenv)
  (battery :when active)
  (selection-info :priority 95)
  (input-method)
  ((point-position
    line-column)
    :priority 96)
  (global :when active)
  (buffer-position :priority 99)
  (hud :priority 99)
))
(defun status-lines-compile-header ()
  (spaceline-compile 'status-header
    status-lines-header-segments-left
    status-lines-header-segments-right)
)

;; footer line definition
(setq status-lines-footer-segments-left `(
  (major-mode :priority 79)
  ((flycheck-error flycheck-warning flycheck-info)
    :when active
    :priority 89)
))
(setq status-lines-footer-segments-right `(
  (auto-compile)
  (process :when active)
  (version-control :when active :priority 78)
  (buffer-size)
  (buffer-encoding-abbrev)
))
(defun status-lines-compile-footer ()
  (spaceline-compile 'status-footer
    status-lines-footer-segments-left
    status-lines-footer-segments-right)
)

;; header line setup
(status-lines-compile-header)
(setq-default header-line-format '("%e" (:eval (delayed-mode-line-format #'spaceline-ml-status-header 'status-lines-header-cache))))

;; footer line setup
(status-lines-compile-footer)
(setq-default mode-line-format '("%e" (:eval (delayed-mode-line-format #'spaceline-ml-status-footer 'status-lines-footer-cache))))
