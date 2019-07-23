;; delayed update trigger for performance
(setq delayed-mode-line-enabled t)
(setq delayed-mode-line--updating nil)
(setq delayed-mode-line--selected-window nil)
(setq delayed-mode-line--temp-var nil)
(put 'delayed-mode-line--temp-var 'risky-local-variable t)
(defun delayed-mode-line-format (func cache-name)
  "The mode-line format function with auto caching."
  (when (or (not delayed-mode-line-enabled)
            delayed-mode-line--updating)
    (let ((powerline-selected-window delayed-mode-line--selected-window))
      (set-window-parameter nil cache-name (format-mode-line (funcall func)))))
	(setq delayed-mode-line--temp-var (window-parameter nil cache-name))
  'delayed-mode-line--temp-var)
(defun delayed-mode-line-update (&rest _)
  "Update the mode-line."
  (unless (and (boundp 'evil-state)
               (eq evil-state 'insert))
    (setq delayed-mode-line--updating t)
    (setq delayed-mode-line--selected-window (selected-window))
    (force-mode-line-update t)))
(defun delayed-mode-line-update-if-idle (&rest _)
  "Update the mode-line if idling."
  (when (and (current-idle-time) (>= (nth 1 (current-idle-time)) 0.5))
    (delayed-mode-line-update)))
(defun delayed-mode-line-pre-command (&rest _)
  "Reset updating state."
  (setq delayed-mode-line--updating nil))
                                        ; update event triggers
(run-with-idle-timer 0.5 t 'delayed-mode-line-update)
(run-at-time 0 5 'delayed-mode-line-update-if-idle)
(add-hook 'window-configuration-change-hook 'delayed-mode-line-update)
(add-hook 'pre-command-hook #'delayed-mode-line-pre-command)
;; (advice-add 'select-window :after #'delayed-mode-line-update) ; TODO avy-jump calls this too much


(provide 'tommyx-status-lines-def)

;;; tommyx-status-lines-def.el ends here
