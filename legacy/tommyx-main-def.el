
;; input response (experimental)
;; (setq input-feedback-ov nil)
;; (defun before-insert-advice (&rest _)
;;  "Flash input feedback."
;;  ;; (when input-feedback-ov
;;  ;;  (delete-overlay input-feedback-ov)
;;  ;; )
;;  ;; (when (eq evil-state 'insert)
;;  ;;  (setq input-feedback-ov (make-overlay (point) (- (point) 1)))
;;  ;;  (overlay-put input-feedback-ov 'priority 9999)
;;  ;;  (overlay-put input-feedback-ov 'window (selected-window))
;;  ;;  (overlay-put input-feedback-ov 'face 'evil-goggles-yank-face)
;;  ;;  (redisplay)
;;  ;; )
;; )
(setq eager-redisplay-allowed t)
;; (defun eager-redisplay-insert-advice (&rest _)
;;  (when (and (eq evil-state 'insert) eager-redisplay-allowed)
;;    (redisplay t)))
(defun eager-redisplay-post-command (&rest _)
  (when (and (eq this-command 'self-insert-command) eager-redisplay-allowed)
    (redisplay t)))
(defun eager-redisplay-inhibit-advice (func &rest args)
  (let ((eager-redisplay-allowed nil))
    (apply func args)))
;; (advice-add 'self-insert-command :before #'before-insert-advice)
(defvar eager-redisplay-mode-on nil)
(defvar eager-redisplay-inhibit-cmd
  '(evil-repeat
    yas-expand
    evil-execute-macro))
(defun eager-redisplay-mode (&optional arg)
  "Minor mode that force redraw after command.

If ARG is positive, enable the mode; otherwise, disable the mode.
If ARG is non-nil, toggle the mode."
  (interactive)
  (if (if arg
          (<= arg 0)
        eager-redisplay-mode-on)
      (progn
        (setq eager-redisplay-mode-on nil)
        ;; (advice-remove 'self-insert-command #'eager-redisplay-insert-advice)
        (remove-hook 'post-command-hook #'eager-redisplay-post-command)
        (dolist (cmd eager-redisplay-inhibit-cmd)
          (advice-remove cmd #'eager-redisplay-inhibit-advice))
        (message "eager-redisplay mode disabled."))
    (progn
      (setq eager-redisplay-mode-on t)
      ;; (advice-add 'self-insert-command :after #'eager-redisplay-insert-advice)
      (add-hook 'post-command-hook #'eager-redisplay-post-command)
      (dolist (cmd eager-redisplay-inhibit-cmd)
        (advice-add cmd :around #'eager-redisplay-inhibit-advice))
      (message "eager-redisplay mode enabled."))))

;; highlight insert region (disabled for performance)
(setq hl-insert-region-ov nil)
(defun hl-insert-region-insert-entry ()
  (setq hl-insert-region-ov (make-overlay (point) (point) nil nil t))
  (overlay-put hl-insert-region-ov 'priority 99)
  (overlay-put hl-insert-region-ov 'window (selected-window))
  (overlay-put hl-insert-region-ov 'face 'vhl/default-face))
(defun hl-insert-region-insert-exit ()
  (when hl-insert-region-ov
    (delete-overlay hl-insert-region-ov)))
(defvar hl-insert-region-mode-on nil)
(defun hl-insert-region-mode ()
  (interactive)
  (if hl-insert-region-mode-on
      (progn
        (setq hl-insert-region-mode-on nil)
        (remove-hook 'evil-insert-state-entry-hook 'hl-insert-region-insert-entry)
        (remove-hook 'evil-insert-state-exit-hook 'hl-insert-region-insert-exit)
        (message "hl-insert-region mode disabled."))
    (progn
      (setq hl-insert-region-mode-on t)
      (add-hook 'evil-insert-state-entry-hook 'hl-insert-region-insert-entry)
      (add-hook 'evil-insert-state-exit-hook 'hl-insert-region-insert-exit)
      (message "hl-insert-region mode enabled."))))
;; (hl-insert-region-mode)

(defmacro tommyx-config-layer (name &rest body)
  "Define a configuration layer with NAME and BODY."
  (declare (indent 1))
  `(progn ,@body))


(provide 'tommyx-main-def)

;;; tommyx-main-def.el ends here
