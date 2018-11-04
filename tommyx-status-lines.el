;; imports
(require 'cl-lib)
(require 'powerline)
(require 'powerline-evil)
(require 'spaceline)
(require 'spaceline-config)
(require 'spaceline-all-the-icons)
(require 'which-func)
(require 'all-the-icons)

;; misc config
(setq powerline-default-separator 'slant)
(setq spaceline-byte-compile t)
(spaceline-spacemacs-theme)
(spaceline-helm-mode)
(spaceline-info-mode)
(spaceline-compile)
(setq status-lines-scale 1.15)

;; solve bug about MacOS status line separator
(setq powerline-image-apple-rgb t)
(setq ns-use-srgb-colorspace t)

;; delayed update trigger for performance
(setq delayed-mode-line--updating nil)
(setq delayed-mode-line--temp-var nil)
(put 'delayed-mode-line--temp-var 'risky-local-variable t)
(defun delayed-mode-line-format (func cache-name)
  "The mode-line format function with auto caching."
  (when delayed-mode-line--updating
    (set-window-parameter nil cache-name (format-mode-line (funcall func))))
	(setq delayed-mode-line--temp-var (window-parameter nil cache-name))
  'delayed-mode-line--temp-var)
(defun delayed-mode-line-update (&rest _)
  "Update the mode-line."
  (setq delayed-mode-line--updating t)
    (force-mode-line-update t)
    (run-at-time 0.01 nil (lambda () (setq delayed-mode-line--updating nil))))
(defun delayed-mode-line-update-if-idle (&rest _)
  "Update the mode-line if idling."
  (when (and (current-idle-time) (>= (nth 1 (current-idle-time)) 0.5))
    (delayed-mode-line-update)))
; update event triggers
(run-with-idle-timer 0.5 t 'delayed-mode-line-update)
(run-at-time 0 1 'delayed-mode-line-update-if-idle)
(add-hook 'window-configuration-change-hook 'delayed-mode-line-update)
;; (advice-add 'select-window :after #'delayed-mode-line-update) ; TODO avy-jump calls this too much


;; helper functions
(defun set-powerline-scale (scale)
	(setq powerline-height (round (* (frame-char-height) scale))))

(defun status-lines--git-stats (icon text face)
  "Wrapper to render git statistics ICON with TEXT using FACE.
When FAMILY is provided, put `:family' property into face."
  (let* ((family (all-the-icons-icon-family icon))
         (height 1.0)
         (icon-face `(:foreground ,(spaceline-all-the-icons--face-foreground face)
                      :height ,(spaceline-all-the-icons--height height))))

    (when family (setq icon-face (append `(:family ,family) icon-face)))
    (concat
     ;; (propertize icon 'face icon-face)
		 ;; (format "%s" text)
     (propertize (format "%s" text)
                 'face `(:foreground ,(spaceline-all-the-icons--face-foreground face)
												:height 1.0))
		 )))


;; segment definition
(spaceline-define-segment status-lines-line-column
  "The current line and column numbers, or `(current page/number of pages)`
in pdf-view mode (enabled by the `pdf-tools' package)."
  (if (eq major-mode 'pdf-view-mode)
		(spaceline--pdfview-page-number)
		(concat
		 (propertize (format-mode-line "%l") 'face 'bold)
		 ":%2c")))
(spaceline-define-segment status-lines-major-mode
  "The name of the major mode."
  (propertize (powerline-major-mode) 'face 'bold))
(spaceline-define-segment status-lines-mode-icon-colored
  "A segment indicating the current buffer's mode with an icon"
  (let ((icon (all-the-icons-icon-for-mode major-mode)))
    (unless (symbolp icon)
      (propertize icon
                  ;; 'help-echo (format "Major-mode: `%s'" major-mode)
                  'display '(raise 0)))))

(spaceline-define-segment status-lines-mode-icon
  "A segment indicating the current buffer's mode with an icon"
  (let ((icon (all-the-icons-icon-for-mode major-mode)))
    (unless (symbolp icon)
      (propertize icon
                  ;; 'help-echo (format "Major-mode: `%s'" major-mode)
                  'display '(raise 0)
                  'face `(; :height ,(spaceline-all-the-icons--height 1.1)
                          :family ,(all-the-icons-icon-family-for-mode major-mode)
                          :inherit)))))

(spaceline-define-segment status-lines-git-status
  "A segment to display Added/Removed stats for files under git VC."
  (cl-destructuring-bind (added removed modified) (spaceline-all-the-icons--git-statistics)
    (cl-destructuring-bind (added-icon removed-icon modified-icon) (spaceline-all-the-icons-icon-set-git-stats)
      (let* ((space "|")
             (icons (list
                     (unless (zerop added) (status-lines--git-stats added-icon added 'git-gutter:added))
                     (unless (zerop removed) (status-lines--git-stats removed-icon removed 'git-gutter:deleted))
                     (unless (zerop modified) (status-lines--git-stats modified-icon modified 'git-gutter:modified)))))
        (propertize
         (mapconcat 'identity (cl-remove-if 'not icons) space)
					'display '(raise 0)
         ;; 'help-echo "View Diff of current file"
         ;; 'mouse-face (spaceline-all-the-icons--highlight)
         ;; 'local-map (make-mode-line-mouse-map 'mouse-1 'vc-ediff
				))))

  :face mode-line
  :when (not (equal '(0 0 0) (spaceline-all-the-icons--git-statistics))))


;; header line definition
(setq status-lines-header-segments-left `(
  (window-number :face highlight-face :priority 99)
  ((buffer-modified status-lines-mode-icon-colored buffer-id remote-host) :face mode-line)
  (purpose :priority 94)
))
(setq status-lines-header-segments-right `(
  (anzu :priority 95)
  (nyan-cat)
  (selection-info :priority 95)
  (input-method)
	(which-function)
  (status-lines-line-column :priority 99)
  (buffer-position)
  (hud :priority 99)
))

;; footer line definition
(setq status-lines-footer-segments-left `(
  (status-lines-mode-icon)
  (status-lines-major-mode :priority 99)
  ((flycheck-error flycheck-warning flycheck-info) :priority 89)
))
(setq status-lines-footer-segments-right `(
  (python-pyvenv :fallback python-pyenv)
  (mu4e-alert-segment :when active)
  (erc-track :when active)
  ;; (global :when active)
  (auto-compile)
  (process)
  ;; (version-control :when active :priority 78)
  (status-lines-git-status :priority 78)
	(projectile-root)
  (buffer-size)
  (buffer-encoding-abbrev)
))

;; compilation function definition
(defun status-lines-compile-header ()
	(let ((powerline-default-separator 'slant)
				(spaceline-separator-dir-left '(left . right))
				(spaceline-separator-dir-right '(left . right)))
  (spaceline-compile 'status-header
    status-lines-header-segments-left
    status-lines-header-segments-right)))
(defun status-lines-compile-footer ()
	(let ((powerline-default-separator 'slant)
				;; (spaceline-separator-dir-left '(left . left))
				;; (spaceline-separator-dir-right '(right . right)))
				(spaceline-separator-dir-left '(left . right))
				(spaceline-separator-dir-right '(right . left)))
		(spaceline-compile 'status-footer
			status-lines-footer-segments-left
			status-lines-footer-segments-right)))
(defun status-lines-compile ()
	(set-powerline-scale status-lines-scale)
	(status-lines-compile-header)
	(status-lines-compile-footer))
(status-lines-compile)

;; header line setup
(setq-default header-line-format '("%e" (:eval (delayed-mode-line-format #'spaceline-ml-status-header 'status-lines-header-cache))))

;; footer line setup
(setq-default mode-line-format '("%e" (:eval (delayed-mode-line-format #'spaceline-ml-status-footer 'status-lines-footer-cache))))
