;; TODO: dependencies here
;; TODO: refactor lots of the hacks here

;; startup appearance

(defun setup-appearance ()
  ;; theme
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (setq dark-theme 'infinity-dark)
  (setq light-theme 'infinity-light)
  (if (bound-and-true-p use-light-theme)
      (load-theme light-theme t)
    (load-theme dark-theme t))
  ;; font
  (unless (bound-and-true-p selected-font)
    (cond
     ((and (eq system-type 'ms-dos)
           (find-font (font-spec :name "Consolas")))
      (setq selected-font "Consolas"))
     ((find-font (font-spec :name "Fira Mono"))
      (setq selected-font "Fira Mono"))
     ((find-font (font-spec :name "Source Code Pro"))
      (setq selected-font "Source Code Pro"))
     ((find-font (font-spec :name "DejaVu Sans Mono"))
      (setq selected-font "DejaVu Sans Mono"))
     (t
      (setq selected-font "Menlo"))))
  (unless (boundp 'font-size-small)
    (setq font-size-small 120))
  (unless (boundp 'font-size-big)
    (setq font-size-big 150))
  (set-face-attribute 'default nil
                      :family selected-font
                      :height font-size-small
                      :weight 'normal
                      :width 'normal))

;; scanning all org files

(defvar all-org-directory-files nil
  "List of all org files in org-directory.
Update with 'update-all-org-directory-files'.")

(defun find-org-files
    (&optional recurse dirs file-excludes dir-excludes)
  "Fill the variable `all-org-directory-files'.
Optional parameters:
  RECURSE        If t, scan the directory recusively.
  DIRS           A list of directories to scan for *.org files.
  FILE-EXCLUDES  Regular expression.  If a filename matches this regular
expression,
                 do not add it to `all-org-directory-files'.
  DIR-EXCLUDES   Regular expression.  If a directory name matches this
regular expression,
                 do not add it to `all-org-directory-files'."
  (let ((targets (or dirs (list org-directory)))
        (fex (or file-excludes  "^[#\\.].*$"))
        (dex (or dir-excludes  "^[#\\.].*$\\|archive\\|images"))
        path)
    (dolist (dir targets)
      (if (file-directory-p dir)
          (let ((all (directory-files dir nil "^[^#\\.].*$")))
            (dolist (f all)
              (setq path
                    (concat (file-name-as-directory dir) f))
              (cond
               ((file-directory-p path)
                (if (and recurse (not (string-match dex f)))
                    (find-org-files t (list path) fex dex)))
               ((and (string-match "^[^#\\.].*\\.org$" f)
                     (not (string-match fex f)))
                (setq all-org-directory-files
                      (append (list path) all-org-directory-files))))))
        (message "Not a directory: %s" path)))))

(defun update-all-org-directory-files ()
  "Add files org-refile targets recursively."
  (interactive)
  (setq all-org-directory-files nil)
  (find-org-files t))

;; org key binding helpers

(defun org-sort-checklist-compare-func (a b)
  (let ((a (if (string= a "-")
               " "
             a))
        (b (if (string= b "-")
               " "
             b)))
    (string-lessp a b)))

(defun org-sort-checklist-getkey-func ()
  (or (let* ((str (match-string 1))
             (len (length str)))
        (and (stringp str)
             (substring str
                        (- len 2)
                        (- len 1))))
			""))

(defun org-show-all-latex-fragments ()
  (interactive)
  (org-remove-latex-fragment-image-overlays)
  (org-toggle-latex-fragment '(16)))

(defun org-focus ()
  "Fold all subtrees except ones relevant to the current."
  (interactive)
  (org-overview)
  (org-reveal '(4))
  (org-show-entry)
  (org-show-children)
  (recenter nil))

;; org automatic capturing

(defconst org-auto-capture-delete-flag "# (deleted) ")
(defconst org-auto-capture-delete-flag-regexp "# (deleted) ")
(defconst org-auto-capture-heading-regexp "\\*+ ")

;; TODO: make these a defcustom
(defvar org-auto-capture-targets nil)

(defun org-auto-capture-immediately-finalize
    (capture-key content)
  "Capture content and immediately finalize."
  (org-capture-string content capture-key)
  (org-capture-finalize)
  '())
(defun org-auto-capture-get-target-map (auto-capture-targets)
  (let ((result (ht-create)))
    (dolist (config auto-capture-targets)
      (let ((capture-key (car config))
            (patterns (cadr config))
            (action (caddr config)))
        (dolist (pattern patterns)
          (when (ht-contains? result pattern)
            (error "Error: pattern \"%s\" already exists" pattern))
          (ht-set! result pattern config))))
    result))
(defun org-auto-capture-ignore
    (capture-key content)
  "Do nothing and move on to the next entry."
  '(:no-mark-delete t))
(defun org-auto-capture-mark-delete ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (insert org-auto-capture-delete-flag)))
(defun org-auto-capture-parse-line (line)
  ;; TODO
  (unless (string-match-p "^[ \t]*$" line)
    (string-match "^\\([^:]+\\):\\(.+\\)" line)
    (let ((pattern (match-string 1 line))
          (content (match-string 2 line)))
      (when (and pattern content)
        `(:pattern ,(string-trim (downcase pattern)) :content ,(string-trim content))))))
(defun org-auto-capture-delete-marked-subtree ()
  "Delete all lines marked to be deleted from current line to the end of current subtree."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (error "Buffer not in org mode"))
  (save-excursion
    (let ((last-point -1))
      (while (progn
               (beginning-of-line)
               (and (not (looking-at org-auto-capture-heading-regexp))
                    (not (= (point) last-point))))
        (if (looking-at org-auto-capture-delete-flag-regexp)
            (delete-region (point-at-bol) (1+ (point-at-eol)))
          (setq last-point (point))
          (next-logical-line))))))
(defun org-auto-capture-process-subtree ()
  "Perform auto capture from current line to the end of current subtree."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (error "Buffer not in org mode"))
  (let ((target-map
         (org-auto-capture-get-target-map org-auto-capture-targets))
        (loop-running t))
    (while loop-running
      (beginning-of-line)
      (cond
       ((looking-at org-auto-capture-delete-flag-regexp)
        (if (eobp)
            (setq loop-running nil)
          (next-logical-line)))
       ((looking-at org-auto-capture-heading-regexp)
        (setq loop-running nil))
       (t
        (let* ((parsed-line (org-auto-capture-parse-line
                             (buffer-substring-no-properties
                              (point-at-bol)
                              (point-at-eol))))
               (pattern (plist-get parsed-line :pattern))
               (content (plist-get parsed-line :content)))
          (cond
           ((null parsed-line)
            (if (eobp)
                (setq loop-running nil)
              (next-logical-line)))
           ((not (ht-contains? target-map (downcase pattern)))
            (message "Error: pattern \"%s\" does not exist" pattern)
            (next-logical-line))
           (t
            (let* ((config (ht-get target-map (downcase pattern)))
                   (capture-key (car config))
                   (action (caddr config))
                   (action-return (funcall action capture-key content)))
              (if (plist-get action-return :stop)
                  (setq loop-running nil)
                (unless (plist-get action-return :no-mark-delete)
                  (org-auto-capture-mark-delete))
                (next-logical-line))))))
        (redisplay))))))

(defface org-priority-1
  '((t :inherit error))
  "")
(defface org-priority-2
  '((t :inherit warning))
  "")
(defface org-priority-3
  '((t :inherit success))
  "")

(defun $setup-org-mode-local-pairs ()
  (sp-local-pair 'org-mode "$" "$")
  (sp-local-pair 'org-mode "\\[" "\\]")
  (sp-local-pair 'org-mode "\\(" "\\)"))

(defun $ask-for-clock-out-on-quit ()
  (defun my/org-clock-query-out ()
    "Ask the user before clocking out.
  This is a useful function for adding to `kill-emacs-query-functions'."
    (if (and
         (featurep 'org-clock)
         (funcall 'org-clocking-p)
         (y-or-n-p "You are currently clocking time, clock out? "))
        (org-clock-out) t)) ;; only fails on keyboard quit or error
  (add-hook 'kill-emacs-query-functions 'my/org-clock-query-out))

(defun org-agenda-special-prefix ()
  ;; extra is the agenda deadline / scheduled leader string
  (if (string-match "^@\\(.*\\)$" extra)
      (progn
        (let*
            ((days-string (match-string 1 extra))
             (days (string-to-number days-string))
             (blocks (min 8 (max 0 (- 8 (/ days 2)))))
             (spaces (- 8 blocks)))
          (concat
           days-string
           "|"
           (s-repeat blocks (if (>= days 0) "-" "="))
           (s-repeat spaces " ")
           "●")))
    extra))

(defvar org-notify-priority-map nil) ; plist map from priority to notify type
(defun org-notify-make-todo (heading &rest ignored)
  "Create one todo item."
  (cl-macrolet ((get (k) `(plist-get list ,k))
                (pr (k v) `(setq result (plist-put result ,k ,v))))
    (let* ((list (nth 1 heading))      (notify (or (get :NOTIFY) (plist-get org-notify-priority-map (get :priority)) "default"))
           (deadline (org-notify-convert-deadline (get :deadline)))
           (heading (get :raw-value))
           result)
      (when (and (eq (get :todo-type) 'todo) heading deadline)
        (pr :heading heading)     (pr :notify (intern notify))
        (pr :begin (get :begin))
        (pr :file (nth org-notify-parse-file (org-agenda-files 'unrestricted)))
        (pr :timestamp deadline)  (pr :uid (md5 (concat heading deadline)))
        (pr :deadline (- (org-time-string-to-seconds deadline)
                         (float-time))))
      result)))

(defun $remove-vc-hooks ()
  (remove-hook 'find-file-hooks 'vc-find-file-hook)
  (remove-hook 'find-file-hooks 'vc-refresh-state))

(defun $type-break-my-query-function (prompt)
  (yes-or-no-p
   (concat
    prompt
    (propertize (format "(!! %s !!) "
                        (get-random-element
                         type-break-health-quotes))))))

(defun $defer-hl-indentation-in-insert-mode ()
  (add-hook 'evil-insert-state-entry-hook
            (lambda ()
              (setq highlight-indentation--defer-redraw t)))
  (add-hook 'evil-insert-state-exit-hook
            (lambda ()
              (setq highlight-indentation--defer-redraw nil))))

(defun $disable-hl-line-in-insert-and-visual-mode ()
  (defvar-local $hl-line-prev-state nil)
  (dolist (state '("insert" "visual"))
    (add-hook (intern (concat "evil-" state "-state-exit-hook"))
              (lambda ()
                (when $hl-line-prev-state
                  (hl-line-mode $hl-line-prev-state)
                  (setq $hl-line-prev-state nil))))
    (add-hook (intern (concat "evil-" state "-state-entry-hook"))
              (lambda ()
                (setq-local $hl-line-prev-state (if hl-line-mode 1 -1))
                (hl-line-mode -1)))))

(defun $disable-trailing-whitespace-in-insert-mode ()
  (defvar-local $show-trailing-whitespace-temp nil)
  (add-hook 'evil-insert-state-entry-hook
            (lambda ()
              (setq-local $show-trailing-whitespace-temp
                          show-trailing-whitespace)
              (setq-local show-trailing-whitespace nil)))
  (add-hook 'evil-insert-state-exit-hook
            (lambda () (setq-local show-trailing-whitespace
                                   $show-trailing-whitespace-temp))))

(defun $disable-yascroll-in-insert-mode ()
  (add-hook 'evil-insert-state-entry-hook
            (lambda ()
              (setq yascroll:last-state yascroll-bar-mode)
              (yascroll-bar-mode -1)))
  (add-hook 'evil-insert-state-exit-hook
            (lambda ()
              (yascroll-bar-mode (if yascroll:last-state 1 -1)))))

(defun $show-yascroll-if-not-in-insert-mode ()
  (when (and (not (eq evil-state 'insert))
             yascroll-bar-mode)
    (yascroll:safe-show-scroll-bar)))

(defvar $flash-cursor--overlay nil)

(defun $flash-cursor-pre-command-hook ()
  (when $flash-cursor--overlay
    (delete-overlay $flash-cursor--overlay))
  (remove-hook 'pre-command-hook #'$flash-cursor-pre-command-hook))

(defun $flash-cursor (&rest _)
  (when $flash-cursor--overlay
    (delete-overlay $flash-cursor--overlay))
  (let ((ov (make-overlay (point) (- (point) 1))))
    (overlay-put ov 'priority 9999)
    (overlay-put ov 'window (selected-window))
    (overlay-put ov 'face 'cursor)
    (setq $flash-cursor--overlay ov)
    (add-hook 'pre-command-hook #'$flash-cursor-pre-command-hook)))

(defun $set-jump-on-insert-mode ()
  (add-hook 'evil-insert-state-entry-hook 'evil-set-jump))

(defun $setup-evil-args ()
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))

(defun $setup-helm-describe-modes ()
  (global-set-key [remap describe-mode] #'helm-describe-modes))

(defun $type-break-schedule-check (&rest _)
  (when (null type-break-time-next-break)
    (type-break-schedule)))

(defun $ivy-posframe-get-size ()
  "Functon used by `ivy-posframe-size-function'."
  (list
   :height ivy-posframe-height
   :width (+ (window-width) 6)
   :min-height (or ivy-posframe-min-height (+ ivy-height 1))
   :min-width (or ivy-posframe-min-width (round (* (frame-width) 0.62)))))

(defun $posframe-poshandler-point-horizontal (info &optional font-height)
  (let* ((x-pixel-offset (plist-get info :x-pixel-offset))
         (y-pixel-offset (plist-get info :y-pixel-offset))
         (posframe-width (plist-get info :posframe-width))
         (posframe-height (plist-get info :posframe-height))
         (window (plist-get info :parent-window))
         (xmax (plist-get info :parent-frame-width))
         (ymax (plist-get info :parent-frame-height))
         (position-info (plist-get info :position-info))
         (header-line-height (plist-get info :header-line-height))
         (x (+ (car (window-inside-pixel-edges window))
               x-pixel-offset))
         (y-top (+ (cadr (window-pixel-edges window))
                   header-line-height
                   (- (or (cdr (posn-x-y position-info)) 0)
                      ;; Fix the conflict with flycheck
                      ;; http://lists.gnu.org/archive/html/emacs-devel/2018-01/msg00537.html
                      (or (cdr (posn-object-x-y position-info)) 0))
                   y-pixel-offset))
         (font-height (or font-height (plist-get info :font-height)))
         (y-bottom (+ y-top font-height)))
    (cons (max 0 (min x (- xmax (or posframe-width 0))))
          (max 0 (if (> (+ y-bottom (or posframe-height 0)) ymax)
                     (- y-top (or posframe-height 0))
                   y-bottom)))))

(defun $posframe-poshandler-adaptive-top-bottom (info)
  "Posframe's position handler.

Get a position which let posframe stay onto current window's
top or bottom side without blocking center content.
Useful for a search overview popup."
  (let* ((posframe (plist-get info :posframe))
         (parent-frame (plist-get info :parent-frame))
         (frame-height (frame-pixel-height parent-frame))
         (window (plist-get info :parent-window))
         (window-left (window-pixel-left window))
         (window-top (window-pixel-top window))
         (window-height (window-pixel-height window))
         (posframe-height (frame-pixel-height posframe))
         (modeline-height (window-mode-line-height)))
    (cond
     (t; (<= (/ window-height posframe-height) 3.0)
      (if (<= (+ window-top (/ window-height 2.0)) (/ frame-height 2.0))
          (cons ; bottom
           window-left
           (min (- frame-height modeline-height posframe-height)
                (+ window-top window-height)))
        (cons ; top
         window-left
         (max 0
              (- window-top posframe-height)))))
     ;; (t
     ;;   (cons ; window bottom
     ;;     window-left
     ;;     (+ window-top window-height (- 0 modeline-height posframe-height)))
     ;; )
     )))

(defun $ivy-posframe-display-swiper (str)
  (ivy-posframe--display str #'$posframe-poshandler-adaptive-top-bottom))

(defun $ivy-posframe-display-at-point-horizontal (str)
  (ivy-posframe--display str #'$posframe-poshandler-point-horizontal))

(defun $bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings."
  (when (and
         (or (not (fboundp 'easy-layout-get-property))
             (not (easy-layout-get-property
                   'persist-compilation-window)))
         (buffer-live-p buffer)
         (string-match "compilation" (buffer-name buffer))
         (string-match "finished" string)
         (not
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "warning" nil t))))
    (run-with-timer 1 nil
                    (lambda (buf window)
                      (with-selected-window window
                        (quit-window)))
                    buffer
                    (get-buffer-window buffer))))

(defun $use-utf8-encoding ()
  (set-language-environment 'utf-8)
  ;; for old Carbon emacs on OS X only
  (set-keyboard-coding-system 'utf-8-mac)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (if (eq system-type 'windows-nt)
      ;; fix inability to paste non-ascii char
      (set-selection-coding-system 'utf-16-le)
    (set-selection-coding-system 'utf-8))
  (prefer-coding-system 'utf-8))

(defun $start-emacs-server ()
  (when window-system
    (server-start)))

(defun $volatile-highlights-undo-tree-setup ()
  (vhl/define-extension 'undo-tree
                        'undo-tree-move
                        'undo-tree-yank)
  (with-eval-after-load 'undo-tree
    (vhl/install-extension 'undo-tree)
    (vhl/load-extension 'undo-tree)))

(defun $which-key-window-number-filter (cell prefix)
  (cond
   ((string-match "move to window 1" (cdr cell))
    '("[0-9]" . "move to window [0-9]"))
   ((string-match "move to window [0-9]" (cdr cell))
    nil)
   (cell)))

(defun $extra-word-char (chars)
  "Recognize character in CHARS as word character."
  (dolist (char chars)
    (modify-syntax-entry char "w")))

(defun $remove-parens-overlay (&rest _)
  (sp-remove-active-pair-overlay))

(defun $remove-parens-overlay-on-insert-exit ()
  (add-hook 'evil-insert-state-exit-hook #'$remove-parens-overlay))

(defun $setup-smartparens-expansion ()
  (sp-local-pair 'prog-mode "{" nil :post-handlers
                 '(("||\n[i]" "RET")
                   ("||\n[i]" "<return>")
                   ("| " "SPC")
                   ("| " "<space>")))
  (sp-local-pair 'text-mode "{" nil :post-handlers
                 '(("||\n[i]" "RET")
                   ("||\n[i]" "<return>")
                   ("| " "SPC")
                   ("| " "<space>")))
  (sp-local-pair 'prog-mode "[" nil :post-handlers
                 '(("||\n[i]" "RET")
                   ("||\n[i]" "<return>")
                   ("| " "SPC")
                   ("| " "<space>")))
  (sp-local-pair 'text-mode "[" nil :post-handlers
                 '(("||\n[i]" "RET")
                   ("||\n[i]" "<return>")
                   ("| " "SPC")
                   ("| " "<space>")))
  (sp-local-pair 'prog-mode "(" nil :post-handlers
                 '(("||\n[i]" "RET")
                   ("||\n[i]" "<return>")
                   ("| " "SPC")
                   ("| " "<space>")))
  (sp-local-pair 'text-mode "(" nil :post-handlers
                 '(("||\n[i]" "RET")
                   ("||\n[i]" "<return>")
                   ("| " "SPC")
                   ("| " "<space>"))))

(defun $avy-handler-tommyx (char)
  (let (dispatch)
    (cond ((setq dispatch (assoc char avy-dispatch-alist))
           (setq avy-action (cdr dispatch))
           (throw 'done 'restart))
          ;; Let f and tab exit avy
          ((memq char '(?f ?\t 27 ?\C-g))
           ;; exit silently
           (throw 'done 'exit))
          ((eq char ??)
           (avy-show-dispatch-help)
           (throw 'done 'restart))
          ((mouse-event-p char)
           (signal 'user-error (list "Mouse event not handled" char)))
          (t
           (message "No such candidate: %s, hit `C-g' to quit."
                    (if (characterp char) (string char) char))))))

(defun $setup-ycmd-flycheck ()
  (add-hook 'ycmd-mode-hook 'flycheck-ycmd-setup))

(defun $setup-ycmd-eldoc ()
  (add-hook 'ycmd-mode-hook
            (lambda ()
              (when (ycmd-major-mode-to-file-types major-mode)
                (ycmd-eldoc-setup)))))

(defface sidebar-background
  '((t :inherit default))
  "*Face used for the sidebar."
  :group 'appearance)

(defface sidebar-fringe
  '((t :inherit fringe))
  "*Face used for the sidebar fringe."
  :group 'appearance)

(defface sidebar-hl-line
  '((t :inherit hl-line))
  "*Face used for the sidebar line highlight."
  :group 'appearance)

(defface color-identifiers-avoid-face-1
  '((t :foreground "#d53b9c34cd15"))
  "")

(defface color-identifiers-avoid-face-2
  '((t :foreground "#d53a9c359c35"))
  "")

(defface color-identifiers-avoid-face-3
  '((t :foreground "#ac809c34d53a"))
  "")

(defun $setup-color-identifiers-parser (style mode)
  (cond
   ((eq style 'c)
    (color-identifiers:set-declaration-scan-fn
     mode 'color-identifiers:cc-mode-get-declarations)
    (add-to-list
     'color-identifiers:modes-alist
     `(,mode . (""
                "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
                (nil font-lock-variable-name-face)))))
   ((eq style 'js)
    (add-to-list
     'color-identifiers:modes-alist
     `(,mode . ("[^.][[:space:]]*"
                "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
                (nil font-lock-variable-name-face)))))))

(defun $start-imenu-list-and-neotree ()
  (imenu-list-get-buffer-create)
  (imenu-list-start-timer)
  (imenu-list-update nil t)
  (neotree-show)
  ;; (display-buffer-in-side-window (get-buffer imenu-list-buffer-name) '((side . left)))
  )

(defun set-font-size (size)
  (set-face-attribute 'default nil :height size)
  (status-lines-compile) ; should be before companion
  (when (fboundp 'companion-reopen)
    (companion-reopen)))

(defun set-to-small-font ()
  (interactive)
  (set-font-size font-size-small))

(defun set-to-big-font ()
  (interactive)
  (set-font-size font-size-big))

(defun toggle-readable-buffer-font ()
  (interactive)
  (if (bound-and-true-p buffer-face-mode)
      (buffer-face-mode -1)
    (cond
     ((find-font (font-spec :name "Helvetica"))
      (buffer-face-set '(:family "Helvetica")))
     (t
      (buffer-face-set '(:family "Arial"))))))

(defun select-paste-region ()
  (interactive)
  (evil-goto-mark ?\[)
  (evil-visual-char)
  (evil-goto-mark ?\]))

(defun delete-line-content ()
  (interactive)
  (if (save-excursion
        (beginning-of-line)
        (looking-at "[ \t]*$"))
      (beginning-of-line)
    (evil-first-non-blank))
  (call-interactively 'evil-delete-line))

(defun smart-open-line-above ()
  (interactive)
  ;; (move-beginning-of-line nil)
  ;; (save-excursion
  ;;   (newline))
  ;; TODO: if we do not want to indent, comment this
  (save-excursion
    (newline)
    (unless (eolp)
      (indent-according-to-mode)))
  (indent-according-to-mode))

(defun update-heavy-tasks () (interactive)
       "Update all the heavy tasks."
       (message "Updating heavy tasks...")
       (color-identifiers:refresh)
       (font-lock-fontify-buffer)
       (flyspell-lazy-check-visible)
       (git-gutter:update-all-windows)
       (git-gutter)
       (flycheck-buffer)
       (garbage-collect)
       (yascroll:safe-show-scroll-bar)
       (message "Done.")
       (beacon-blink))

(defun execute-buffer-as-sh ()
  (interactive)
  (let (compile-command)
    (compile (buffer-string))))

(defun execute-region-as-sh ()
  (interactive)
  (let (compile-command)
    (compile (buffer-substring (region-beginning) (region-end)))))

(defun company-smart-complete ()
  (interactive)
  (if (yas-maybe-expand-abbrev-key-filter t)
      (yas-expand)
    (setq company-echo-metadata-frontend-bypass t)
    (cond
     (company-selection-changed
      (company-complete-selection))
     (company-candidates
      (company-select-next)
      (company-complete-selection))
     (t
      (company-auto-begin)
      (company-select-next)))))

(defun company-complete-number-1 ()
  (interactive) (company-complete-number 1))

(defun company-complete-number-2 ()
  (interactive) (company-complete-number 2))

(defun company-complete-number-3 ()
  (interactive) (company-complete-number 3))

(defun company-complete-number-4 ()
  (interactive) (company-complete-number 4))

(defun company-complete-number-5 ()
  (interactive) (company-complete-number 5))

(defun company-complete-number-6 ()
  (interactive) (company-complete-number 6))

(defun company-complete-number-7 ()
  (interactive) (company-complete-number 7))

(defun company-complete-number-8 ()
  (interactive) (company-complete-number 8))

(defun company-complete-number-9 ()
  (interactive) (company-complete-number 9))

(defun company-complete-number-0 ()
  (interactive) (company-complete-number 10))

(defun get-random-element (list)
  "Returns a random element of LIST."
  (if (and list (listp list))
      (nth (random (1- (1+ (length list)))) list)
    (error "Argument to get-random-element not a list or the list is empty")))

(defun sort-lines-ignore-case ()
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively #'sort-lines)))

(evil-define-command evil-ex-search-next-flash () :repeat nil
  (evil-ex-search-next)
  ($flash-cursor))

(evil-define-command evil-ex-search-previous-flash () :repeat nil
  (evil-ex-search-previous)
  ($flash-cursor))

(evil-define-command evil-ex-search-word-forward-flash () :repeat nil
  (evil-ex-search-word-forward 1)
  ($flash-cursor))

(evil-define-command evil-ex-search-word-backward-flash () :repeat nil
  (evil-ex-search-word-backward 1)
  ($flash-cursor))

(evil-define-command evil-visualstar/begin-search-forward-flash () :repeat nil
  (call-interactively 'evil-visualstar/begin-search-forward)
  ($flash-cursor))

(evil-define-command evil-visualstar/begin-search-backward-flash () :repeat nil
  (call-interactively 'evil-visualstar/begin-search-backward)
  ($flash-cursor))

(setq-default use-line-nav nil)
(evil-define-motion adaptive-avy () :type exclusive :repeat nil :jump t
  (if use-line-nav (evil-avy-goto-line) (evil-avy-goto-word-0 nil)))

(evil-define-motion fast-move-up () :type exclusive
  (evil-previous-visual-line 5))

(evil-define-motion fast-move-down () :type exclusive
  (evil-next-visual-line 5))

(evil-define-motion fast-move-left () :type exclusive
  (evil-backward-char 8))

(evil-define-motion fast-move-right () :type exclusive
  (evil-forward-char 8))

(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun selection-or-word-at-point (&optional rg-symbol)
  (cond
   ;; If there is selection use it
   ((and transient-mark-mode
         mark-active
         (not (eq (mark) (point))))
    (let ((mark-saved (mark))
          (point-saved (point)))
      (deactivate-mark)
      (buffer-substring-no-properties mark-saved point-saved)))
   ;; Otherwise, use word at point or empty
   (t
    (save-excursion
      (when (not (looking-at "\\sw"))
        (while (and (> (point) (point-min)) (= (char-before) ? ))
          (backward-char)))
      (format (if rg-symbol
                  "\\b%s\\b"
                "\\<%s\\>")
              (or (word-at-point)
                  ""))))))

(evil-define-motion swiper-movement () :type exclusive :repeat nil :jump t
  (swiper))

(evil-define-command evil-noh-blink () :repeat nil
  (evil-ex-nohighlight) (beacon-blink))

(evil-define-command evil-comfortable-recenter () :repeat nil
  (recenter-top-bottom (/ (* (window-total-height) 2) 7)))

(evil-define-command evil-delete-backward-line ()
  (let ((first-non-blank (save-excursion (back-to-indentation)
                                         (point))))
    (if (<= (point) first-non-blank)
        (delete-region (line-beginning-position)
                       (point))
      (delete-region first-non-blank
                     (point)))))

(defun change-theme (theme)
  "Change to a new theme."
  (interactive)
  (load-theme theme t)
  (status-lines-compile)
  (when (fboundp 'companion-reopen)
    (companion-reopen))
  (unless (eq system-type 'darwin)
    (posframe-delete-all)))

(defun pop-kill-ring ()
  "Remove most recent entry from kill-ring"
  (when kill-ring
    (setq kill-ring (cdr kill-ring)))
  (when kill-ring-yank-pointer
    (setq kill-ring-yank-pointer kill-ring)))

(defun call-with-command-hooks (command &optional enforce-keys bypass-tng)
  "Call command, invoking pre-command and post-command hooks of company.

company-tng-frontend only update on 'pre-command-hook',
so this is used to make dispatched commands triggered by
general-key-dispatch also trigger a 'pre-command-hook'.
Since company-tng-frontend modifies 'this-command' to complete selection,
this function also makes sure if 'pre-command-hook' modifies 'this-command',
the new command is called instead.

ENFORCE-KEYS is set to the key sequence for this command to enforce
the value of 'this-command-keys' for general-key-dispatch key sequence
during 'post-command-hook'.  It is observed that, when company-tng unreads
key sequence to complete selection, 'this-command-keys' for the actual
command (ran after) is mysteriously incorrect.

If BYPASS-TNG is non-nil, make sure tng frontend does not trigger a completion.
This is useful for commands that will call `company-complete-selection'."
  (let ((old-command this-command))
    (let (($company-bypass-tng bypass-tng))
      (setq this-command command)
      (run-hooks 'pre-command-hook)
      (call-interactively this-command)
      (when (and (eq command this-command) enforce-keys)
        (set--this-command-keys enforce-keys))
      (run-hooks 'post-command-hook)
      (setq this-command old-command))))

(defun insert-todo () (interactive)
       (insert "TODO"))

(defun insert-backslash () (interactive)
       (insert "\\"))

(defun paste-from-default-register () (interactive)
       ;; (evil-paste-from-register ?\")
       (yank))

(defun self-insert-or-send-raw (string)
  (interactive)
  (if (eq major-mode 'term-mode)
      (term-send-raw-string string)
    (self-insert-command 1)))

(evil-define-motion evil-sp-forward-sexp () :type exclusive
  (sp-forward-sexp))

(evil-define-motion evil-sp-backward-sexp () :type exclusive
  (sp-backward-sexp))

(evil-define-motion evil-org-up-heading () :type exclusive
  (if (org-at-heading-p)
      (org-up-heading-safe)
    (org-back-to-heading)))
(evil-define-motion evil-outline-next-heading () :type exclusive
  (outline-next-heading))
(evil-define-motion evil-org-backward-heading-same-level () :type exclusive
  (if (org-at-heading-p)
      (org-backward-heading-same-level 1)
    (org-back-to-heading)))
(evil-define-motion evil-org-forward-heading-same-level () :type exclusive
  (org-forward-heading-same-level 1))
(evil-define-motion evil-org-next-visible-heading () :type exclusive
  (org-next-visible-heading 1))
(evil-define-motion evil-org-previous-visible-heading () :type exclusive
  (org-previous-visible-heading 1))

(defun $load-external-org-config-if-exist ()
  (let ((external-config-path
         (expand-file-name "org-config.el" org-directory)))
    (when (file-exists-p external-config-path)
      (load external-config-path))))

(defun peek-region-in-split ()
  "Doesn't work.  Improve."
  (interactive)
  (split-window (selected-window) 1 'below)
  (switch-to-buffer (clone-indirect-buffer nil nil))
  (call-interactively 'narrow-to-region)
  (fit-window-to-buffer)
  (goto-char 0)
  (windmove-down))

(defun outline-up-heading-custom ()
  (interactive)
  (if (outline-on-heading-p)
      (outline-up-heading 1)
    (outline-back-to-heading)))

(defun outline-backward-heading-same-level-custom ()
  (interactive)
  (if (outline-on-heading-p)
      (outline-backward-same-level 1)
    (outline-back-to-heading)))

(defun $improve-fringe-bitmaps ()
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [#b00000000
     #b00000000
     #b00001110
     #b00011111
     #b00011111
     #b00011111
     #b00001110
     #b00000000
     #b00000000])
  (define-fringe-bitmap 'right-arrow
    [#b01110000
     #b00111000
     #b00011100
     #b00001110
     #b00001110
     #b00011100
     #b00111000
     #b01110000])
  (define-fringe-bitmap 'left-arrow
    [#b00001110
     #b00011100
     #b00111000
     #b01110000
     #b01110000
     #b00111000
     #b00011100
     #b00001110])
  (define-fringe-bitmap 'right-curly-arrow
    [#b00011000
     #b00011000
     #b00011000
     #b00011000
     #b00011000
     #b00011000
     #b00011000
     #b00011000
     #b00011000
     #b00011000
     #b00011000
     #b00011000
     #b00011000
     #b00011000
     #b00011000
     #b00011000])
  (define-fringe-bitmap 'left-curly-arrow
    [#b00011000
     #b00011000
     #b00011000
     #b00011000
     #b00011000
     #b00011000
     #b00011000
     #b00011000
     #b00011000
     #b00011000
     #b00011000
     #b00011000
     #b00011000
     #b00011000
     #b00011000
     #b00011000]))

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

(defvar python-auto-format-code t)
(defun python-format-code ()
  (when python-auto-format-code
    (elpy-format-code)))

(defun counsel--find-org-files-matcher (regexp candidates)
  (seq-filter
   (lambda (x)
     (string-match-p "\\.org$" x))
   (counsel--find-file-matcher regexp candidates)))

(defun counsel-find-org-files ()
  (interactive)
  ;; TODO make possible to search for org files only
  (unless org-directory
    (error "org-directory is nil"))
  (let ((counsel-projectile-find-file-matcher
         #'counsel--find-org-files-matcher))
    (counsel-projectile-switch-project-action-find-file
     org-directory)))

(defvar counsel-org-rg-initial-input "")

(defun counsel-org-rg ()
  (interactive)
  (unless org-directory
    (error "org-directory is nil"))
  (let ((counsel-projectile-rg-initial-input
         (concat "-g '*.org' -- " counsel-org-rg-initial-input)))
    (counsel-projectile-switch-project-action-rg
     org-directory)))

(defun $ivy-avy ()
  (interactive)
  (if (bound-and-true-p ivy-posframe--display-p)
      (ivy-posframe-avy)
    (ivy-avy)))

(provide 'tommyx-extensions)

;;; tommyx-extensions.el ends here
