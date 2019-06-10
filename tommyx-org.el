;; requires
(require 's)
(require 'ht)
(require 'evil)
(require 'alert)
(require 'org-super-agenda)
(require 'org-pomodoro)
(require 'org-bullets)
(require 'org-notify)
(require 'ox-publish)
(require 'companion)
(require 'hydra)
(require 'smartparens)

;; startup settings
(add-hook 'org-mode-hook (lambda () (interactive)
	                         (setq-local indent-tabs-mode nil) ; use space instead of tabs
	                         (setq-local tab-width 2)
	                         (setq-local evil-shift-width tab-width)
                           ))

;; use clean (indented) view
(setq org-startup-indented t)
(setq org-startup-folded nil)

;; use full path in refile (TODO buggy)
;; (setq org-refile-use-outline-path t)
;; (setq org-outline-path-complete-in-steps t)

;; logging
(setq org-log-done 'time)
(setq org-log-into-drawer t)

;; use readable font in org mode
;; (add-hook 'org-mode-hook #'enable-readable-buffer-font)

;; check if org-notes-dir exists.
;; (when (boundp 'org-agenda-dir)

;; lists
(setq org-list-allow-alphabetical t)

;; ivy integration
(setq counsel-org-headline-display-style 'path)
(setq counsel-org-headline-path-separator "/")

;; tag
(setq org-tags-column 0)

;; bullet
(setq org-bullets-bullet-list '("●"))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; links
(setq org-descriptive-links t)
(setq org-link-file-path-type 'relative)
;; (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
(setq org-id-link-to-org-use-id nil)

;; habit
(add-to-list 'org-modules 'org-habit)

;; latex
(setq org-highlight-latex-and-related '(latex))
(setq org-startup-with-latex-preview t)
(sp-local-pair 'org-mode "$" "$")
(sp-local-pair 'org-mode "\\[" "\\]")
(sp-local-pair 'org-mode "\\(" "\\)")
(setq org-format-latex-options
      `(
        :foreground default
        :background default
        :scale ,(if (eq system-type 'darwin)
                    1.6
                  1.0)
        :html-foreground "Black"
        :html-background "Transparent"
        :html-scale 1.0
        :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
(setq org-preview-latex-default-process 'dvipng)
(setq org-format-latex-header-default
      org-format-latex-header)
(setq org-format-latex-header
      (concat
       org-format-latex-header-default
       "\n\\DeclareMathOperator*{\\argmax}{arg\\,max}"
       "\n\\DeclareMathOperator*{\\argmin}{arg\\,min}"))
;; (setq org-preview-latex-default-process 'imagemagick)

;; clocking
                                        ; ask for clock-out on leave
(setq org-clock-report-include-clocking-task t)
(setq org-clock-persist t)
(setq org-clock-history-length 25)
(org-clock-persistence-insinuate)
(defun my/org-clock-query-out ()
	"Ask the user before clocking out.
	This is a useful function for adding to `kill-emacs-query-functions'."
	(if (and
		   (featurep 'org-clock)
		   (funcall 'org-clocking-p)
		   (y-or-n-p "You are currently clocking time, clock out? "))
		  (org-clock-out) t)) ;; only fails on keyboard quit or error
(add-hook 'kill-emacs-query-functions 'my/org-clock-query-out) ; timeclock.el puts this on the wrong hook!

;; todo dependencies
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)

;; images
(setq org-image-actual-width nil)

;; misc
(setq org-M-RET-may-split-line nil)
(setq org-fontify-done-headline t)
(setq org-fontify-whole-heading-line t)
(setq org-super-agenda-fontify-whole-header-line t) ; this doesn't work
(setq org-imenu-depth 4)
;; new version of org has angular brackets which messes with rainbow delimiters
(add-hook 'org-mode-hook
          (lambda ()
            (modify-syntax-entry ?< ".")
            (modify-syntax-entry ?> ".")))

;; agenda configs
(setq org-agenda-dim-blocked-tasks t)
(setq org-agenda-window-setup 'only-window)
(setq org-agenda-start-with-clockreport-mode t)
(setq org-agenda-start-with-log-mode t)
(setq org-agenda-log-mode-items '(closed clock state))
(setq org-agenda-restore-windows-after-quit t)
(setq org-agenda-use-tag-inheritance t) ; tag inheritance in agenda (turn off if slow)
(setq org-agenda-sticky t) ; should manually refresh agenda
(setq org-agenda-span 'day)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
(setq org-agenda-move-date-from-past-immediately-to-today t)
(org-super-agenda-mode)
(evil-set-initial-state 'org-agenda-mode 'motion)
(setq org-agenda-time-grid '((daily today)
                             (800 1000 1200 1400 1600 1800 2000)
                             "......" "----------------"))
(setq org-agenda-entry-text-leaders "    > ")
(defun org-agenda-special-prefix ()
                                        ; extra is the agenda deadline / scheduled leader string
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
		extra)
  )
(setq org-agenda-prefix-format
	    '(
		    (agenda  . " %i %?-12t %(org-agenda-special-prefix) %-12:c")
		    ;; (agenda  . " %i %?-12t % s %-12:c")
		    (todo  . " %i %-12:c")
		    (tags  . " %i %-12:c")
		    (search . " %i %-12:c")))
(setq org-agenda-timerange-leaders '("" "(%d/%d): "))
(setq org-agenda-scheduled-leaders '("[S]        : " "[S]     -%2d: "))
;; (setq org-agenda-deadline-leaders '("[D]      : " "[D]    %2d: " "[D]   -%2d: "))
(setq org-agenda-deadline-leaders '("@0  " "@%-3d" "@-%-2d"))
(setq org-agenda-inactive-leader "[")

;; entry text filter
                                        ; remove blank lines and state logs
(setq org-agenda-entry-text-exclude-regexps '("^- State.*\n" "^[ \t]*\n"))

;; checks if org-notes-dir exists.
;; )

;; agenda notification for companion
                                        ; patch org-notify to allow notification type determined by priority
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
(companion-notif-create-stream 'org-notify 120)
(defun org-notify-action-alert (plist)
	"A org-notify action for showing notification using alert.el."
	(alert
	 (plist-get plist :heading)
	 :severity (plist-get plist :severity)
	 :data `(:duration ,(plist-get plist :duration)
			               :stream ,(plist-get plist :stream)
		                 )
	 :id (plist-get plist :id)
	 )
  )
(org-notify-add 'default '(:time "1h" :actions nil :period "2m" :duration 60))

;; automatic capturing

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

;; key binding helpers

(defun org-up-heading-custom ()
  (interactive)
  (if (org-at-heading-p)
      (org-up-heading-safe)
    (org-back-to-heading)))

(defun org-backward-heading-same-level-custom ()
  (interactive)
  (if (org-at-heading-p)
      (org-backward-heading-same-level 1)
    (org-back-to-heading)))

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

;; hydra
(defhydra hydra-org-nav ()
  "org heading navigation"
  ("h" org-up-heading-custom "parent heading")
  ("l" outline-next-heading "next heading")
  ("k" org-backward-heading-same-level-custom "previous heading same level")
  ("j" org-forward-heading-same-level "next heading same level"))

;;; key bindings

(evil-define-motion evil-org-next-visible-heading () :type exclusive
	(org-next-visible-heading 1))
(evil-define-motion evil-org-previous-visible-heading () :type exclusive
	(org-previous-visible-heading 1))

(global-leader-navigation-def
	:keymaps 'org-mode-map
	:states '(motion normal visual)

  "s" '(counsel-org-goto
        :which-key "org goto headings"))

(global-leader-org-def
	:states '(motion normal)

  "h" '(helm-org-rifle-org-directory
        :which-key "helm org rifle")

	"f" '(counsel-org-goto-all ; go to heading of opened org files
		    :which-key "org goto all")

	"r" '((lambda () (interactive) (org-refile '(4)))
		    :which-key "org goto refile targets")

	"c" '(counsel-org-capture
		    :which-key "org capture")

	"C" '(org-capture
		    :which-key "org capture")

	"a" '(org-agenda
		    :which-key "org agenda")

	"i" '((lambda () (interactive) (org-clock-in '(4))) ; (lambda () (interactive) (org-pomodoro '(4)))
		    :which-key "org clock in recent")

	"I" '(org-clock-goto
		    :which-key "org goto clock")

	"o" '(org-clock-out
		    :which-key "org clock out")

	"O" '(org-clock-cancel
		    :which-key "org clock cancel")

  "l" '(org-insert-link-global
        :which-key "insert org link")

  "L" '(org-store-link
        :which-key "store org link"))

(global-leader-mode-specific-def
	:keymaps 'org-mode-map
	:states '(motion normal visual)

	"h" '(org-toggle-heading
		    :which-key "org toggle heading/text")

	"-" '(org-toggle-item
		    :which-key "org toggle item type")

	"[" '(org-toggle-checkbox
		    :which-key "org toggle checkbox")

	"]" '((lambda () (interactive) (org-toggle-checkbox '(4)))
		    :which-key "org toggle checkbox presence")

	"f" '(counsel-org-goto
		    :which-key "org goto")

  ";" '(org-sort
        :which-key "org sort")

  ":" '(org-sort-list
        :which-key "org list"))

(global-leader-mode-specific-def
  :keymaps 'org-mode-map
	:states '(motion normal)

	"r" '(org-refile
		    :which-key "org refile")

	"c" '(org-copy
		    :which-key "org copy")

	"t" '(org-todo
		    :which-key "org todo")

	"T" '(org-shiftleft
		    :which-key "org todo back")

	"x" '(org-ctrl-c-ctrl-c
		    :which-key "org update")

	"s" '(org-schedule
		    :which-key "org schedule")

  "e" '(org-set-effort
        :which-key "org set effort")

  "e" '(org-set-effort
        :which-key "org set effort")

  "E" '(org-inc-effort
        :which-key "org increase effort")

  "v" '(:ignore t
                :which-key "org view and export")

  "vl" '(org-show-all-latex-fragments
         :which-key "preview latex fragment")

  "vi" '(org-redisplay-inline-images
         :which-key "display inline images")

  "vI" '(org-remove-inline-images
         :which-key "remove inline images")

  "vL" '((lambda () (interactive)
           (org-remove-latex-fragment-image-overlays))
         :which-key "remove latex fragment")

  "vc" '(org-clock-display
         :which-key "clock display")

  "vC" '(org-clock-remove-overlays
         :which-key "remove clock display")

  "vd" '(org-update-all-dblocks
         :which-key "update dynamic blocks")

	"C-s" '((lambda () (interactive) (org-todo "TODAY"))
		      :which-key "org set TODAY")

	"C-S-s" '((lambda () (interactive) (org-todo "TODO"))
		        :which-key "org remove TODAY")

	"S" '((lambda () (interactive) (org-schedule nil "+1d"))
		    :which-key "org schedule to tomorrow")

	"d" '(org-deadline
		    :which-key "org deadline")

	"D" '((lambda () (interactive) (org-deadline nil "+1w"))
		    :which-key "org deadline to 1w")

	"p" '(org-priority
		    :which-key "org priority")

	"P" '(org-set-property
		    :which-key "org set property")

	"q" '(org-set-tags-command
		    :which-key "org set tags")

	"i" '(org-clock-in ; org-pomodoro
		    :which-key "org clock in")

  "%" '((lambda () (interactive)
          (insert "[%]")
          (org-ctrl-c-ctrl-c)
          (insert " "))
        :which-key "org insert % cookie")

  "/" '((lambda () (interactive)
          (insert "[/]")
          (org-ctrl-c-ctrl-c)
          (insert " "))
        :which-key "org insert / cookie")

  "l" '(org-insert-link ; also allow editing link
        :which-key "org insert link")

  "L" '(org-store-link
        :which-key "org store link")

  "o" '(org-open-at-point
        :which-key "org open link")

  "C-l" '(org-toggle-link-display
          :which-key "org toggle link display"))

(general-define-key
 :keymaps 'org-mode-map
 :states '(motion normal visual insert)

 "<tab>" 'org-cycle)

(general-define-key
 :keymaps 'org-mode-map
 :states '(motion normal)

 ;; using evil-collection with org mode:
 ;;
 ;; notes:
 ;; can use zc, zo, zO etc.
 ;; many of these are dot-repeatable.
 ;;
 ;; TAB: toggle show or hide, navigate table
 ;; C-TAB: cycle visibility
 ;;
 ;; C-c C-t: make into todo / cycle todo states
 ;; C-c C-s: add / change scheduled start
 ;; C-c C-d: add / change deadline
 ;; C-c C-w: refile (move to)
 ;; C-c ,: add / change priority
 ;; C-c .: enter / modify timestamp (date only)
 ;; C-u C-c .: enter / modify timestamp (with time)
 ;; C-c C-.: enter / modify inactive (no agenda) timestamp (date only)
 ;; C-c C-.: enter / modify inactive (no agenda) timestamp (date only)
 ;;
 ;; C-c C-d: refile (move subtree to)
 ;;
 ;; C-c C-c:
 ;; refresh item under cursor
 ;; toggle state of checkbox
 ;; edit tag of item
 ;;
 ;; C-c [: add current file to agenda file list (DO NOT USE)
 ;;
 ;; C-c c: initiate org capture. can be used everywhere.

 ;; remove bindings
 "M-h" nil
 "M-j" nil
 "M-k" nil
 "M-l" nil

 "C-c C-." 'org-time-stamp-inactive ; with C-u as previx also add time.

 "_" 'org-shiftdown ; change date like speed-dating
 "+" 'org-shiftup

 "C-_" 'org-shiftleft
 "C-+"'org-shiftright

 "C-S-h" 'org-shiftmetaleft ; promote/outdent
 "C-S-j" 'org-metadown ; move down
 "C-S-k" 'org-metaup ; move up
 "C-S-l" 'org-shiftmetaright ; demote/indent

 "<M-return>" 'org-insert-heading-respect-content
 "<M-S-return>" 'org-insert-todo-heading-respect-content
 "<C-return>" 'org-insert-subheading
 "<C-S-return>" 'org-insert-todo-subheading

 "C-g" (lambda () (interactive) (outline-hide-subtree))
 ;; (kbd "C-j") 'org-next-visible-heading
 ;; (kbd "C-k") 'org-previous-visible-heading
 ;; ;; (kbd "C-l") (lambda () (interactive) (outline-show-entry) (outline-show-children))
 "C-;" 'org-cycle

 "X" 'org-show-all
 "Z" 'org-focus
 ;; Just use shift-tab itself, or C-u tab
 ;; "Z" org-shifttab ; cycle global visibility

 "t" 'hydra-org-nav/body

 "C-h" 'org-up-heading-custom
 "C-l" 'outline-next-heading
 "C-k" 'org-backward-heading-same-level-custom
 "C-j" 'org-forward-heading-same-level)

(general-define-key
 :keymaps 'org-mode-map
 :states '(insert)

 ;; using evil-collection with org mode:
 ;;
 ;; M-RET: create heading at same level
 ;; M-S-RET: create TODO heading at same level
 ;; C-RET: create heading at same level below current one (most useful)
 ;; C-S-RET: create TODO heading at same level below current one
 ;;
 ;; TAB and S-TAB: go through table fields
 ;; RET: table next row

 "C-c C-." 'org-time-stamp-inactive ; with C-u as previx also add time.
 "M-RET" 'org-meta-return
 "M-S-RET" 'org-insert-todo-heading
 "C-RET" 'org-insert-subheading
 "C-S-RET" 'org-insert-todo-subheading
 "<M-return>" 'org-meta-return
 "<M-S-return>" 'org-insert-todo-heading
 "<C-return>" 'org-insert-subheading
 "<C-S-return>" 'org-insert-todo-subheading)

(general-define-key
 :keymaps 'org-mode-map
 :states '(visual)

 ;; promote/outdent
 "C-S-h" (lambda () (interactive)
           (org-metaleft)
           (evil-visual-restore))
 ;; demote/indent
 "C-S-l" (lambda () (interactive)
           (org-metaright)
           (evil-visual-restore)))

(global-leader-mode-specific-def
	:keymaps 'org-agenda-mode-map
	:states '(motion normal)

	"r" '(org-agenda-refile
		    :which-key "agenda refile")

	"f" '(org-agenda-goto-date
		    :which-key "agenda goto date")

	"t" '(org-agenda-todo
		    :which-key "agenda todo")

	"s" '(org-agenda-schedule
		    :which-key "agenda schedule")

  "e" '(org-agenda-set-effort
        :which-key "agenda set effort")

	(kbd "C-s") '((lambda () (interactive) (org-agenda-todo "TODAY"))
		            :which-key "agenda set TODAY")

	(kbd "C-S-s") '((lambda () (interactive) (org-agenda-todo "TODO"))
		              :which-key "agenda remove TODAY")

	"S" '((lambda () (interactive) (org-agenda-schedule nil "+1d"))
		    :which-key "agenda schedule to tomorrow")

	"d" '(org-agenda-deadline
		    :which-key "agenda deadline")

	"D" '((lambda () (interactive) (org-agenda-deadline nil "+1w"))
		    :which-key "agenda deadline to 1w")

	"p" '(org-agenda-priority
		    :which-key "agenda priority")

	"P" '(org-agenda-set-property
		    :which-key "agenda set property")

	"q" '(org-agenda-set-tags
		    :which-key "agenda set tags")

	"i" '(org-agenda-clock-in
		    :which-key "clock in")

  "xv" '(ivy-org-life-agenda-show-view
         :which-key "org-life show view")

  "xm" '(org-life-agenda-show-main
         :which-key "org-life main")

  "xl" '(org-life-agenda-show-task-list
         :which-key "org-life task list"))

(global-shortcut-def
  :keymaps 'org-mode-map
  :states '(motion normal)

  ;; narrow
  ",n" 'org-narrow-to-subtree
  ;; insert date
  "t" 'org-time-stamp
  ;; insert date and time
  ",t" (lambda () (interactive)
         (org-time-stamp '(4)))
  ;; insert inactive date
  "T" 'org-time-stamp-inactive
  ;; insert inactive date and time
  ",T" (lambda () (interactive)
         (org-time-stamp-inactive '(4)))
  ;; show latex fragments
  "l" (lambda () (interactive)
        (org-show-all-latex-fragments)
        (org-redisplay-inline-images))
  ;; toggle latex fragments
  ",l" 'org-toggle-latex-fragment
  ;; remove latex fragments
  "L" (lambda () (interactive)
        (org-remove-latex-fragment-image-overlays)
        (org-remove-inline-images)))

(general-define-key
 :keymaps 'org-agenda-mode-map
 :states '(motion normal)

 ;; C-c C-t: make into todo / cycle todo states
 ;; C-c C-s: add / change scheduled start
 ;; C-c C-d: add / change deadline
 ;; C-c ,: add / change priority
 ;;
 ;; .: go to today.
 ;;
 ;; TAB: goto entry.
 ;;
 ;; r: refresh
 ;; q: quit

 "Z" (lambda () (interactive)
       (when org-agenda-entry-text-mode
         (org-agenda-entry-text-mode)))
 "X" (lambda () (interactive)
       (when (not org-agenda-entry-text-mode)
         (org-agenda-entry-text-mode)))

 "_" 'org-agenda-do-date-earlier
 "+" 'org-agenda-do-date-later

 "C-h" 'org-agenda-earlier
 "C-l" 'org-agenda-later

 "r" 'org-agenda-redo
 "u" (lambda () (interactive)
       (message "Temporarily disabled undo.")) ; 'org-agenda-undo
 "U" 'org-agenda-redo
 "j" 'org-agenda-next-line
 "k" 'org-agenda-previous-line
 "C-j" 'org-agenda-next-date-line
 "C-k" 'org-agenda-previous-date-line
 "C-c v" 'org-agenda-view-mode-dispatch)

(global-leader-mode-specific-def ; sometimes doesn't work?
	:keymaps 'org-capture-mode-map
	:states '(motion normal)

	"r" '(org-capture-refile
		    :which-key "capture refile")

	"j" '(org-capture-kill
		    :which-key "capture discard")

	"k" '(org-capture-finalize
		    :which-key "capture save")

	"K" '((lambda () (interactive) (org-capture-finalize '(4)))
		    :which-key "capture save"))

;;; others

;; do not use org-super-agenda-header-map
(setq org-super-agenda-header-map (make-sparse-keymap))

;; start notification server
;; temporarily disabled
;; (org-notify-start 60)

(when org-directory
  ;; refresh all org files
  (update-all-org-directory-files)

  ;; org directory external config
  (let ((external-config-path
         (expand-file-name "org-config.el" org-directory)))
    (when (file-exists-p external-config-path)
      (load external-config-path)))
  ;; other agenda views

  ;; my org-life
  (add-to-list 'load-path
	             (expand-file-name "packages/org-life" (file-name-directory load-file-name)))
  (require 'org-life)
  (setq org-life-config-file-path
        (expand-file-name "org-life-config.org" org-directory))
  (push
   '("x" "org-life agenda"
     ((org-life-agenda ""
                       ())))
   org-agenda-custom-commands))
