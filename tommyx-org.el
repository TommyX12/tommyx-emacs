;; requires
(require 's)
(require 'evil)
(require 'alert)
(require 'org-super-agenda)
(require 'org-pomodoro)
(require 'org-bullets)
(require 'org-notify)
(require 'companion)

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

;; bullet
(setq org-bullets-bullet-list '("●"))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; habit
(add-to-list 'org-modules 'org-habit)

;; clocking
; ask for clock-out on leave
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

;; misc
(setq org-M-RET-may-split-line nil)
(setq org-fontify-done-headline t)
(setq org-fontify-whole-heading-line t)
(setq org-super-agenda-fontify-whole-header-line t) ; this doesn't work
(setq org-imenu-depth 4)

;; agenda configs
(setq org-agenda-dim-blocked-tasks t)
(setq org-agenda-window-setup 'only-window)
(setq org-agenda-start-with-clockreport-mode t)
(setq org-clock-report-include-clocking-task t)
(setq org-agenda-start-with-log-mode t)
(setq org-agenda-log-mode-items '(closed clock state))
(setq org-agenda-restore-windows-after-quit t)
(setq org-agenda-use-tag-inheritance t) ; tag inheritance in agenda (turn off if slow)
(setq org-agenda-sticky nil) ; make sure to refresh agenda when re-launch
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

;; scanning all org files
(defvar all-org-directory-files nil 
  "List of all org files in org-directory.
Update with 'update-all-org-directory-files'.")

(defun find-org-files
    (&optional recurse dirs file-excludes dir-excludes)
  "Fill the variable `all-org-directory-files'.
Optional parameters:
  recurse        If `t', scan the directory recusively.
  dirs           A list of directories to scan for *.org files.
  file-excludes  Regular expression. If a filename matches this regular
expression,
                 do not add it to `all-org-directory-files'.
  dir-excludes   Regular expression. If a directory name matches this
regular expression,
                 do not add it to `all-org-directory-files'."
  (let ((targets (or dirs (list org-directory)))
        (fex (or file-excludes  "^[#\\.].*$"))
        (dex (or dir-excludes  "^[#\\.].*$"))
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

;; key bindings
; global leader
(general-define-key
	:keymaps 'override
	:states '(motion normal visual)
	:prefix "SPC"
	:non-normal-prefix "M-SPC"

)
(general-define-key
	:keymaps 'override
	:states '(motion normal)
	:prefix "SPC"
	:non-normal-prefix "M-SPC"

	"of" '(counsel-org-goto-all ; go to heading of opened org files
		:which-key "org goto all")

	"or" '((lambda () (interactive) (org-refile '(4)))
		:which-key "org goto refile targets")

	"oc" '(org-capture
		:which-key "org capture")

	"oa" '(org-agenda
		:which-key "org agenda")

	"oi" '((lambda () (interactive) (org-clock-in '(4))) ; (lambda () (interactive) (org-pomodoro '(4)))
		:which-key "org clock in recent")

	"oI" '(org-clock-goto
		:which-key "org goto clock")

	"oo" '(org-clock-out
		:which-key "org clock out")

	"oO" '(org-clock-cancel
		:which-key "org clock cancel")
)
; org mode leader
(general-define-key
	:keymaps 'org-mode-map
	:states '(motion normal visual)
	:prefix "SPC"
	:non-normal-prefix "M-SPC"

	"jh" '(org-toggle-heading
		:which-key "org toggle heading/text")

	"j-" '(org-toggle-item
		:which-key "org toggle item type")

	"j[" '(org-toggle-checkbox
		:which-key "org toggle checkbox")

	"j]" '((lambda () (interactive) (org-toggle-checkbox '(4)))
		:which-key "org toggle checkbox presence")

	"jf" '(counsel-org-goto
		:which-key "org goto")
)
(general-define-key
	:keymaps 'org-mode-map
	:states '(visual)
	:prefix "SPC"
	:non-normal-prefix "M-SPC"

	"js" '(org-sort
		:which-key "org sort")
)
(general-define-key
	:keymaps 'org-mode-map
	:states '(motion normal)
	:prefix "SPC"
	:non-normal-prefix "M-SPC"

	"jr" '(org-refile
		:which-key "org refile")

	"jc" '(org-copy
		:which-key "org copy")

	"jt" '(org-todo
		:which-key "org todo")

	"jT" '(org-shiftleft
		:which-key "org todo back")

	"jx" '(org-ctrl-c-ctrl-c
		:which-key "org update")

	"js" '(org-schedule
		:which-key "org schedule")

	(kbd "j C-s") '((lambda () (interactive) (org-todo "TODAY"))
		:which-key "org set TODAY")

	(kbd "j C-S-s") '((lambda () (interactive) (org-todo "TODO"))
		:which-key "org remove TODAY")

	"jS" '((lambda () (interactive) (org-schedule nil "+1d"))
		:which-key "org schedule to tomorrow")

	"jd" '(org-deadline
		:which-key "org deadline")

	"jD" '((lambda () (interactive) (org-deadline nil "+1w"))
		:which-key "org deadline to 1w")

	"jp" '(org-priority
		:which-key "org priority")

	"jP" '(org-set-property
		:which-key "org set property")

	"jq" '(org-set-tags-command
		:which-key "org set tags")

	"ji" '(org-clock-in ; org-pomodoro
		:which-key "org clock in")
)

; org mode
(evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)
(evil-define-key 'normal org-mode-map (kbd "<tab>") 'org-cycle)
(evil-define-key 'insert org-mode-map (kbd "TAB") 'org-cycle)
(evil-define-key 'insert org-mode-map (kbd "<tab>") 'org-cycle)
(evil-define-key 'normal org-mode-map
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
	(kbd "M-h") nil
	(kbd "M-j") nil
	(kbd "M-k") nil
	(kbd "M-l") nil

	(kbd "C-c C-.") 'org-time-stamp-inactive ; with C-u as previx also add time.

	"_" 'org-shiftdown ; change date like speed-dating
	"+" 'org-shiftup

	(kbd "C-_") 'org-shiftleft
	(kbd "C-+")'org-shiftright

	(kbd "C-S-h") 'org-shiftmetaleft ; promote/outdent
	(kbd "C-S-j") 'org-metadown ; move down
	(kbd "C-S-k") 'org-metaup ; move up
	(kbd "C-S-l") 'org-shiftmetaright ; demote/indent

	(kbd "C-RET") 'org-insert-heading-respect-content
	(kbd "C-S-RET") 'org-insert-todo-heading-respect-content
	(kbd "M-RET") 'org-insert-subheading
	(kbd "M-S-RET") 'org-insert-todo-subheading
	(kbd "<C-return>") 'org-insert-heading-respect-content
	(kbd "<C-S-return>") 'org-insert-todo-heading-respect-content
	(kbd "<M-return>") 'org-insert-subheading
	(kbd "<M-S-return>") 'org-insert-todo-subheading

	(kbd "C-h") (lambda () (interactive) (outline-hide-subtree))
	(kbd "C-j") 'org-next-visible-heading
	(kbd "C-k") 'org-previous-visible-heading
	;; (kbd "C-l") (lambda () (interactive) (outline-show-entry) (outline-show-children))
	(kbd "C-l") 'org-cycle

	"X" 'outline-show-all
	"Z" 'org-shifttab ; cycle global visibility
)
(evil-define-key 'insert org-mode-map
  ;; using evil-collection with org mode:
  ;;
  ;; M-RET: create heading at same level
  ;; M-S-RET: create TODO heading at same level
  ;; C-RET: create heading at same level below current one (most useful)
  ;; C-S-RET: create TODO heading at same level below current one
  ;;
  ;; TAB and S-TAB: go through table fields
  ;; RET: table next row

	(kbd "C-c C-.") 'org-time-stamp-inactive ; with C-u as previx also add time.

	(kbd "C-RET") 'org-meta-return
	(kbd "C-S-RET") 'org-insert-todo-heading
	(kbd "M-RET") 'org-insert-subheading
	(kbd "M-S-RET") 'org-insert-todo-subheading
	(kbd "<C-return>") 'org-meta-return
	(kbd "<C-S-return>") 'org-insert-todo-heading
	(kbd "<M-return>") 'org-insert-subheading
	(kbd "<M-S-return>") 'org-insert-todo-subheading
)
(evil-define-motion evil-org-next-visible-heading () :type exclusive
	(org-next-visible-heading 1))
(evil-define-motion evil-org-previous-visible-heading () :type exclusive
	(org-previous-visible-heading 1))
(evil-define-key 'visual org-mode-map
	(kbd "C-S-h") (lambda () (interactive) (org-metaleft) (evil-visual-restore)) ; promote/outdent
	(kbd "C-S-l") (lambda () (interactive) (org-metaright) (evil-visual-restore)) ; demote/indent

	(kbd "C-j") 'evil-org-next-visible-heading
	(kbd "C-k") 'evil-org-previous-visible-heading
)
(evil-define-key 'motion 'org-mode-map ",,n" 'org-narrow-to-subtree)

; org agenda
(general-define-key
	:keymaps 'org-agenda-mode-map
	:states '(motion normal visual)
	:prefix "SPC"
	:non-normal-prefix "M-SPC"

)
(general-define-key
	:keymaps 'org-agenda-mode-map
	:states '(motion normal)
	:prefix "SPC"
	:non-normal-prefix "M-SPC"

	"jr" '(org-agenda-refile
		:which-key "agenda refile")

	"jf" '(org-agenda-goto-date
		:which-key "agenda goto date")

	"jt" '(org-agenda-todo
		:which-key "agenda todo")

	"js" '(org-agenda-schedule
		:which-key "agenda schedule")

	(kbd "j C-s") '((lambda () (interactive) (org-agenda-todo "TODAY"))
		:which-key "agenda set TODAY")

	(kbd "j C-S-s") '((lambda () (interactive) (org-agenda-todo "TODO"))
		:which-key "agenda remove TODAY")

	"jS" '((lambda () (interactive) (org-agenda-schedule nil "+1d"))
		:which-key "agenda schedule to tomorrow")

	"jd" '(org-agenda-deadline
		:which-key "agenda deadline")

	"jD" '((lambda () (interactive) (org-agenda-deadline nil "+1w"))
		:which-key "agenda deadline to 1w")

	"jp" '(org-agenda-priority
		:which-key "agenda priority")

	"jP" '(org-agenda-set-property
		:which-key "agenda set property")

	"jq" '(org-agenda-set-tags
		:which-key "agenda set tags")

	"ji" '(org-agenda-clock-in
		:which-key "clock in")
)
(evil-define-key 'normal org-agenda-mode-map
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

	"Z" (lambda () (interactive) (when org-agenda-entry-text-mode (org-agenda-entry-text-mode)))
	"X" (lambda () (interactive) (when (not org-agenda-entry-text-mode) (org-agenda-entry-text-mode)))

	"_" 'org-agenda-do-date-earlier
	"+" 'org-agenda-do-date-later

	(kbd "C-h") 'org-agenda-earlier
	(kbd "C-l") 'org-agenda-later

	"r" 'org-agenda-redo
	"u" (lambda () (interactive) (message "Temporarily disabled undo.")) ; 'org-agenda-undo
	"U" 'org-agenda-redo

	"j" 'org-agenda-next-line
	"k" 'org-agenda-previous-line
	(kbd "C-j") 'org-agenda-next-date-line
	(kbd "C-k") 'org-agenda-previous-date-line

	(kbd "C-c v") 'org-agenda-view-mode-dispatch
)

; org capture
(general-define-key ; sometimes doesn't work?
	:keymaps 'org-capture-mode-map
	:states '(motion normal)
	:prefix "SPC"
	:non-normal-prefix "M-SPC"

	"jr" '(org-capture-refile
		:which-key "capture refile")

	"jk" '(org-capture-kill
		:which-key "capture discard")

	"jj" '(org-capture-finalize
		:which-key "capture save")
)

;; org directory external config
(load (expand-file-name "org-config.el" org-directory))

;; do not use org-super-agenda-header-map
(setq org-super-agenda-header-map (make-sparse-keymap))

;; start notification server
(org-notify-start 60)
