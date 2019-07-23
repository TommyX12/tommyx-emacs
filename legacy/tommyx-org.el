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
(enable-auto-compilation 'tommyx-org-def)
(require 'tommyx-org-def)
(require 'tommyx-bind-def)

;; face def

(defface org-priority-1
  '((t :inherit error))
  "")
(defface org-priority-2
  '((t :inherit warning))
  "")
(defface org-priority-3
  '((t :inherit success))
  "")

;; startup settings
(add-hook 'org-mode-hook (lambda () (interactive)
                           (setq-local indent-tabs-mode nil) ; use space instead of tabs
                           (setq-local tab-width 2)
                           (setq-local evil-shift-width tab-width)
                           (setq-local show-trailing-whitespace t)))

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
                  1.5)
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
(add-hook
 'org-agenda-mode-hook
 (lambda ()
   (hl-line-mode 1)
   (setq-local use-line-nav t)))
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


;;; key bindings

;; helpers

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

(defhydra hydra-org-nav ()
  "org heading navigation"
  ("h" evil-org-up-heading "parent heading")
  ("l" evil-outline-next-heading "next heading")
  ("k" evil-org-backward-heading-same-level "previous heading same level")
  ("j" evil-org-forward-heading-same-level "next heading same level"))

(tommyx-bind-keys
 `(:case
   (:bindings

    org-prefix
    (:bindings

     "h" (:def
          helm-org-rifle-org-directory
          :which-key "Helm Org Rifle")
     "f" (:def
          counsel-org-goto-all ; go to heading of opened org files
          :which-key "Org Goto All")
     "r" (:def
          ,(lambda () (interactive) (org-refile '(4)))
          :which-key "Org Goto Refile Targets")
     "c" (:def
          counsel-org-capture
          :which-key "Org Capture")
     "C" (:def
          org-capture
          :which-key "Org Capture")
     "a" (:def
          org-agenda
          :which-key "Org Agenda")
     "i" (:def
          ,(lambda () (interactive) (org-clock-in '(4))) ; (lambda () (interactive) (org-pomodoro '(4)))
          :which-key "Org Clock In Recent")
     "I" (:def
          org-clock-goto
          :which-key "Org Goto Clock")
     "o" (:def
          org-clock-out
          :which-key "Org Clock Out")
     "O" (:def
          org-clock-cancel
          :which-key "Org Clock Cancel")
     "l" (:def
          org-insert-link-global
          :which-key "Insert Org Link")
     "L" (:def
          org-store-link
          :which-key "Store Org Link")
     "A"
     (:bindings
      :which-key "Auto Capture"

      "p" (:def
           org-auto-capture-process-subtree
           :which-key "Process Subtree")
      "d" (:def
           org-auto-capture-delete-marked-subtree
           :which-key "Delete Marked In Subtree"))))

   :keymaps org-mode-map
   :states (motion normal visual)
   (:bindings

    "<tab>" (:case
             :states (motion normal visual insert)
             org-cycle)

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

    "C-c C-." org-time-stamp-inactive ; with C-u as previx also add time.

    "_" org-shiftdown ; change date like speed-dating
    "+" org-shiftup

    "C-_" org-shiftleft
    "C-+" org-shiftright

    "C-S-h" org-shiftmetaleft ; promote/outdent
    "C-S-j" org-metadown ; move down
    "C-S-k" org-metaup ; move up
    "C-S-l" org-shiftmetaright ; demote/indent

    "<M-return>" org-insert-heading-respect-content
    "<M-S-return>" org-insert-todo-heading-respect-content
    "<C-return>" org-insert-subheading
    "<C-S-return>" org-insert-todo-subheading

    "C-g" ,(lambda () (interactive) (outline-hide-subtree))
    ;; (kbd "C-j") org-next-visible-heading
    ;; (kbd "C-k") org-previous-visible-heading
    ;; ;; (kbd "C-l") ,(lambda () (interactive) (outline-show-entry) (outline-show-children))
    "C-;" org-cycle

    fold-expand-all org-show-all
    fold-focus org-focus
    ;; Just use shift-tab itself, or C-u tab
    ;; "Z" org-shifttab ; cycle global visibility

    "t" hydra-org-nav/body

    goto-parent-semantic-element evil-org-up-heading
    goto-child-semantic-element evil-outline-next-heading
    goto-previous-semantic-element evil-org-backward-heading-same-level
    goto-next-semantic-element evil-org-forward-heading-same-level

    find-semantic-item
    (:def
     counsel-org-goto
     :which-key "Org Goto Headings")

    mode-specific-prefix
    (:case
     (:bindings

      "h" (:def
           org-toggle-heading
           :which-key "Org Toggle Heading/Text")
      "-" (:def
           org-toggle-item
           :which-key "Org Toggle Item Type")
      "[" (:def
           org-toggle-checkbox
           :which-key "Org Toggle Checkbox")
      "]" (:def
           ,(lambda () (interactive) (org-toggle-checkbox '(4)))
           :which-key "Org Toggle Checkbox Presence")
      "f" (:def
           counsel-org-goto
           :which-key "Org Goto")
      ";" (:def
           org-sort
           :which-key "Org Sort")
      ":" (:def
           ,(lambda () (interactive)
              (org-sort-list nil
                             ?f
                             #'org-sort-checklist-getkey-func
                             #'org-sort-checklist-compare-func))
           :which-key "Org Sort Checklist"))

     :states (motion normal)
     (:bindings

      "r" (:def
           org-refile
           :which-key "Org Refile")
      "c" (:def
           org-copy
           :which-key "Org Copy")
      "t" (:def
           org-todo
           :which-key "Org Todo")
      "T" (:def
           org-shiftleft
           :which-key "Org Todo Back")
      "x" (:def
           org-ctrl-c-ctrl-c
           :which-key "Org Update")
      "s" (:def
           org-schedule
           :which-key "Org Schedule")
      "e" (:def
           org-set-effort
           :which-key "Org Set Effort")
      "e" (:def
           org-set-effort
           :which-key "Org Set Effort")
      "E" (:def
           org-inc-effort
           :which-key "Org Increase Effort")
      "C-s" (:def
             ,(lambda () (interactive) (org-todo "TODAY"))
             :which-key "Org Set Today")
      "C-S-s" (:def
               ,(lambda () (interactive) (org-todo "TODO"))
               :which-key "Org Remove Today")
      "S" (:def
           ,(lambda () (interactive) (org-schedule nil "+1d"))
           :which-key "Org Schedule To Tomorrow")
      "d" (:def
           org-deadline
           :which-key "Org Deadline")
      "D" (:def
           ,(lambda () (interactive) (org-deadline nil "+1w"))
           :which-key "Org Deadline To 1w")
      "p" (:def
           org-priority
           :which-key "Org Priority")
      "P" (:def
           org-set-property
           :which-key "Org Set Property")
      "q" (:def
           org-set-tags-command
           :which-key "Org Set Tags")
      "i" (:def
           org-clock-in ; org-pomodoro
           :which-key "Org Clock In")
      "%" (:def
           ,(lambda () (interactive)
              (insert "[%]")
              (org-ctrl-c-ctrl-c)
              (insert " "))
           :which-key "Org Insert % Cookie")
      "/" (:def
           ,(lambda () (interactive)
              (insert "[/]")
              (org-ctrl-c-ctrl-c)
              (insert " "))
           :which-key "Org Insert / Cookie")
      "l" (:def
           org-insert-link ; also allow editing link
           :which-key "Org Insert Link")
      "L" (:def
           org-store-link
           :which-key "Org Store Link")
      "o" (:def
           org-open-at-point
           :which-key "Org Open Link")
      "C-l" (:def
             org-toggle-link-display
             :which-key "Org Toggle Link Display")

      "v"
      (:bindings
       :which-key "Org View And Export"

       "l" (:def
            org-show-all-latex-fragments
            :which-key "Preview Latex Fragment")
       "i" (:def
            org-redisplay-inline-images
            :which-key "Display Inline Images")
       "I" (:def
            org-remove-inline-images
            :which-key "Remove Inline Images")
       "L" (:def
            ,(lambda () (interactive)
               (org-remove-latex-fragment-image-overlays))
            :which-key "Remove Latex Fragment")
       "c" (:def
            org-clock-display
            :which-key "Clock Display")
       "C" (:def
            org-clock-remove-overlays
            :which-key "Remove Clock Display")
       "d" (:def
            org-update-all-dblocks
            :which-key "Update Dynamic Blocks"))))

    shortcuts-prefix
    (:bindings

     ;; insert date
     "t" org-time-stamp
     ;; insert inactive date
     "T" org-time-stamp-inactive
     ;; show latex fragments
     "l" ,(lambda () (interactive)
            (org-show-all-latex-fragments)
            (org-redisplay-inline-images))
     ;; remove latex fragments
     "L" ,(lambda () (interactive)
            (org-remove-latex-fragment-image-overlays)
            (org-remove-inline-images)))

    extended-shortcuts-prefix
    (:bindings

     ;; narrow
     "n" org-narrow-to-subtree
     ;; insert date and time
     "t" ,(lambda () (interactive)
            (org-time-stamp '(4)))
     ;; insert inactive date and time
     "T" ,(lambda () (interactive)
            (org-time-stamp-inactive '(4)))
     ;; toggle latex fragments
     "l" org-toggle-latex-fragment))

   :keymaps org-mode-map
   :states (insert)
   (:bindings

    ;; using evil-collection with org mode:
    ;;
    ;; M-RET: create heading at same level
    ;; M-S-RET: create TODO heading at same level
    ;; C-RET: create heading at same level below current one (most useful)
    ;; C-S-RET: create TODO heading at same level below current one
    ;;
    ;; TAB and S-TAB: go through table fields
    ;; RET: table next row

    "C-c C-." org-time-stamp-inactive ; with C-u as previx also add time.
    "M-RET" org-meta-return
    "M-S-RET" org-insert-todo-heading
    "C-RET" org-insert-subheading
    "C-S-RET" org-insert-todo-subheading
    "<M-return>" org-meta-return
    "<M-S-return>" org-insert-todo-heading
    "<C-return>" org-insert-subheading
    "<C-S-return>" org-insert-todo-subheading)

   :keymaps org-mode-map
   :states (visual)
   (:bindings

    ;; promote/outdent
    "C-S-h" ,(lambda () (interactive)
               (org-metaleft)
               (evil-visual-restore))
    ;; demote/indent
    "C-S-l" ,(lambda () (interactive)
               (org-metaright)
               (evil-visual-restore)))

   :keymaps org-agenda-mode-map
   :states (motion normal visual)
   (:bindings

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

    fold-focus ,(lambda () (interactive)
                  (when org-agenda-entry-text-mode
                    (org-agenda-entry-text-mode)))
    fold-expand-all ,(lambda () (interactive)
                       (when (not org-agenda-entry-text-mode)
                         (org-agenda-entry-text-mode)))

    "_" org-agenda-do-date-earlier
    "+" org-agenda-do-date-later

    "C-h" org-agenda-earlier
    "C-l" org-agenda-later

    "r" org-agenda-redo
    "u" ,(lambda () (interactive)
           (message "Temporarily disabled undo.")) ; org-agenda-undo
    "U" org-agenda-redo
    "q" org-agenda-quit
    "j" org-agenda-next-line
    "k" org-agenda-previous-line
    goto-next-semantic-element org-agenda-next-date-line
    goto-previous-semantic-element org-agenda-previous-date-line
    "C-c v" org-agenda-view-mode-dispatch

    mode-specific-prefix
    (:bindings

     "r" (:def
          org-agenda-refile
          :which-key "Agenda Refile")
     "f" (:def
          org-agenda-goto-date
          :which-key "Agenda Goto Date")
     "t" (:def
          org-agenda-todo
          :which-key "Agenda Todo")
     "s" (:def
          org-agenda-schedule
          :which-key "Agenda Schedule")
     "e" (:def
          org-agenda-set-effort
          :which-key "Agenda Set Effort")
     "C-s" (:def
            ,(lambda () (interactive)
               (org-agenda-todo "TODAY"))
            :which-key "Agenda Set Today")
     "C-S-s" (:def
              ,(lambda () (interactive)
                 (org-agenda-todo "TODO"))
              :which-key "Agenda Remove Today")
     "S" (:def
          ,(lambda () (interactive) (org-agenda-schedule nil "+1d"))
          :which-key "Agenda Schedule To Tomorrow")
     "d" (:def
          org-agenda-deadline
          :which-key "Agenda Deadline")
     "D" (:def
          ,(lambda () (interactive) (org-agenda-deadline nil "+1w"))
          :which-key "Agenda Deadline To 1w")
     "p" (:def
          org-agenda-priority
          :which-key "Agenda Priority")
     "P" (:def
          org-agenda-set-property
          :which-key "Agenda Set Property")
     "q" (:def
          org-agenda-set-tags
          :which-key "Agenda Set Tags")
     "i" (:def
          org-agenda-clock-in
          :which-key "Clock In")
     "x"
     (:bindings
      :which-key "Org-life"

      "v" (:def
           ivy-org-life-agenda-show-view
           :which-key "Org-life Show View")
      "m" (:def
           org-life-agenda-show-main
           :which-key "Org-life Main")
      "l" (:def
           org-life-agenda-show-task-list
           :which-key "Org-life Task List"))))

   :keymaps org-capture-mode-map
   :states (motion normal visual)
   (:bindings

    mode-specific-prefix
    (:bindings

     "r" (:def
          org-capture-refile
          :which-key "Capture Refile")
     "j" (:def
          org-capture-kill
          :which-key "Capture Discard")
     "k" (:def
          org-capture-finalize
          :which-key "Capture Save")
     "K" (:def
          ,(lambda () (interactive) (org-capture-finalize '(4)))
          :which-key "Capture Save")))))

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

  ;;; my org-life

  (enable-auto-compilation 'org-life)
  (require 'org-life)
  (setq org-life-config-file-path
        (expand-file-name "org-life-config.org" org-directory))
  (push
   '("x" "org-life agenda"
     ((org-life-agenda ""
                       ())))
   org-agenda-custom-commands)

  ;;; my org-catalyst

  (enable-auto-compilation 'org-catalyst)
  (require 'org-catalyst)
  (add-hook 'org-catalyst-mode-hook
            (lambda ()
              ;; (hl-line-mode 1)
              (setq-local use-line-nav t)))
  (setq org-catalyst-save-path (f-join org-directory "org-catalyst"))
  (org-catalyst-setup-evil-status-bindings)
  (org-catalyst-auto-save-mode 1)
  (tommyx-bind-keys
   `(:case
     (:bindings

      global-leader
      (:bindings

       "l"
       (:bindings
        :which-key "Catalyst"

        "s" org-catalyst-status
        "r" org-catalyst-recompute-history))))))


(provide 'tommyx-org)

;;; tommyx-org.el ends here
