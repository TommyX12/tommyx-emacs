;; requires
(require 'evil)
(require 'org-super-agenda)

;; settings
(add-hook 'org-mode-hook (lambda () (interactive) (org-indent-mode))) ; use clean view
(setq org-agenda-window-setup 'current-window) ; use current window for agenda
(setq org-agenda-use-tag-inheritance t) ; tag inheritance in agenda (turn off if slow)
(setq org-agenda-sticky t) ; hide, not kill, agenda buffer when quiting

;; check if org-notes-dir exists.
;; (when (boundp 'org-agenda-dir)

;; todo
(setq org-todo-keywords '((sequence "TODO" "DONE")))

;; refiling
(setq org-refile-targets '((nil . (:level . 1)) (nil . (:level . 2))))

;; ivy integration
(setq counsel-org-headline-display-style 'path)
(setq counsel-org-headline-path-separator "/")

;; agenda files
(add-to-list 'org-agenda-files org-directory)

;; agenda configs
(setq org-agenda-span 'day)
(setq org-agenda-move-date-from-past-immediately-to-today t)
(org-super-agenda-mode)
(evil-set-initial-state 'org-agenda-mode 'motion)
(add-hook 'org-agenda-mode-hook (lambda () (hl-line-mode 1)))

;; entry text filter
; remove blank lines and state logs
(setq org-agenda-entry-text-exclude-regexps '("^- State.*\n" "^[ \t]*\n"))

;; logging
(setq org-log-done 'time)

;; misc
(setq org-M-RET-may-split-line nil)

;; capture templates
(setq org-capture-templates '(
    ("I" "Important" entry
    (file+headline "GTD.org" "Important")
    "* %? %i")
    ("i" "Not important" entry
    (file+headline "GTD.org" "Not important")
    "* %? %i")
))

;; checks if org-notes-dir exists.
;; )

;; key bindings
; global
(evil-define-key 'motion 'global (kbd ", C-c c") 'org-capture)
(evil-define-key 'motion 'global (kbd ", C-c a") 'org-agenda)
(evil-define-key 'motion 'global (kbd ", C-c C-f") 'counsel-org-goto-all)
; org mode
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
    (kbd "M-j") nil
    (kbd "M-k") nil

    (kbd "C-c C-.") 'org-time-stamp-inactive ; with C-u as previx also add time.

    (kbd "C--") 'org-shiftdown ; change date like speed-dating
    (kbd "C-=") 'org-shiftup

    (kbd "C-S-h") 'org-shiftmetaleft ; promote/outdent
    (kbd "C-S-j") 'org-metadown ; move down
    (kbd "C-S-k") 'org-metaup ; move up
    (kbd "C-S-l") 'org-shiftmetaright ; demote/indent
    (kbd "M-h") help-map

    (kbd "M-RET") 'org-insert-heading-respect-content
    (kbd "M-S-RET") 'org-insert-todo-heading-respect-content
    (kbd "C-RET") 'org-insert-subheading
    (kbd "C-S-RET") 'org-insert-todo-subheading
    (kbd "<M-return>") 'org-insert-heading-respect-content
    (kbd "<M-S-return>") 'org-insert-todo-heading-respect-content
    (kbd "<C-return>") 'org-insert-subheading
    (kbd "<C-S-return>") 'org-insert-todo-subheading

    (kbd "C-h") (lambda () (interactive) (outline-hide-subtree))
    (kbd "C-j") 'org-next-visible-heading
    (kbd "C-k") 'org-previous-visible-heading
    (kbd "C-l") (lambda () (interactive) (outline-show-entry) (outline-show-children))

    "X" 'outline-show-all
    "Z" 'org-shifttab ; cycle global visibility

    (kbd "C-S-f") 'counsel-org-goto
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

    (kbd "M-RET") 'org-insert-heading-respect-content
    (kbd "M-S-RET") 'org-insert-todo-heading-respect-content
    (kbd "C-RET") 'org-insert-subheading
    (kbd "C-S-RET") 'org-insert-todo-subheading
    (kbd "<M-return>") 'org-insert-heading-respect-content
    (kbd "<M-S-return>") 'org-insert-todo-heading-respect-content
    (kbd "<C-return>") 'org-insert-subheading
    (kbd "<C-S-return>") 'org-insert-todo-subheading

    (kbd "M-h") help-map
)
(evil-define-motion evil-org-next-visible-heading () :type exclusive
    (org-next-visible-heading 1))
(evil-define-motion evil-org-previous-visible-heading () :type exclusive
    (org-previous-visible-heading 1))
(evil-define-key 'visual org-mode-map
    (kbd "C-S-h") (lambda () (interactive) (org-metaleft) (evil-visual-restore)) ; promote/outdent
    (kbd "C-S-l") (lambda () (interactive) (org-metaright) (evil-visual-restore)) ; demote/indent
    (kbd "M-h") help-map

    (kbd "C-j") 'evil-org-next-visible-heading
    (kbd "C-k") 'evil-org-previous-visible-heading
)
(evil-define-key 'motion 'global ",,n" 'org-narrow-to-subtree)
; org agenda
(evil-define-key 'motion org-agenda-mode-map
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

    (kbd "M-h") help-map

    (kbd "C--") 'org-agenda-do-date-earlier
    (kbd "C-=") 'org-agenda-do-date-later

    (kbd "C-h") 'org-agenda-earlier
    (kbd "C-l") 'org-agenda-later

    "r" 'org-agenda-redo
    "u" (lambda () (interactive) (message "Temporarily disabled undo.")) ; 'org-agenda-undo
    "U" 'org-agenda-redo

    "j" 'org-agenda-next-line
    "j" 'org-agenda-next-line
    "k" 'org-agenda-previous-line
    (kbd "C-j") 'org-agenda-next-date-line
    (kbd "C-k") 'org-agenda-previous-date-line

    (kbd "C-c v") 'org-agenda-view-mode-dispatch
)
