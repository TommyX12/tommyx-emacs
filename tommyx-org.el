;; settings
(add-hook 'org-mode-hook (lambda () (interactive) (org-indent-mode))) ; use clean view
(setq org-agenda-window-setup 'current-window) ; use current window for agenda
(setq org-agenda-use-tag-inheritance t) ; tag inheritance in agenda (turn off if slow)
(setq org-agenda-sticky t) ; hide, not kill, agenda buffer when quiting

;; check if org-notes-dir exists.
;; (when (boundp 'org-agenda-dir)

;; configs
(setq org-todo-keywords '((sequence "TODO" "DONE")))

;; agenda files
(add-to-list 'org-agenda-files org-directory)

;; capture templates
(setq org-capture-templates '(
    ("i" "Inbox" entry
    (file+headline "GTD.org" "Inbox")
    "* %? %i")
))

;; checks if org-notes-dir exists.
;; )

;; key bindings
(evil-define-key 'motion 'global (kbd ", C-c") 'org-capture)
(evil-define-key 'normal 'global (kbd ", C-c") 'org-capture)
(evil-define-key 'visual 'global (kbd ", C-c") 'org-capture)
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

    (kbd "C--") 'org-shiftdown ; change date similar to speeddating
    (kbd "C-=") 'org-shiftup

    (kbd "C-S-h") 'org-shiftmetaleft ; promote/outdent
    (kbd "C-S-j") 'org-metadown ; move down
    (kbd "C-S-k") 'org-metaup ; move up
    (kbd "C-S-l") 'org-shiftmetaright ; demote/indent
    (kbd "M-h") help-map

    (kbd "C-h") (lambda () (interactive) (outline-hide-subtree))
    (kbd "C-j") 'org-next-visible-heading
    (kbd "C-k") 'org-previous-visible-heading
    (kbd "C-l") (lambda () (interactive) (outline-show-entry) (outline-show-children))

    "X" 'outline-show-all
    "Z" 'org-shifttab ; cycle global visibility

    (kbd "C-S-f") 'helm-org-in-buffer-headings
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

    (kbd "M-h") help-map
)
(evil-define-key 'visual org-mode-map
    (kbd "C-S-h") (lambda () (interactive) (org-metaleft) (evil-visual-restore)) ; promote/outdent
    (kbd "C-S-l") (lambda () (interactive) (org-metaright) (evil-visual-restore)) ; demote/indent
    (kbd "M-h") help-map

    (kbd "C-j") 'org-next-visible-heading
    (kbd "C-k") 'org-previous-visible-heading
)
(evil-define-key 'motion 'global ",a" 'org-agenda) ; open agenda
(evil-define-key 'motion org-agenda-mode-map
  ;; C-c C-t: make into todo / cycle todo states
  ;; C-c C-s: add / change scheduled start
  ;; C-c C-d: add / change deadline
  ;; C-c ,: add / change priority
  ;;
  ;; TAB: goto entry.
  ;;
  ;; r: refresh
  ;; q: quit

    (kbd "M-h") help-map

    "r" 'org-agenda-redo
    "u" 'org-agenda-undo
    "U" 'org-agenda-redo

    "j" 'org-agenda-next-line
    "j" 'org-agenda-next-line
    "k" 'org-agenda-previous-line
    (kbd "C-j") 'org-agenda-next-date-line
    (kbd "C-k") 'org-agenda-previous-date-line
)
