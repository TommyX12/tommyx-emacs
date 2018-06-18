
;;; initialize packages
(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)


;;; package to automatically track and install packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))


;;; packages settings before loading

;; evil
(setq evil-want-Y-yank-to-eol t)
(setq evil-want-integration nil)
(setq evil-search-module 'evil-search)

;; evil-collection
(setq evil-collection-company-use-tng nil)
(setq evil-collection-setup-minibuffer nil)


;;; install packages
(use-package evil :ensure t)
(use-package evil-collection :ensure t :after evil)
(use-package evil-visualstar :ensure t)
(use-package evil-surround :ensure t)
(use-package evil-args :ensure t)
(use-package evil-matchit :ensure t :defer t)
(use-package evil-numbers :ensure t)
(use-package evil-search-highlight-persist :ensure t)
(use-package evil-nerd-commenter :ensure t)
(use-package projectile :ensure t)
(use-package helm :ensure t)
(use-package helm-flx :ensure t)
(use-package helm-descbinds :ensure t)
(use-package helm-projectile :ensure t :after projectile)
(use-package which-key :ensure t)
(use-package spacemacs-theme :ensure t :defer t)
(use-package doom-themes :ensure t :defer t)
(use-package ace-window :ensure t)
(use-package general :ensure t)
(use-package highlight-indent-guides :ensure t)
(use-package origami :ensure t)
(use-package volatile-highlights :ensure t)
(use-package flycheck :ensure t)
(use-package flyspell-lazy :ensure t)
(use-package rainbow-delimiters :ensure t)
(use-package avy :ensure t)
(use-package smartparens :ensure t
	     ; don't show in mode display
	     :diminish smartparens-mode)
(use-package company :ensure t)
(use-package company-quickhelp :ensure t)
(use-package company-flx :ensure t)
(use-package yasnippet :ensure t
  :bind (:map yas-minor-mode-map
	("TAB" . nil)
	("<tab>" . nil)
	("S-TAB" . nil)
	("<S-tab>" . nil))
	(:map yas-keymap
	("TAB" . nil)
	("<tab>" . nil)
	("S-TAB" . nil)
	("<S-tab>" . nil)))
(use-package yasnippet-snippets :ensure t)
(use-package powerline :ensure t)
(use-package powerline-evil :ensure t)
(use-package spaceline :ensure t)
(use-package window-numbering :ensure t)
(use-package which-func :ensure t)
(use-package git-gutter :ensure t)
(use-package yascroll :ensure t)
(use-package color-identifiers-mode :ensure t)
(use-package neotree :ensure t)
(use-package magit :ensure t)
(use-package page-break-lines :ensure t)
(use-package dashboard :ensure t :after page-break-lines)
(use-package org :ensure t)
(use-package org-super-agenda :ensure t)
; language specific
(use-package csv-mode :ensure t)
(use-package json-mode :ensure t)


;;; package settings

;; org
(add-hook 'org-mode-hook (lambda () (interactive) (org-indent-mode)))

;; color-identifiers-mode
(setq color-identifiers-coloring-method 'sequential)
(setq color-identifiers:max-color-saturation 0.3)
(setq color-identifiers:min-color-saturation 0.25)
(setq color-identifiers:timer (run-with-idle-timer 2 t 'color-identifiers:refresh))
(global-color-identifiers-mode)

;; dashboard
(dashboard-setup-startup-hook)

;; window-numbering
(window-numbering-mode)

;; org
(org-super-agenda-mode)
(setq org-M-RET-may-split-line nil)
(setq org-log-done 'time)

;; yascroll
(global-yascroll-bar-mode 1)
(setq yascroll:delay-to-hide nil)
(set-face-foreground 'yascroll:thumb-fringe "#555555")
(set-face-background 'yascroll:thumb-fringe "#555555")

;; git gutter
(setq
    git-gutter:window-width 1
    git-gutter:update-interval 1.5
    git-gutter:modified-sign "|"
    git-gutter:added-sign "|"
    git-gutter:deleted-sign "-")
(global-git-gutter-mode +1)

;; which-function
(add-hook 'prog-mode-hook #'which-function-mode)
(add-hook 'prog-mode-hook (lambda () (interactive)
    (setq header-line-format '(" - " which-func-format))))

;; powerline
(setq powerline-default-separator nil)
(require 'spaceline-config)
(spaceline-toggle-hud-off)
(spaceline-toggle-which-function-off)
(spaceline-spacemacs-theme)
(spaceline-helm-mode)

;; dashboard
(setq dashboard-items '((agenda . 5)
			(recents  . 5)
			(projects . 5)
                        (bookmarks . 5)))
; custom logo and message
(setq dashboard-banner-length 250)
(setq dashboard-banner-logo-title
    (concat "GNU Emacs " emacs-version " (" system-configuration ")"))
(setq dashboard-startup-banner (expand-file-name "logo.png"
    (file-name-directory load-file-name)))


;; yasnippet
(add-hook 'after-init-hook 'yas-global-mode)
(add-to-list 'yas-snippet-dirs (expand-file-name "snippets"
    (file-name-directory load-file-name)))

;; company
(add-hook 'after-init-hook 'global-company-mode)
(company-tng-configure-default)
(company-quickhelp-mode)
(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
     (define-key company-active-map (kbd "<S-tab>") 'company-select-previous)))
(setq company-frontends
      '(company-pseudo-tooltip-unless-just-one-frontend
        company-preview-frontend
        company-tng-frontend
        company-echo-metadata-frontend))
(setq company-idle-delay 0)
(setq company-quickhelp-delay 0.3)
(setq company-require-match 'never)
(with-eval-after-load 'company
  (company-flx-mode +1))

;; smartparens
(require 'smartparens-config)
(smartparens-global-mode 1)
(show-smartparens-global-mode 1)
(setq smartparens-strict-mode nil)
; auto expanison of brackets
(sp-local-pair 'prog-mode "{" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
(sp-local-pair 'text-mode "{" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
(sp-local-pair 'prog-mode "[" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
(sp-local-pair 'text-mode "[" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
(sp-local-pair 'prog-mode "(" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
(sp-local-pair 'text-mode "(" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))

;; themes
; (load-theme 'spacemacs-dark t)
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)
(setq dark-theme 'doom-one)
(setq light-theme 'doom-one-light)
(load-theme dark-theme t)

;; evil
(evil-mode 1) ; use evil-mode at startup
; treat underscore as word
(modify-syntax-entry ?_ "w")
; split to the right and below
(setq evil-split-window-below t)
(setq evil-vsplit-window-right t)
(setq evil-ex-substitute-global t)
; auto center after search
(defun my-center-line (&rest _) (evil-scroll-line-to-center nil))
(advice-add 'evil-ex-search-next :after #'my-center-line)
(advice-add 'evil-ex-search-previous :after #'my-center-line)
(advice-add 'evil-ex-search-word-forward :after #'evil-ex-search-previous)
(advice-add 'evil-ex-search-word-backward :after #'evil-ex-search-next)
(defun my-search-previous (&rest _) (evil-ex-search-previous))
(defun my-search-next (&rest _) (evil-ex-search-next))
(advice-add 'evil-visualstar/begin-search-forward :after #'my-search-previous)
(advice-add 'evil-visualstar/begin-search-backward :after #'my-search-next)
; no magic for search
(setq evil-magic nil) ; doesn't work
; search highlight persist
(global-evil-search-highlight-persist) ; doesn't work

;; rainbow-delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'text-mode-hook #'rainbow-delimiters-mode)

;; evil-surround
(global-evil-surround-mode 1)

;; avy
(setq avy-keys '(?w ?e ?r ?u ?i ?o ?p ?a ?s ?d ?g ?h ?j ?k ?l ?v ?n))
(setq avy-all-windows nil)

;; undo-tree
; persistent undo
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/history")))

;; volatile-highlight
(vhl/define-extension 'evil
                      'evil-move
                      'evil-paste-after
                      'evil-paste-before
                      'evil-paste-pop)
(with-eval-after-load 'evil
    (vhl/install-extension 'evil)
    (vhl/load-extension 'evil))
(vhl/define-extension 'undo-tree
                      'undo-tree-move
                      'undo-tree-yank)
(with-eval-after-load 'undo-tree
    (vhl/install-extension 'undo-tree)
    (vhl/load-extension 'undo-tree))
(volatile-highlights-mode)

;; neotree
; (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq neo-theme 'arrow)
(setq neo-show-hidden-files t)
(add-hook 'neotree-mode-hook (lambda () (interactive)
    (hl-line-mode 1)))
(setq neo-confirm-change-root 'off-p)

;; evil-collection
(delete 'neotree evil-collection-mode-list)
(delete 'company evil-collection-mode-list)
(evil-collection-init)

;; flyspell lazy
(flyspell-lazy-mode 1)
(setq flyspell-lazy-idle-seconds 1)
(setq flyspell-lazy-window-idle-seconds 2.5)

;; highlight indent guides
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(add-hook 'text-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character) ; TODO use the character method. right now the font doesn't work
(setq highlight-indent-guides-character ?\|)
(setq highlight-indent-guides-responsive nil)

;; evil-visualstar
(global-evil-visualstar-mode)

;; general
(general-evil-setup)

;; flycheck
(global-flycheck-mode)

;; which key
(which-key-mode 1)
(setq which-key-idle-delay 0.5)

;; projectile
(projectile-mode)

;; helm
(require 'helm-config)
(helm-mode 1)
(helm-autoresize-mode 1) ; always auto resize window
(setq helm-autoresize-max-height 30)
(setq helm-autoresize-min-height 20)
(setq helm-split-window-inside-p t)
(setq helm-full-frame nil)
(setq helm-recentf-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t)
(setq helm-recentf-fuzzy-match t)
(setq helm-locate-fuzzy-match t)
(setq helm-M-x-fuzzy-match t)
(setq helm-semantic-fuzzy-match t)
(setq helm-imenu-fuzzy-match t)
(setq helm-apropos-fuzzy-match t)
(setq helm-lisp-fuzzy-completion t)
(setq helm-session-fuzzy-match t)
(setq helm-etags-fuzzy-match t)
(setq helm-mode-fuzzy-match t)
(setq helm-completion-in-region-fuzzy-match t)
(setq recentf-max-menu-items 20)
(setq recentf-max-saved-items 50)

;; helm-flx
(helm-flx-mode +1)
(setq helm-flx-for-helm-find-files t
      helm-flx-for-helm-locate t)

;; helm-descbinds
(helm-descbinds-mode)

;; helm-projectile
(helm-projectile-on)


;;; key bindings

;; helm
(global-set-key (kbd "M-x") 'helm-M-x)
; use ctrl-n for recent files
(evil-define-key 'motion 'global (kbd "C-n") 'helm-mini)
(evil-define-key 'normal 'global (kbd "C-n") 'helm-mini)
; use ctrl-f for occur
(evil-define-key 'motion 'global (kbd "C-f") 'helm-occur)
(evil-define-key 'normal 'global (kbd "C-f") 'helm-occur)
; use ctrl-p for find files
(evil-define-key 'motion 'global (kbd "C-p") 'helm-find-files)
(evil-define-key 'normal 'global (kbd "C-p") 'helm-find-files)
; in helm window move using j and k
(define-key helm-map (kbd "C-j") 'helm-next-line)
(define-key helm-map (kbd "C-k") 'helm-previous-line)
; in file window, move up one level using C-h
(define-key helm-find-files-map (kbd "C-h") 'helm-find-files-up-one-level)

;; evil

; leader
(define-prefix-command 'leader-map)
(evil-define-key 'motion 'global "," 'leader-map)
; use Q for macro record and q for playback
(evil-define-key 'normal 'global "q" 'evil-execute-macro)
(evil-define-key 'motion 'global "Q" 'evil-record-macro)
(evil-define-key 'visual 'global "Q" 'evil-record-macro)
; allow repeat in visual mode
(evil-define-key 'visual 'global "." (kbd ";norm . RET"))
; open line above
(defun smart-open-line-above () (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))
(evil-define-key 'insert 'global (kbd "S-RET") 'smart-open-line-above)
(evil-define-key 'insert 'global (kbd "<S-return>") 'smart-open-line-above)
; macro in visual mode
(evil-define-key 'visual 'global "q" (lambda () (interactive) (evil-ex "'<,'>norm @")))
; use U for redo
(evil-define-key 'normal 'global "U" 'undo-tree-redo)
; use ; for :
(evil-define-key 'motion 'global ";" 'evil-ex)
; repeat last ex command
(evil-define-key 'motion 'global ",." "@:")
; save all
(evil-define-key 'motion 'global ",wa" (kbd ";wa RET"))
; change to last buffer
(evil-define-key 'motion 'global (kbd ", TAB") 'evil-buffer)
(evil-define-key 'motion 'global (kbd ", <tab>") 'evil-buffer)
; kill buffer
(evil-define-key 'motion 'global ",q" 'kill-this-buffer)
; use visual line
(evil-define-key 'motion 'global "j" 'evil-next-visual-line)
(evil-define-key 'motion 'global "k" 'evil-previous-visual-line)
; faster movement
(evil-define-motion fast-move-up () :type exclusive
    (evil-previous-visual-line 5))
(evil-define-motion fast-move-down () :type exclusive
    (evil-next-visual-line 5))
(evil-define-motion fast-move-left () :type exclusive
    (evil-backward-char 8))
(evil-define-motion fast-move-right () :type exclusive
    (evil-forward-char 8))
(evil-define-key 'motion 'global "H" (lambda () (interactive) (evil-backward-char 8)))
(evil-define-key 'motion 'global "J" (lambda () (interactive) (evil-next-visual-line 5)))
(evil-define-key 'motion 'global "K" (lambda () (interactive) (evil-previous-visual-line 5)))
(evil-define-key 'motion 'global "L" (lambda () (interactive) (evil-forward-char 8)))
(evil-define-key 'motion 'global "G" 'evil-first-non-blank)
(evil-define-key 'motion 'global ":" 'evil-end-of-line)
(evil-define-key 'normal 'global "H" 'fast-move-left)
(evil-define-key 'normal 'global "J" 'fast-move-down)
(evil-define-key 'normal 'global "K" 'fast-move-up)
(evil-define-key 'normal 'global "L" 'fast-move-right)
(evil-define-key 'normal 'global "G" 'evil-first-non-blank)
(evil-define-key 'normal 'global ":" 'evil-end-of-line)
(evil-define-key 'visual 'global "H" 'fast-move-left)
(evil-define-key 'visual 'global "J" 'fast-move-down)
(evil-define-key 'visual 'global "K" 'fast-move-up)
(evil-define-key 'visual 'global "L" 'fast-move-right)
(evil-define-key 'visual 'global "G" 'evil-first-non-blank)
(evil-define-key 'visual 'global ":" (lambda () (interactive) (evil-end-of-line)))
; use gG to go to bottom
(evil-define-key 'motion 'global "gG" 'evil-goto-line)
; evil numbers
(evil-define-key 'normal 'global "=" 'evil-numbers/inc-at-pt)
(evil-define-key 'normal 'global "-" 'evil-numbers/dec-at-pt)
(evil-define-key 'visual 'global "=" 'evil-numbers/inc-at-pt)
(evil-define-key 'visual 'global "-" 'evil-numbers/dec-at-pt)
; easy copy and pasting (TODO need some work)
(evil-define-key 'insert 'global (kbd "C-b") (lambda () (interactive) (evil-paste-from-register ?\")))
(evil-define-key 'insert 'global (kbd "C-v") (lambda () (interactive) (evil-paste-from-register ?\"))) ; TODO need some work
(evil-define-key 'visual 'global (kbd "C-c") (lambda () (interactive) (evil-yank))) ; TODO need some work
; search
(evil-define-key 'motion 'global (kbd "SPC") (lambda () (interactive) (evil-ex-search-forward)))
(evil-define-key 'normal help-mode-map (kbd "SPC") (lambda () (interactive) (evil-ex-search-forward)))
(evil-define-key 'motion 'global (kbd "S-SPC") (lambda () (interactive) (evil-ex-search-backward)))
(evil-define-key 'normal help-mode-map (kbd "S-SPC") (lambda () (interactive) (evil-ex-search-backward)))
; use { and } to indent
(evil-define-key 'normal 'global "{" (lambda () (interactive) (evil-shift-left-line 1)))
(evil-define-key 'normal 'global "}" (lambda () (interactive) (evil-shift-right-line 1)))
(evil-define-key 'visual 'global "{" "<gv")
(evil-define-key 'visual 'global "}" ">gv")
; move cursor to comfortable reading position
(evil-define-key 'motion 'global ",z" (lambda () (interactive) (recenter-top-bottom (/ (* (window-total-height) 2) 7))))
; substitute command
(evil-define-key 'normal 'global ",s" (lambda () (interactive) (evil-ex "s/")))
(evil-define-key 'normal 'global ",S" (lambda () (interactive) (evil-ex "%s/")))
(evil-define-key 'visual 'global ",s" (lambda () (interactive) (evil-ex "'<,'>s/")))
; argument text object
(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
; switch color scheme
(evil-define-key 'motion 'global ",CL" (lambda () (interactive) (load-theme light-theme t) (spaceline-compile)))
(evil-define-key 'motion 'global ",CD" (lambda () (interactive) (load-theme dark-theme t) (spaceline-compile)))
; neo tree
(evil-define-key 'motion 'global ",n" (lambda () (interactive) (neotree-show)))
(evil-define-key 'motion 'global ",N" (lambda () (interactive) (neotree-find)))
(evil-define-key 'normal neotree-mode-map
    ; apparently writing neotree-mode-map instead of 'neotree-mode-map works
    "h" (neotree-make-executor :dir-fn 'neo-open-dir)
    "l" (neotree-make-executor :dir-fn 'neo-open-dir)
    "R" 'neotree-refresh
    "r" 'neotree-refresh
    "u" 'neotree-select-up-node
    "U" 'neotree-select-down-node
    "i" 'neotree-change-root
    "a" 'neotree-create-node ; add
    "m" 'neotree-rename-node ; move
    "d" 'neotree-delete-node ; delete
    "c" 'neotree-copy-node ; copy
    "o" 'neotree-enter
    (kbd "RET") 'neotree-enter
    (kbd "<return>") 'neotree-enter
)
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
  ;; C-c .: enter / modify timestamp
  ;; C-c !: enter / modify inactive (no agenda) timestamp
  ;; S-up/down: change date similar to speeddating
  ;; 
  ;; C-c C-c:
  ;; refresh item under cursor
  ;; toggle state of checkbox
  ;; edit tag of item

    ;; custom
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

    (kbd "M-h") help-map
)
(evil-define-key 'visual org-mode-map
    (kbd "C-S-h") (lambda () (interactive) (org-metaleft) (evil-visual-restore)) ; promote/outdent
    (kbd "C-S-l") (lambda () (interactive) (org-metaright) (evil-visual-restore)) ; demote/indent
    (kbd "M-h") help-map

    (kbd "C-j") 'org-next-visible-heading
    (kbd "C-k") 'org-previous-visible-heading
)
; ,<space> no highlight
(evil-define-key 'motion 'global (kbd ", SPC") 'evil-ex-nohighlight)
; easy quit visual mode
(evil-define-key 'visual 'global (kbd ", SPC") 'evil-exit-visual-state)
; m and M for jumping
(evil-define-key 'motion 'global "m" 'evil-jump-backward)
(evil-define-key 'motion 'global "M" 'evil-jump-forward)
(evil-define-key 'normal 'global "m" 'evil-jump-backward)
(evil-define-key 'normal 'global "M" 'evil-jump-forward)
(evil-define-key 'visual 'global "m" 'evil-jump-backward)
(evil-define-key 'visual 'global "M" 'evil-jump-forward)
; map dw cw etc.
(general-nmap "d" (general-key-dispatch 'evil-delete
                   :timeout 0.5
              "w" (general-simulate-key ('evil-delete "aw"))
              "W" (general-simulate-key ('evil-delete "aW"))
              ")" (general-simulate-key ('evil-delete "i)"))
              "]" (general-simulate-key ('evil-delete "i]"))
              "}" (general-simulate-key ('evil-delete "i}"))
              ">" (general-simulate-key ('evil-delete "i>"))
              "'" (general-simulate-key ('evil-delete "i'"))
              "\"" (general-simulate-key ('evil-delete "i\""))
              "t" (general-simulate-key ('evil-delete "it"))
              "n" (general-simulate-key ('evil-delete "gn"))
))
(evil-define-key 'visual 'global "d" 'evil-delete)
(general-nmap "c" (general-key-dispatch 'evil-change
                   :timeout 0.5
              "w" (general-simulate-key ('evil-change "iw"))
              "W" (general-simulate-key ('evil-change "iW"))
              ")" (general-simulate-key ('evil-change "i)"))
              "]" (general-simulate-key ('evil-change "i]"))
              "}" (general-simulate-key ('evil-change "i}"))
              ">" (general-simulate-key ('evil-change "i>"))
              "'" (general-simulate-key ('evil-change "i'"))
              "\"" (general-simulate-key ('evil-change "i\""))
              "t" (general-simulate-key ('evil-change "it"))
              "n" (general-simulate-key ('evil-change "gn"))
))
(evil-define-key 'visual 'global "c" 'evil-change)
(general-nmap "y" (general-key-dispatch 'evil-yank
                   :timeout 0.5
              "w" (general-simulate-key ('evil-yank "iw"))
              "W" (general-simulate-key ('evil-yank "iW"))
              ")" (general-simulate-key ('evil-yank "i)"))
              "]" (general-simulate-key ('evil-yank "i]"))
              "}" (general-simulate-key ('evil-yank "i}"))
              ">" (general-simulate-key ('evil-yank "i>"))
              "'" (general-simulate-key ('evil-yank "i'"))
              "\"" (general-simulate-key ('evil-yank "i\""))
              "t" (general-simulate-key ('evil-yank "it"))
))
(evil-define-key 'visual 'global "y" 'evil-yank)
; join with ,j
(evil-define-key 'normal 'global ",j" 'evil-join)
; break with ,k
(evil-define-key 'normal 'global ",k" 'newline)

;; help mode
(evil-define-key 'motion help-mode-map (kbd "u") 'help-go-back)
(evil-define-key 'normal help-mode-map (kbd "u") 'help-go-back)
(evil-define-key 'motion help-mode-map (kbd "U") 'help-go-forward)
(evil-define-key 'normal help-mode-map (kbd "U") 'help-go-forward)

;; insert mode mappings
(evil-define-key 'insert 'global (kbd "C-l") 'yas-expand)
(evil-define-key 'insert 'global (kbd "C-j") 'yas-next-field)
(evil-define-key 'insert 'global (kbd "C-k") 'yas-prev-field)
(general-imap "j" (general-key-dispatch 'self-insert-command
                   :timeout 0.25
              "j" 'self-insert-command
              "t" (lambda () (interactive) (insert "TODO"))
              "f" (lambda () (interactive) (insert "\\"))
              "k" 'evil-normal-state ; jk quit insert mode
              "l" 'evil-delete-backward-word ; jl delete word
              ";" 'move-end-of-line ; j; move to end of line
              "p" 'company-complete-common-or-cycle ; jp complete
              "[" 'evil-complete-next ; j[ context complete (TODO)
))

;; window management
(evil-define-key 'motion 'global (kbd "C-w C-h") 'evil-window-left)
(evil-define-key 'motion 'global (kbd "C-w C-j") 'evil-window-down)
(evil-define-key 'motion 'global (kbd "C-w C-k") 'evil-window-up)
(evil-define-key 'motion 'global (kbd "C-w C-l") 'evil-window-right)
(evil-define-key 'motion 'global ",wv" 'evil-window-vsplit)
(evil-define-key 'motion 'global ",wh" 'evil-window-split)
(evil-define-key 'motion 'global ",wq" 'evil-quit)
(evil-define-key 'motion 'global ",0" 'select-window-0)
(evil-define-key 'motion 'global ",1" 'select-window-1)
(evil-define-key 'motion 'global ",2" 'select-window-2)
(evil-define-key 'motion 'global ",3" 'select-window-3)
(evil-define-key 'motion 'global ",4" 'select-window-4)
(evil-define-key 'motion 'global ",5" 'select-window-5)
(evil-define-key 'motion 'global ",6" 'select-window-6)
(evil-define-key 'motion 'global ",7" 'select-window-7)
(evil-define-key 'motion 'global ",8" 'select-window-8)
(evil-define-key 'motion 'global ",9" 'select-window-9)

;; nerd commenter
(evil-define-key 'normal 'global (kbd ",c SPC") 'evilnc-comment-or-uncomment-lines)
(evil-define-key 'visual 'global (kbd ",c SPC") 'evilnc-comment-or-uncomment-lines)
(evil-define-key 'normal 'global (kbd ",c y") 'evilnc-copy-and-comment-lines)
(evil-define-key 'visual 'global (kbd ",c y") 'evilnc-copy-and-comment-lines)

;; ace-window
; (evil-define-key 'motion 'global (kbd "TAB") 'ace-window)
; (evil-define-key 'normal 'eshell-mode-map (kbd "TAB") 'ace-window)
; (evil-define-key 'normal 'shell-mode-map (kbd "TAB") 'ace-window)
; (evil-define-key 'motion 'global (kbd "<tab>") 'ace-window)
; (evil-define-key 'normal 'eshell-mode-map (kbd "<tab>") 'ace-window)
; (evil-define-key 'normal 'shell-mode-map (kbd "<tab>") 'ace-window)

;; avy
(evil-define-key 'motion 'global "f" 'avy-goto-word-0)
(evil-define-key 'motion 'global "F" 'avy-goto-char-2)

;; misc bindings
; use alt-h for help instead of ctrl-h
(bind-key* (kbd "M-h") help-map)


;;; misc settings

;; UTF-8 as default encoding
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)

;; scroll-off emulation
(setq scroll-margin (/ (* (window-total-height) 2) 7))

;; wrap lines
(add-hook 'prog-mode-hook (lambda () (setq truncate-lines nil)))
(add-hook 'text-mode-hook (lambda () (setq truncate-lines nil)))

;; auto load if changed
(global-auto-revert-mode t)

;; auto start server if on GUI
(and window-system (server-start))

;; no auto saving
(add-hook 'prog-mode-hook (lambda () (auto-save-mode -1)))
(add-hook 'text-mode-hook (lambda () (auto-save-mode -1)))

;; no backup file
(setq make-backup-files nil)

;; auto display line numbers
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode)))

;; disable blink
(blink-cursor-mode 0)

;; set font
(if (not (boundp 'selected-font)) (progn
  (setq selected-font "DejaVu Sans Mono")
  (cond
    ((find-font (font-spec :name "Consolas"))
    (setq selected-font "Consolas"))
    ((find-font (font-spec :name "Noto Mono"))
    (setq selected-font "Noto Mono"))
)))
(if (not (boundp 'font-size))
    (setq font-size 120))
(set-face-attribute 'default nil
                    :family selected-font
                    :height font-size
                    :weight 'normal
                    :width 'normal)

;; full screen automatically
(toggle-frame-fullscreen)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; enable some modes
; flyspell
(dolist (hook '(prog-mode-hook))
    (add-hook hook (lambda () (flyspell-prog-mode))))
(dolist (hook '(text-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
    (add-hook hook (lambda () (flyspell-mode -1))))
