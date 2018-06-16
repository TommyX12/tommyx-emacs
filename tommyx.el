
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


;;; install packages
(use-package evil :ensure t)
(use-package evil-collection :ensure t :after evil)
(use-package evil-visualstar :ensure t)
(use-package evil-surround :ensure t)
(use-package helm :ensure t)
(use-package helm-flx :ensure t)
(use-package which-key :ensure t)
(use-package spacemacs-theme :ensure t :defer t
             :init (load-theme 'spacemacs-dark t))
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


;;; package settings

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

;; evil-mode
(evil-mode 1) ; use evil-mode at startup
; split to the right and below
(setq evil-split-window-below t)
(setq evil-vsplit-window-right t)
(setq evil-ex-substitute-global t)
; auto center after search
(defun my-center-line (&rest _) (evil-scroll-line-to-center nil))
(advice-add 'evil-search-next :after #'my-center-line)
(advice-add 'evil-search-previous :after #'my-center-line)
(advice-add 'evil-search-word-forward :after #'evil-search-previous)
(advice-add 'evil-search-word-backward :after #'evil-search-next)

;; rainbow-delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'text-mode-hook #'rainbow-delimiters-mode)

;; evil-surround
(global-evil-surround-mode 1)

;; avy
(setq avy-keys '(?w ?e ?r ?u ?i ?o ?p ?a ?s ?d ?g ?h ?j ?k ?l ?v ?n))
(setq avy-all-windows nil)

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

;; evil-collection
(setq evil-collection-setup-minibuffer t)
(evil-collection-init)

;; flyspell lazy
(flyspell-lazy-mode 1)
(setq flyspell-lazy-idle-seconds 1)
(setq flyspell-lazy-window-idle-seconds 2)

;; highlight indent guides
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(add-hook 'text-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character) ; TODO use the character method. right now the font doesn't work
(setq highlight-indent-guides-character ?\|)
(setq highlight-indent-guides-responsive 'nil)

;; evil-visualstar
(global-evil-visualstar-mode)

;; general
(general-evil-setup)

;; flycheck
(global-flycheck-mode)

;; which key
(which-key-mode 1)
(setq which-key-idle-delay 0.5)

;; helm
(require 'helm-config)
(helm-mode 1)
(helm-autoresize-mode 1) ; always auto resize window
(setq helm-split-window-inside-p t)
(setq helm-full-frame nil)
(setq helm-mode-fuzzy-match t)
(setq helm-recentf-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t)
(setq helm-recentf-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t)
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

;; helm-flx
(helm-flx-mode +1)
(setq helm-flx-for-helm-find-files t
      helm-flx-for-helm-locate t)


;;; key bindings

;; helm
(global-set-key (kbd "M-x") 'helm-M-x)
; use ctrl-n for recent files
(evil-define-key 'motion 'global (kbd "C-n") 'helm-mini)
(evil-define-key 'normal 'global (kbd "C-n") 'helm-mini)
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
; kill buffer
(evil-define-key 'motion 'global ",q" 'kill-buffer)
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
; easy copy and pasting (TODO need some work)
(evil-define-key 'insert 'global (kbd "C-b") (lambda () (interactive) (evil-paste-from-register ?\")))
(evil-define-key 'insert 'global (kbd "C-v") (lambda () (interactive) (evil-paste-from-register ?\"))) ; TODO need some work
(evil-define-key 'visual 'global (kbd "C-c") (lambda () (interactive) (evil-yank))) ; TODO need some work
; search
(evil-define-key 'motion 'global (kbd "SPC") (lambda () (interactive) (evil-search-forward)))
(evil-define-key 'normal 'global (kbd "SPC") (lambda () (interactive) (evil-search-forward)))
(evil-define-key 'visual 'global (kbd "SPC") (lambda () (interactive) (evil-search-forward)))
(evil-define-key 'motion 'global (kbd "S-SPC") (lambda () (interactive) (evil-search-backward)))
(evil-define-key 'normal 'global (kbd "S-SPC") (lambda () (interactive) (evil-search-backward)))
(evil-define-key 'visual 'global (kbd "S-SPC") (lambda () (interactive) (evil-search-backward)))
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
; switch color scheme
(evil-define-key 'motion 'global ",CL" (lambda () (interactive) (load-theme 'spacemacs-light t)))
(evil-define-key 'motion 'global ",CD" (lambda () (interactive) (load-theme 'spacemacs-dark t)))
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
              "t" (general-simulate-key ('evil-yank "it"))
))
(evil-define-key 'visual 'global "y" 'evil-yank)
; join with ,j
(evil-define-key 'normal 'global ",j" 'evil-join)
; break with ,k
(evil-define-key 'normal 'global ",k" 'newline)

;; insert mode mappings
(general-imap "j" (general-key-dispatch 'self-insert-command
                   :timeout 0.25
              "j" 'self-insert-command
              "t" (lambda () (interactive) (insert "TODO"))
              "f" (lambda () (interactive) (insert "\\"))
              "k" 'evil-normal-state ; jk quit insert mode
              "l" 'evil-delete-backward-word ; jl delete word
              ";" 'move-end-of-line ; j; move to end of line
              "p" 'evil-complete-next ; jp complete
              "[" 'evil-complete-next-line ; j[ context complete (TODO)
))

;; window management
(evil-define-key 'motion 'global (kbd "C-w C-h") 'evil-window-left)
(evil-define-key 'motion 'global (kbd "C-w C-j") 'evil-window-down)
(evil-define-key 'motion 'global (kbd "C-w C-k") 'evil-window-up)
(evil-define-key 'motion 'global (kbd "C-w C-l") 'evil-window-right)
(evil-define-key 'motion 'global ",wv" 'evil-window-vsplit)
(evil-define-key 'motion 'global ",wh" 'evil-window-split)
(evil-define-key 'motion 'global ",wq" 'evil-quit)

;; ace-window
(evil-define-key 'motion 'global (kbd "TAB") 'ace-window)

;; avy
(evil-define-key 'motion 'global "f" 'avy-goto-word-0)
(evil-define-key 'motion 'global "F" 'avy-goto-char-2)

;; misc bindings
; use alt-h for help instead of ctrl-h
(global-set-key (kbd "M-h") help-map)


;;; misc settings

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
