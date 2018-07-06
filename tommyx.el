;;; add current directory to load-path
(add-to-list 'load-path (file-name-directory load-file-name))
(add-to-list 'load-path
    (expand-file-name "infinity-theme" (file-name-directory load-file-name)))
(add-to-list 'custom-theme-load-path
    (expand-file-name "infinity-theme" (file-name-directory load-file-name)))


;;; themes
; (load-theme 'spacemacs-dark t)
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)
(setq dark-theme 'infinity-dark)
(setq light-theme 'infinity-light)
(if (and (boundp 'use-light-theme) use-light-theme)
    (load-theme light-theme t)
    (load-theme dark-theme t))

;; set font
(if (not (boundp 'selected-font)) (progn
  (setq selected-font "DejaVu Sans Mono")
  (cond
    ((find-font (font-spec :name "Consolas"))
    (setq selected-font "Consolas"))
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
(use-package emms :ensure t
    :config 
    (require 'emms-setup)
    (emms-all)
    (emms-default-players))
(use-package undo-tree :ensure t)
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
(use-package smex :ensure t)
(use-package helm :ensure t)
(use-package helm-flx :ensure t)
(use-package helm-descbinds :ensure t)
(use-package helm-describe-modes :ensure t)
(use-package helm-swoop :ensure t
    :config
    (setq helm-swoop-split-with-multiple-windows t)
)
(use-package helm-projectile :ensure t :after projectile)
(use-package swiper-helm :ensure t)
(use-package ivy :ensure t)
(use-package ivy-posframe :ensure t)
(use-package counsel :ensure t)
(use-package counsel-projectile :ensure t)
(use-package swiper :ensure t)
(use-package which-key :ensure t)
(use-package spacemacs-theme :ensure t :defer t)
(use-package doom-themes :ensure t :defer t)
(use-package ace-window :ensure t)
(use-package general :ensure t)
(use-package beacon :ensure t)
(use-package highlight-indent-guides :ensure t)
(use-package origami :ensure t)
(use-package volatile-highlights :ensure t)
(use-package evil-goggles :ensure t)
(use-package flycheck :ensure t)
(use-package flyspell-lazy :ensure t)
(use-package rainbow-delimiters :ensure t)
(use-package avy :ensure t)
(use-package smartparens :ensure t
	     ; don't show in mode display
	     :diminish smartparens-mode)
(use-package company :ensure t)
(use-package company-childframe :ensure t)
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
(use-package ycmd :ensure t)
(use-package company-ycmd :ensure t)
(use-package flycheck-ycmd :ensure t)
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
(use-package load-relative :ensure t)
(use-package rainbow-mode :ensure t)
(use-package highlight-numbers :ensure t)
; language specific
(use-package csv-mode :ensure t)
(use-package json-mode :ensure t)
;; (use-package vue-mode :ensure t
;;     :config
;;     (setq mmm-submode-decoration-level 0)
;; )
(use-package web-mode :ensure t
    :config
    (setq web-mode-enable-auto-expanding t)
    (setq-default web-mode-markup-indent-offset 2)
    (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.xml?\\'" . web-mode))

    ; use for vue files
    (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
)
(use-package js2-mode :ensure t
    :config
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (setq js2-strict-missing-semi-warning nil)
)
(use-package emmet-mode :ensure t
    :config
    (add-hook 'sgml-mode-hook 'emmet-mode)
    (add-hook 'web-mode-hook 'emmet-mode)
    (add-hook 'css-mode-hook  'emmet-mode)
    (setq emmet-move-cursor-after-expanding t)
    (setq emmet-move-cursor-between-quotes t)
    (setq emmet-indentation 2)
    :bind (:map emmet-mode-keymap
        ("C-j" . nil)
    )
)
;; (use-package js2-refactor :ensure t)


;;; package settings

;; color-identifiers-mode
(setq color-identifiers-coloring-method 'sequential)
(setq color-identifiers:max-color-saturation 0.3)
(setq color-identifiers:min-color-saturation 0.25)
(setq color-identifiers:timer (run-with-idle-timer 5 t 'color-identifiers:refresh))
(add-hook 'prog-mode-hook (lambda () (color-identifiers-mode 1)))
(global-color-identifiers-mode 1)

;; dashboard
(dashboard-setup-startup-hook)
(add-hook 'dashboard-mode-hook (lambda () (hl-line-mode 1)))

;; window-numbering
(window-numbering-mode)

;; yascroll
(global-yascroll-bar-mode 1)
(setq yascroll:delay-to-hide nil)

;; beacon
(setq beacon-blink-when-focused nil) ; may cause problem
(setq beacon-blink-when-buffer-changes t)
(setq beacon-blink-when-window-changes t)
(setq beacon-blink-when-window-scrolls t)
(setq beacon-blink-duration 0.15)
(setq beacon-blink-delay 0.15)
(setq beacon-size 15)
(setq beacon-color "#2499ff")
(beacon-mode 1)

;; git gutter
(setq
    git-gutter:window-width 1
    git-gutter:update-interval 5
    git-gutter:modified-sign "|"
    git-gutter:added-sign "|"
    git-gutter:deleted-sign "-")
(global-git-gutter-mode +1)

;; which-function
(add-hook 'prog-mode-hook #'which-function-mode)
(add-hook 'prog-mode-hook (lambda () (interactive)
    (setq-default header-line-format '(" - " which-func-format))))

;; powerline and spaceline
(setq powerline-default-separator 'slant)
(require 'spaceline-config)
(spaceline-toggle-which-function-off)
(spaceline-toggle-hud-on)
(spaceline-spacemacs-theme)
(spaceline-helm-mode)
(spaceline-compile)
; delayed update trigger for performance
(setq delayed-mode-line-updating nil)
(defun delayed-mode-line-format ()
  "The mode-line format function with auto caching."
  (when delayed-mode-line-updating
    (set-window-parameter nil 'delayed-mode-line-cache (spaceline-ml-main)))
  (window-parameter nil 'delayed-mode-line-cache))
(defun delayed-mode-line-update (&rest _)
  "Updates the mode-line."
  (setq delayed-mode-line-updating t)
    (force-mode-line-update t)
    (run-at-time 0.01 nil (lambda () (setq delayed-mode-line-updating nil))))
(setq-default mode-line-format '("%e" (:eval (delayed-mode-line-format))))
; update mode line
(run-with-idle-timer 0.5 t 'delayed-mode-line-update)
(add-hook 'window-configuration-change-hook 'delayed-mode-line-update)
;; (advice-add 'select-window :after #'delayed-mode-line-update) ; TODO avy-jump calls this too much

;; dashboard
(setq dashboard-items '((recents  . 5)
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
; company posframe (childframe)
(company-childframe-mode 1)
(defun company-childframe-show () ; override function
  "Show company-childframe candidate menu."
  (let* ((height (min company-tooltip-limit company-candidates-length))
         (lines (company--create-lines company-selection height))
         (contents (mapconcat #'identity lines "\n"))
         (buffer (get-buffer-create company-childframe-buffer)))
    (setq contents (copy-sequence contents))
    (remove-text-properties 0 (length contents) '(mouse-face nil) contents)
    (with-current-buffer buffer
      (setq-local overriding-local-map company-childframe-active-map))
    (posframe-show buffer
                   :override-parameters '((border-width . 1) (internal-border-width . 1))
                   :string contents
                   :position (- (point) (length company-prefix))
                   :x-pixel-offset (* -1 company-tooltip-margin (default-font-width))
                   :font company-childframe-font
                   :min-width company-tooltip-minimum-width
                   :background-color (face-attribute 'company-tooltip :background))))
; integration with desktop package if installed
(when (require 'desktop nil 'noerror)
  (push '(company-childframe-mode . nil)
      desktop-minor-mode-table))
;; (company-quickhelp-mode)
(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
     (define-key company-active-map (kbd "<S-tab>") 'company-select-previous)))
(setq company-frontends
      '(company-pseudo-tooltip-unless-just-one-frontend
        ;; company-preview-frontend
        company-tng-frontend
        company-echo-metadata-frontend))
(setq company-idle-delay 0.2)
(setq company-quickhelp-delay nil) ; we will manually trigger the help
(setq company-require-match 'never)
(with-eval-after-load 'company
  (company-flx-mode +1))

;; ycmd
(setq fuck (expand-file-name "third_party/ycmd/ycmd/"
    (file-name-directory load-file-name)))
(setq ycmd-server-command `("python" "-u" ,(expand-file-name "third_party/ycmd/ycmd/"
    (file-name-directory load-file-name))))
(add-hook 'ycmd-mode-hook 'company-ycmd-setup)
(add-hook 'ycmd-mode-hook 'flycheck-ycmd-setup)
; generic file types
(add-hook 'prog-mode-hook (lambda () (ycmd-mode 1)))
(add-hook 'text-mode-hook (lambda () (ycmd-mode 1)))
; java
(add-to-list 'ycmd-file-type-map '(java-mode "java")) ; file type detection
(evil-define-key 'normal java-mode-map (kbd "C-]") 'ycmd-goto) ; goto
; python
(evil-define-key 'normal python-mode-map (kbd "C-]") 'ycmd-goto) ; goto
; elisp
(add-hook 'emacs-lisp-mode-hook (lambda () (ycmd-mode -1))) ; disable ycm

;; smartparens
(require 'smartparens-config)
(smartparens-global-mode 1)
(show-smartparens-global-mode 1)
(setq sp-show-pair-from-inside t)
(setq smartparens-strict-mode nil)
; auto expanison of brackets
(sp-local-pair 'prog-mode "{" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
(sp-local-pair 'text-mode "{" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
(sp-local-pair 'prog-mode "[" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
(sp-local-pair 'text-mode "[" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
(sp-local-pair 'prog-mode "(" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
(sp-local-pair 'text-mode "(" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))

;; evil
(evil-mode 1) ; use evil-mode at startup
; split to the right and below
(setq evil-split-window-below t)
(setq evil-vsplit-window-right t)
(setq evil-ex-substitute-global t)
; auto center after search
(defun my-center-line (&rest _) (evil-scroll-line-to-center nil))
;; (advice-add 'evil-ex-search-next :after #'my-center-line)
;; (advice-add 'evil-ex-search-previous :after #'my-center-line)
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

;; highlight numbers
(add-hook 'prog-mode-hook #'highlight-numbers-mode)
;; (add-hook 'text-mode-hook #'highlight-numbers-mode)

;; evil-surround
(global-evil-surround-mode 1)

;; avy
(setq avy-keys '(?w ?e ?r ?u ?i ?o ?p ?a ?s ?d ?g ?h ?j ?k ?l ?v ?n))
(setq avy-all-windows nil)

;; undo-tree
; attempt to fix bug
(setq undo-tree-enable-undo-in-region nil)
; persistent undo
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/history")))

;; volatile-highlight
;; (vhl/define-extension 'evil
;;                       'evil-move
;;                       'evil-paste-after
;;                       'evil-paste-before
;;                       'evil-paste-pop)
;; (with-eval-after-load 'evil
;;     (vhl/install-extension 'evil)
;;     (vhl/load-extension 'evil))
(vhl/define-extension 'undo-tree
                      'undo-tree-move
                      'undo-tree-yank)
(with-eval-after-load 'undo-tree
    (vhl/install-extension 'undo-tree)
    (vhl/load-extension 'undo-tree))
(volatile-highlights-mode 1)

;; evil-goggles
(setq evil-goggles-pulse nil)
(setq evil-goggles-duration 0.1)
(setq evil-goggles-async-duration 0.2)
(setq evil-goggles-blocking-duration 0.1)
(evil-goggles-mode)

;; neotree
; (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq neo-theme 'nerd)
(setq neo-show-hidden-files t)
(add-hook 'neotree-mode-hook (lambda () (hl-line-mode 1)))
(setq neo-confirm-change-root 'off-p)
(setq neo-banner-message "")
(setq neo-show-updir-line nil)
(setq neo-toggle-window-keep-p t)
(setq neo-window-width 30)
(setq neo-vc-integration '(face))
(add-hook 'after-init-hook (lambda () (neotree-show)))

;; evil-collection
(delete 'neotree evil-collection-mode-list)
(delete 'company evil-collection-mode-list)
(evil-collection-init)

;; flyspell lazy
(flyspell-lazy-mode 1)
(setq flyspell-lazy-idle-seconds 2.5)
(setq flyspell-lazy-window-idle-seconds 5)

;; highlight indent guides (currently disabled)
;; (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
;; (add-hook 'text-mode-hook 'highlight-indent-guides-mode)
;; (setq highlight-indent-guides-method 'character) ; TODO use the character method. right now the font doesn't work
;; (setq highlight-indent-guides-character ?\|)
;; (setq highlight-indent-guides-responsive nil)

;; evil-visualstar
(global-evil-visualstar-mode)

;; general
(general-evil-setup)

;; flycheck
(global-flycheck-mode)

;; which key
(which-key-mode 1)
(setq which-key-idle-delay 0.5)
(setq which-key-allow-evil-operators t)
(setq which-key-show-operator-state-maps t)

;; projectile
(setq projectile-enable-caching t)
(projectile-mode)

;; helm
(require 'helm-config)
;; (setq helm-display-function 'helm-display-buffer-in-own-frame
;;         helm-display-buffer-reuse-frame t
;;         helm-use-undecorated-frame-option t)
;; (helm-mode 1)
(helm-autoresize-mode 1) ; always auto resize window
(setq helm-autoresize-max-height 40)
(setq helm-autoresize-min-height 40)
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
(setq recentf-max-menu-items 250)
(setq recentf-max-saved-items 250)
(setq helm-move-to-line-cycle-in-source nil)
(setq helm-ff-file-name-history-use-recentf t)
(setq helm-follow-mode-persistent t)
(setq helm-source-names-using-follow '("Occur"))

;; helm-flx
(setq helm-flx-for-helm-find-files t
      helm-flx-for-helm-locate t)
(helm-flx-mode +1)

;; helm-descbinds
(helm-descbinds-mode)

;; helm-descbinds-mode
(global-set-key [remap describe-mode] #'helm-describe-modes)

;; helm-projectile
(helm-projectile-on)

;; ivy, counsel and swiper
; main
(ivy-mode 1)
(counsel-mode 1)
; remove initial input in ivy commands
(setq ivy-initial-inputs-alist nil)
; enable fuzzy, except for swiper
(setq ivy-re-builders-alist
      '((swiper . ivy--regex-ignore-order)
        (swiper-multi . ivy--regex-plus)
        (t      . ivy--regex-fuzzy)))
; enable wrapping
(setq ivy-wrap t)
(setq ivy-action-wrap t)
; add recent files and bookmarks to ivy-switch-buffer
(setq ivy-use-virtual-buffers t)
; display functions
(setq ivy-posframe-parameters '(
    (width . 50)
    (border-width . 1) (internal-border-width . 1)
    (min-width . 50)
    (refresh . 1)
    ))
(setq ivy-display-functions-alist
      '((counsel-irony . ivy-posframe-display-at-point)
        (ivy-completion-in-region . ivy-posframe-display-at-point)
        (swiper . ivy-posframe-display-at-window-bottom-left)
        (swiper-multi . ivy-posframe-display-at-window-bottom-left)
        (t . ivy-posframe-display-at-point)))
(ivy-posframe-enable)
(defun ivy-posframe--display (str &optional poshandler) ; override function
  "Show STR in ivy's posframe."
  (if (not (ivy-posframe-workable-p))
      (ivy-display-function-fallback str)
    (with-selected-window (ivy--get-window ivy-last)
      (posframe-show
       ivy-posframe-buffer
       :font ivy-posframe-font
       :string
       (with-current-buffer (get-buffer-create " *Minibuf-1*")
         (let ((point (point))
               (string (if ivy-posframe--ignore-prompt
                           str
                         (concat (buffer-string) "  " str))))
           (add-text-properties (- point 1) point '(face ivy-posframe-cursor) string)
           string))
       :position (point)
       :poshandler poshandler
       :background-color (face-attribute 'ivy-posframe :background)
       :foreground-color (face-attribute 'ivy-posframe :foreground)
       :height ivy-height
       :width (window-width)
       :min-height 10
       :min-width 50
       :override-parameters ivy-posframe-parameters))))
; better UI
(defun ivy-format-function-custom (cands)
  "Transform CANS into a string for minibuffer."
  (ivy--format-function-generic
   (lambda (str)
     (concat "> " (ivy--add-face str 'ivy-current-match)))
   (lambda (str)
     (concat "  " str))
   cands
   "\n"))
(setq ivy-format-function 'ivy-format-function-custom)
;; (setq ivy-format-function 'ivy-format-function-default)
; misc
(setq ivy-height 12)


;;; heavy tasks
(defun update-heavy-tasks () (interactive)
  "Update all the heavy tasks."
  (message "Updating heavy tasks...")
  (color-identifiers:refresh)
  (flyspell-lazy-check-visible)
  (git-gutter:update-all-windows)
  (flycheck-buffer)
  (garbage-collect)
  (message "Done.")
  )


;;; key bindings util functions and motion
(defun selection-or-word-at-point ()
  (cond
   ;; If there is selection use it
   ((and transient-mark-mode
         mark-active
         (not (eq (mark) (point))))
    (let ((mark-saved (mark))
          (point-saved (point)))
      (deactivate-mark)
      (buffer-substring-no-properties mark-saved point-saved)))
   ;; Otherwise, use symbol at point or empty
   (t (format "\\<%s\\>"
              (or (word-at-point)
                  "")))))
(evil-define-motion swiper-movement () :type exclusive
    (swiper))


;;; key bindings

;; use esc (same as "C-[") for escape
(global-set-key (kbd "ESC") 'keyboard-escape-quit)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; global leader
(define-prefix-command 'global-leader-window)
(define-prefix-command 'global-leader-helm)
(define-prefix-command 'global-leader-ivy)
(define-prefix-command 'global-leader-org)
(define-prefix-command 'global-leader-mode-specific)
(general-define-key
    :keymaps 'override
    :states '(motion normal visual)
    :prefix "SPC"

    "w" '(global-leader-window
        :which-key "window")
    "h" '(global-leader-helm
        :which-key "helm")
    "i" '(global-leader-ivy
        :which-key "ivy")
    "o" '(global-leader-org
        :which-key "org")
    "j" '(global-leader-mode-specific
        :which-key "mode specific")

    "x" '(counsel-M-x
        :which-key "counsel M-x")

    "hx" '(helm-M-x
        :which-key "helm M-x")
    "ho" '(helm-occur
        :which-key "helm occur")
    "hs" '(helm-swoop
        :which-key "helm swoop")

)
(general-define-key
    :keymaps 'override
    :states '(visual)
    :prefix "SPC"

    "f" '(swiper-movement
        :which-key "search")

    "*" '((lambda () (interactive) (swiper (selection-or-word-at-point)))
        :which-key "search selection")
    "C-*" '((lambda () (interactive) (swiper-all (selection-or-word-at-point)))
        :which-key "search selection in all buffers")
)
(general-define-key
    :keymaps 'override
    :states '(motion normal)
    :prefix "SPC"

    "f" '(swiper
        :which-key "search")
    "F" '(swiper-all
        :which-key "search in all buffers")

    "*" '((lambda () (interactive) (swiper (selection-or-word-at-point)))
        :which-key "search cursor word")
    "C-*" '((lambda () (interactive) (swiper-all (selection-or-word-at-point)))
        :which-key "search cursor word in all buffers")

    "wh" '((lambda () (interactive) (evil-window-left 1) (delayed-mode-line-update))
        :which-key "move to window left")
    "wj" '((lambda () (interactive) (evil-window-down 1) (delayed-mode-line-update))
        :which-key "move to window down")
    "wk" '((lambda () (interactive) (evil-window-up 1) (delayed-mode-line-update))
        :which-key "move to window up")
    "wl" '((lambda () (interactive) (evil-window-right 1) (delayed-mode-line-update))
        :which-key "move to window right")
    "wd" '((lambda () (interactive) (evil-window-split) (delayed-mode-line-update))
        :which-key "split window horizontally")
    "wv" '((lambda () (interactive) (evil-window-vsplit) (delayed-mode-line-update))
        :which-key "split window vertically")
    "wq" '((lambda () (interactive) (evil-quit) (delayed-mode-line-update))
        :which-key "close window")

    "n" '(ivy-switch-buffer
        :which-key "switch buffer")

    "ii" '(ivy-resume
        :which-key "ivy resume")
    "ip" '(counsel-projectile
        :which-key "counsel projectile")
    "i <tab>" '(projectile-find-other-file
        :which-key "counsel projectile other file")
    "i TAB" '(projectile-find-other-file
        :which-key "counsel projectile other file")
    "iP" '(counsel-projectile-switch-project
        :which-key "counsel projectile project")
    "ir" '(counsel-recentf
        :which-key "counsel recentf")
    "ig" '(counsel-projectile-grep
        :which-key "counsel projectile grep")
    "if" '(counsel-find-file
        :which-key "counsel find files")

    "hh" '(helm-resume
        :which-key "helm resume")
    "hm" '(helm-mini
        :which-key "helm mini")
    "hp" '(helm-projectile
        :which-key "helm projectile")
    "hP" '(helm-projectile-switch-project
        :which-key "helm projectile project")
    "h C-p" '(helm-projectile-find-file-in-known-projects
        :which-key "helm projectile all")
    "h <tab>" '(helm-projectile-find-other-file ; cpp vs h switching
        :which-key "helm projectile other file")
    "h TAB" '(helm-projectile-find-other-file ; cpp vs h switching
        :which-key "helm projectile other file")
    "hr" '(helm-recentf
        :which-key "helm recentf")
    "hg" '(helm-projectile-grep
        :which-key "helm projectile grep")
    "hf" '(helm-find-files
        :which-key "helm find files")
    "hF" '(helm-for-files
        :which-key "helm for files")

    "0" '((lambda () (interactive) (select-window-0) (delayed-mode-line-update))
        :which-key "move to window 0")
    "1"  '((lambda () (interactive) (select-window-1) (delayed-mode-line-update))
        :which-key "move to window 1")
    "2"  '((lambda () (interactive) (select-window-2) (delayed-mode-line-update))
        :which-key "move to window 2")
    "3"  '((lambda () (interactive) (select-window-3) (delayed-mode-line-update))
        :which-key "move to window 3")
    "4"  '((lambda () (interactive) (select-window-4) (delayed-mode-line-update))
        :which-key "move to window 4")
    "5"  '((lambda () (interactive) (select-window-5) (delayed-mode-line-update))
        :which-key "move to window 5")
    "6"  '((lambda () (interactive) (select-window-6) (delayed-mode-line-update))
        :which-key "move to window 6")
    "7"  '((lambda () (interactive) (select-window-7) (delayed-mode-line-update))
        :which-key "move to window 7")
    "8"  '((lambda () (interactive) (select-window-8) (delayed-mode-line-update))
        :which-key "move to window 8")
    "9"  '((lambda () (interactive) (select-window-9) (delayed-mode-line-update))
        :which-key "move to window 9")

    "TAB" '(evil-buffer
        :which-key "switch to other buffer")
)

;; evil

; disable C-z
(global-set-key (kbd "C-z") nil)
(evil-define-key 'motion 'global (kbd "C-z") nil)
(evil-define-key 'normal 'global (kbd "C-z") nil)
(evil-define-key 'visual 'global (kbd "C-z") nil)
(evil-define-key 'insert 'global (kbd "C-z") nil)
; leader
(define-prefix-command 'leader-map)
(evil-define-key 'motion 'global "," 'leader-map)
; manually update things
(evil-define-key 'motion 'global ",r" #'update-heavy-tasks)
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
; sane tabbing
(evil-define-key 'insert 'global (kbd "TAB") 'tab-to-tab-stop)
(evil-define-key 'insert 'global (kbd "<tab>") 'tab-to-tab-stop)
; scrolling
(evil-define-key 'motion 'global (kbd "M-j") 'evil-scroll-down)
(evil-define-key 'motion 'global (kbd "M-k") 'evil-scroll-up)
; change to last buffer
(evil-define-key 'motion 'global (kbd ", TAB") 'evil-buffer)
(evil-define-key 'motion 'global (kbd ", <tab>") 'evil-buffer)
; kill buffer
(evil-define-key 'motion 'global ",q" 'kill-this-buffer)
; ,d delete line content
(evil-define-key 'normal 'global ",d" (lambda () (interactive) (evil-first-non-blank) (kill-line)))
; ,f fix spelling
(evil-define-key 'normal 'global ",f" 'flyspell-auto-correct-word)
(evil-define-key 'normal 'global ",F" (lambda () (interactive) (flyspell-lazy-check-visible) (flyspell-auto-correct-previous-word (point))))
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
;; (evil-define-key 'motion 'global (kbd "SPC") (lambda () (interactive) (evil-ex-search-forward)))
;; (evil-define-key 'normal 'global (kbd "SPC") (lambda () (interactive) (evil-ex-search-forward)))
;; (evil-define-key 'visual 'global (kbd "SPC") (lambda () (interactive) (evil-ex-search-forward)))
;; (evil-define-key 'motion 'global (kbd "S-SPC") (lambda () (interactive) (evil-ex-search-backward)))
;; (evil-define-key 'normal 'global (kbd "S-SPC") (lambda () (interactive) (evil-ex-search-backward)))
;; (evil-define-key 'visual 'global (kbd "S-SPC") (lambda () (interactive) (evil-ex-search-backward)))
;; (evil-define-key 'normal help-mode-map (kbd "SPC") (lambda () (interactive) (evil-ex-search-forward)))
;; (evil-define-key 'normal help-mode-map (kbd "S-SPC") (lambda () (interactive) (evil-ex-search-backward)))
;; (evil-define-key 'normal neotree-mode-map (kbd "SPC") (lambda () (interactive) (evil-ex-search-forward)))
;; (evil-define-key 'normal neotree-mode-map (kbd "S-SPC") (lambda () (interactive) (evil-ex-search-backward)))
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
; narrowing
(evil-define-key 'motion 'global ",,n" 'narrow-to-defun)
(evil-define-key 'motion 'global ",,N" 'widen)
; switch color scheme
;; (evil-define-key 'motion 'global ",CL" (lambda () (interactive) (load-theme light-theme t)))
;; (evil-define-key 'motion 'global ",CD" (lambda () (interactive) (load-theme dark-theme t)))
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
; ,<space> no highlight
(evil-define-key 'motion 'global (kbd ", SPC") (lambda () (interactive) (evil-ex-nohighlight) (beacon-blink)))
; easy quit visual mode
(evil-define-key 'visual 'global (kbd ", SPC") (lambda () (interactive) (evil-exit-visual-state) (beacon-blink)))
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
(evil-define-key 'visual 'global ",j" 'evil-join)
; break with ,k
(evil-define-key 'normal 'global ",k" 'newline)

;; helm
;; (global-set-key (kbd "M-x") 'helm-M-x)
(evil-define-key 'motion 'global (kbd ", C-x") 'helm-resume)
; use ctrl-n for buffer and recent files
(evil-define-key 'motion 'global (kbd "C-n") 'helm-mini)
(evil-define-key 'normal 'global (kbd "C-n") 'helm-mini)
; use ctrl-f for occur
(evil-define-key 'motion 'global (kbd "C-f") 'helm-occur)
(evil-define-key 'normal 'global (kbd "C-f") 'helm-occur)
; use ctrl-p for all the stuff
; (note that adding C-u can make projectile force refresh the cache)
;; (evil-define-key 'normal 'global (kbd "C-p") nil)
;; (evil-define-key 'motion 'global (kbd "C-p C-p") 'helm-projectile)
;; (evil-define-key 'motion 'global (kbd "C-p C-S-p") 'helm-projectile-find-file-in-known-projects)
;; (evil-define-key 'motion 'global (kbd ", S-TAB") 'helm-projectile-find-other-file) ; cpp vs h switching
;; (evil-define-key 'motion 'global (kbd ", <S-tab>") 'helm-projectile-find-other-file) ; cpp vs h switching
;; (evil-define-key 'motion 'global (kbd "C-p p") 'helm-projectile-switch-project)
;; (evil-define-key 'motion 'global (kbd "C-p r") 'helm-recentf)
;; (evil-define-key 'motion 'global (kbd "C-p C-f") 'helm-projectile-grep)
;; (evil-define-key 'motion 'global (kbd "C-p f") 'helm-find-files)
; in helm window move using j and k
(define-key helm-map (kbd "C-j") 'helm-next-line)
(define-key helm-map (kbd "C-k") 'helm-previous-line)
; in file window, move up one level using C-h
(define-key helm-find-files-map (kbd "C-h") 'helm-find-files-up-one-level)

;; ivy, counsel and swiper
(general-define-key
    :keymaps '(swiper-map ivy-minibuffer-map)
    "C-j" 'ivy-next-line
    "C-k" 'ivy-previous-line
    "C-S-j" 'ivy-scroll-up-command
    "C-S-k" 'ivy-scroll-down-command
    ; ivy-next-history-element allows inserting cursor symbol.
    "M-j" 'ivy-next-history-element
    "M-k" 'ivy-previous-history-element
    "C-RET" 'ivy-dispatching-done
    "<C-return>" 'ivy-dispatching-done
    "S-RET" 'ivy-immediate-done ; use exact input, not candidate
    "<S-return>" 'ivy-immediate-done
    "C-l" 'ivy-done
)
(general-define-key ; use / to enter directory, not ENTER.
    :keymaps '(counsel-find-file-map)
    ; use return for opening directory
    "RET" 'ivy-alt-done
    "<return>" 'ivy-alt-done
    "S-RET" 'ivy-immediate-done ; use exact input, not candidate
    "<S-return>" 'ivy-immediate-done
    "C-l" 'ivy-alt-done
)

;; help mode
(evil-define-key 'motion help-mode-map (kbd "u") 'help-go-back)
(evil-define-key 'normal help-mode-map (kbd "u") 'help-go-back)
(evil-define-key 'motion help-mode-map (kbd "U") 'help-go-forward)
(evil-define-key 'normal help-mode-map (kbd "U") 'help-go-forward)

;; insert mode mappings
; yas
(evil-define-key 'insert 'global (kbd "C-j") 'yas-next-field)
(evil-define-key 'insert 'global (kbd "C-k") 'yas-prev-field)
(evil-define-key 'insert 'global (kbd "C-l") 'yas-expand)
; emmet
(evil-define-key 'insert emmet-mode-keymap (kbd "C-S-j") 'yas-next-field)
(evil-define-key 'insert emmet-mode-keymap (kbd "C-S-k") 'yas-prev-field)
(evil-define-key 'insert emmet-mode-keymap (kbd "C-S-l") 'yas-expand)
(evil-define-key 'insert emmet-mode-keymap (kbd "C-j") 'emmet-next-edit-point)
(evil-define-key 'insert emmet-mode-keymap (kbd "C-k") 'emmet-prev-edit-point)
(evil-define-key 'insert emmet-mode-keymap (kbd "C-l") 'emmet-expand-line)
(evil-define-key 'visual emmet-mode-keymap (kbd "C-l") 'emmet-wrap-with-markup)

(evil-define-key 'insert 'global (kbd "S-SPC S-SPC") (lambda () (interactive) (save-excursion (flyspell-lazy-check-pending) (flyspell-auto-correct-previous-word (point)))))
(evil-define-key 'insert 'global (kbd "<S-space> <S-space>") (lambda () (interactive) (save-excursion (flyspell-lazy-check-pending) (flyspell-auto-correct-previous-word (point)))))
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
(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "C-z") 'company-quickhelp-manual-begin)))

;; window management
(evil-define-key 'motion 'global (kbd "C-w C-h") (lambda () (interactive) (evil-window-left 1) (delayed-mode-line-update)))
(evil-define-key 'motion 'global (kbd "C-w C-j") (lambda () (interactive) (evil-window-down 1) (delayed-mode-line-update)))
(evil-define-key 'motion 'global (kbd "C-w C-k") (lambda () (interactive) (evil-window-up 1) (delayed-mode-line-update)))
(evil-define-key 'motion 'global (kbd "C-w C-l") (lambda () (interactive) (evil-window-right 1) (delayed-mode-line-update)))
;; (evil-define-key 'motion 'global ",wv" (lambda () (interactive) (evil-window-vsplit) (delayed-mode-line-update)))
;; (evil-define-key 'motion 'global ",wh" (lambda () (interactive) (evil-window-split) (delayed-mode-line-update)))
;; (evil-define-key 'motion 'global ",wq" (lambda () (interactive) (evil-quit) (delayed-mode-line-update)))
;; (evil-define-key 'motion 'global ",0" (lambda () (interactive) (select-window-0) (delayed-mode-line-update)))
;; (evil-define-key 'motion 'global ",1" (lambda () (interactive) (select-window-1) (delayed-mode-line-update)))
;; (evil-define-key 'motion 'global ",2" (lambda () (interactive) (select-window-2) (delayed-mode-line-update)))
;; (evil-define-key 'motion 'global ",3" (lambda () (interactive) (select-window-3) (delayed-mode-line-update)))
;; (evil-define-key 'motion 'global ",4" (lambda () (interactive) (select-window-4) (delayed-mode-line-update)))
;; (evil-define-key 'motion 'global ",5" (lambda () (interactive) (select-window-5) (delayed-mode-line-update)))
;; (evil-define-key 'motion 'global ",6" (lambda () (interactive) (select-window-6) (delayed-mode-line-update)))
;; (evil-define-key 'motion 'global ",7" (lambda () (interactive) (select-window-7) (delayed-mode-line-update)))
;; (evil-define-key 'motion 'global ",8" (lambda () (interactive) (select-window-8) (delayed-mode-line-update)))
;; (evil-define-key 'motion 'global ",9" (lambda () (interactive) (select-window-9) (delayed-mode-line-update)))

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
(evil-define-motion adaptive-avy () :type exclusive
    (if hl-line-mode (evil-avy-goto-line) (evil-avy-goto-word-0 nil)))
(evil-define-key 'motion 'global "f" 'adaptive-avy)
(evil-define-key 'motion 'global "F" 'evil-avy-goto-char-2)

;; misc bindings
; use alt-h for help instead of ctrl-h
(bind-key* (kbd "M-h") help-map)
(bind-key* (kbd "M-h M-h") 'counsel-apropos)


;;; misc settings

;; no alert sounds
(setq ring-bell-function 'ignore)

;; file-type based syntax entry
(add-hook 'prog-mode-hook (lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'lisp-mode-hook (lambda () (modify-syntax-entry ?- "w")))
(add-hook 'emacs-lisp-mode-hook (lambda () (modify-syntax-entry ?- "w")))
(add-hook 'html-mode-hook (lambda () (modify-syntax-entry ?- "w") (modify-syntax-entry ?_ "w")))
(add-hook 'nxml-mode-hook (lambda () (modify-syntax-entry ?- "w") (modify-syntax-entry ?_ "w")))
(add-hook 'css-mode-hook (lambda () (modify-syntax-entry ?- "w") (modify-syntax-entry ?_ "w")))

;; indent settings
(setq-default indent-tabs-mode nil) ; use space instead of tabs
(setq-default tab-width 4)
(setq-default python-indent 4)
(add-hook 'prog-mode-hook (lambda () (setq evil-shift-width tab-width)))
(add-hook 'python-mode-hook (lambda ()
    (setq-local python-indent-offset 4)
    (setq-local python-indent 4)
    (setq-local evil-shift-width python-indent)
    (setq-local tab-width python-indent)
))
(add-hook 'web-mode-hook (lambda ()
    (setq-local evil-shift-width 2)
    (setq-local tab-width 2)
))

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
(add-hook 'sgml-mode-hook (lambda () (display-line-numbers-mode)))

;; indicate end of buffer
(add-hook 'prog-mode-hook (lambda () (setq indicate-buffer-boundaries t)))
(add-hook 'text-mode-hook (lambda () (setq indicate-buffer-boundaries t)))

;; disable blink
(blink-cursor-mode 0)

;; hl-line-mode for some modes
(add-hook 'buffer-menu-mode-hook (lambda () (hl-line-mode 1)))

;; cursor line (right now disabled)
;; (global-hl-line-mode 1)
;; (setq global-hl-line-sticky-flag t)

;; garbage collection (improve some performance)
(setq gc-cons-threshold 200000000)
(run-with-idle-timer 5 t (lambda () (garbage-collect)))
(add-hook 'focus-out-hook (lambda () (garbage-collect)))

;; window divider
(setq window-divider-default-places 'right-only)
(setq window-divider-default-right-width 2)
(setq window-divider-default-bottom-width 2)
(window-divider-mode 1)

;; enable some modes
; flyspell
(dolist (hook '(prog-mode-hook))
    (add-hook hook (lambda () (flyspell-prog-mode))))
(dolist (hook '(text-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
    (add-hook hook (lambda () (flyspell-mode -1))))


;;; org
(load-relative "./tommyx-org.el")
