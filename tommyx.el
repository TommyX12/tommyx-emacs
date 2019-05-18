;; emergency key setting for debug
(global-set-key (kbd "M-C-X") 'execute-extended-command)

;;; add directories to load-path
;; TODO make sure these overrides package archive install directories
(add-to-list 'load-path (file-name-directory load-file-name))
(add-to-list 'load-path
	(expand-file-name "infinity-theme" (file-name-directory load-file-name)))
(add-to-list 'custom-theme-load-path
	(expand-file-name "infinity-theme" (file-name-directory load-file-name)))
(add-to-list 'load-path
	(expand-file-name "packages" (file-name-directory load-file-name)))
(add-to-list 'load-path
	(expand-file-name "packages/company-tabnine" (file-name-directory load-file-name)))
(add-to-list 'load-path
	(expand-file-name "packages/Highlight-Indentation-for-Emacs" (file-name-directory load-file-name)))

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
    (setq selected-font "Menlo")))))
(if (not (boundp 'font-size-small))
	(setq font-size-small 120))
(if (not (boundp 'font-size-big))
	(setq font-size-big 150))
(set-face-attribute 'default nil
					:family selected-font
					:height font-size-small
					:weight 'normal
					:width 'normal)
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

;; full screen automatically
(toggle-frame-fullscreen)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)


;;; packages settings before loading

;; evil
(setq evil-want-Y-yank-to-eol t)
(setq evil-want-integration nil)
; https://github.com/emacs-evil/evil-collection/issues/60
(setq evil-want-keybinding nil)
(setq evil-search-module 'evil-search)

;; evil-collection
(setq evil-collection-company-use-tng nil)
(setq evil-collection-setup-minibuffer nil)


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


;;; install packages
(require 'redo+)
(require 'font-lock+)
(require 'hl-line+)
(require 'info+)
(use-package package-lint :ensure t)
(use-package dash :ensure t)
(use-package ht :ensure t)
(use-package s :ensure t)
(use-package cl-lib :ensure t)
(use-package htmlize :ensure t)
(use-package request :ensure t
	:config
  (when (eq system-type 'windows-nt)
    (setq request-backend 'url-retrieve)) ; curl is slow on windows
)
(use-package json :ensure t)
(use-package unicode-escape :ensure t)
(use-package alert :ensure t
	:config
	(setq alert-default-style 'companion)
)
(use-package emms :ensure t
	:config
	(require 'emms-setup)
	(require 'emms-player-simple)
	(emms-all)
	(emms-default-players)
  (setq emms-repeat-playlist t)
  (setq emms-random-playlist nil)
  (add-hook 'emms-playlist-mode-hook
            (lambda ()
              (hl-line-mode 1)
              (setq-local use-line-nav t)))
  (when (and (bound-and-true-p emms-default-music-dir))
    (emms-add-directory-tree emms-default-music-dir)))
;; (use-package undo-tree :ensure t)
(use-package all-the-icons :ensure t)
(use-package evil :ensure t)
(use-package evil-collection :ensure t :after evil
  :init
  ;; do not allow certain keys to be used by evil-collection
  (setq evil-collection-key-blacklist
        '("K")))
(use-package evil-visualstar :ensure t)
(use-package evil-surround :ensure t)
(use-package evil-args :ensure t)
(use-package evil-matchit :ensure t :defer t)
(use-package evil-numbers :ensure t)
(use-package evil-exchange :ensure t
	:config
	(setq evil-exchange-key (kbd "x"))
	(setq evil-exchange-cancel-key (kbd ",x"))
	(evil-exchange-install)
)
(use-package evil-search-highlight-persist :ensure t)
(use-package evil-nerd-commenter :ensure t)
(use-package hydra :ensure t
	:config
	(setq lv-use-separator t)
	(setq hydra-lv nil)
)
(use-package projectile :ensure t)
(require 'per-frame-header-mode-line)
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
(use-package ivy-posframe :ensure t :after ivy)
(use-package all-the-icons-ivy :ensure t :after ivy
	:config
	(setq all-the-icons-ivy-buffer-commands
				'(ivy-switch-buffer ivy-switch-buffer-other-window counsel-projectile-switch-to-buffer))
	(setq all-the-icons-ivy-file-commands
				'(counsel-find-file counsel-file-jump counsel-recentf counsel-projectile-find-file counsel-projectile-find-dir))
	(all-the-icons-ivy-setup)
)
(use-package ivy-rich :ensure t
  :config
  (setq
   ivy-rich--display-transformers-list
   '(counsel-M-x
     (:columns
      ((counsel-M-x-transformer
        (:width 40))
       (ivy-rich-counsel-function-docstring
        (:face font-lock-doc-face))))
     counsel-describe-function
     (:columns
      ((counsel-describe-function-transformer
        (:width 40))
       (ivy-rich-counsel-function-docstring
        (:face font-lock-doc-face))))
     counsel-describe-variable
     (:columns
      ((counsel-describe-variable-transformer
        (:width 40))
       (ivy-rich-counsel-variable-docstring
        (:face font-lock-doc-face))))
     counsel-recentf
     (:columns
      ((ivy-rich-candidate
        (:width 0.8))
       (ivy-rich-file-last-modified-time
        (:face font-lock-comment-face))))))
  (ivy-rich-mode 1))
;; TODO: There is a bug. Might cause closing some window to close emacs.
;; (use-package popwin :ensure t
;; 	:config
;; 	(setq popwin:adjust-other-windows t)
;; 	(popwin-mode 1)
;; 	(add-hook 'popwin:after-popup-hook (lambda () (delayed-mode-line-update))))
(use-package counsel :ensure t)
(use-package counsel-projectile :ensure t :after projectile)
(use-package google-this :ensure t)
(use-package swiper :ensure t)
(use-package which-key :ensure t
	:config
	(which-key-mode 1)
	(setq which-key-popup-type 'side-window)
	(setq which-key-sort-order 'which-key-prefix-then-key-order-reverse)
	(setq which-key-idle-delay 0.5)
	(setq which-key-idle-secondary-delay 0.1)
	;; (setq which-key-allow-evil-operators t)
	(setq which-key-show-operator-state-maps t)
	(setq which-key-binding-filter-function
		(lambda (cell prefix)
			(cond
			((string-match "move to window 1" (cdr cell)) '("[0-9]" . "move to window [0-9]"))
			((string-match "move to window [0-9]" (cdr cell)) nil)
			(cell))))
)
(use-package spacemacs-theme :ensure t :defer t)
(use-package doom-themes :ensure t :defer t)
(use-package ace-window :ensure t)
(use-package general :ensure t)
(use-package beacon :ensure t)
;; (use-package highlight-indent-guides :ensure t
;;   :config
;;   (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
;;   (add-hook 'text-mode-hook 'highlight-indent-guides-mode)
;;   (setq highlight-indent-guides-method 'character)
;;   (setq highlight-indent-guides-auto-odd-face-perc 3)
;;   (setq highlight-indent-guides-auto-even-face-perc 6)
;;   (setq highlight-indent-guides-character ?\|)
;;   (setq highlight-indent-guides-responsive nil)
;; )
;; (require 'visual-indentation-mode)
;; (require 'highlight-indent-guides) ; my own version
;; (require 'indent-hint)

;; (use-package highlight-indentation :ensure t
;;   :config
;;   (add-hook 'prog-mode-hook 'highlight-indentation-mode)
;;   (add-hook 'text-mode-hook 'highlight-indentation-mode))

(require 'highlight-indentation)
(setq highlight-indentation-blank-lines t)
(add-hook 'prog-mode-hook 'highlight-indentation-mode)
(add-hook 'text-mode-hook 'highlight-indentation-mode)

(require 'origami)
(require 'crosshairs)
;; (use-package origami :ensure t)
(use-package volatile-highlights :ensure t)
(use-package evil-goggles :ensure t)
(use-package flycheck :ensure t)
(use-package flyspell-lazy :ensure t)
(use-package rainbow-delimiters :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'text-mode-hook #'rainbow-delimiters-mode))
(use-package avy :ensure t)
(use-package smartparens :ensure t
		 ; don't show in mode display
		 :diminish smartparens-mode)
(use-package lsp-mode :ensure t)
(use-package lsp-ui :ensure t :after lsp-mode
	:config
	;; (add-hook 'lsp-mode-hook 'lsp-ui-mode) ; TODO disabled for performance reasons
)
(use-package company :ensure t
  :config
  (make-local-variable 'company-backends))
(use-package company-posframe :ensure t :after company
	:config
	; company posframe (childframe)
	;; (company-posframe-mode 1) ; TODO turned off for possible performance reasons
	(defun company-posframe-show () ; override function
		"Show company-posframe candidate menu."
		(let* ((height (min company-tooltip-limit company-candidates-length))
			(lines (company--create-lines company-selection height))
			(contents (mapconcat #'identity lines "\n"))
			(buffer (get-buffer-create company-posframe-buffer)))
		(setq contents (copy-sequence contents))
		(remove-text-properties 0 (length contents) '(mouse-face nil) contents)
		(with-current-buffer buffer
			(setq-local overriding-local-map company-posframe-active-map))
		(posframe-show
     buffer
		 :override-parameters '((border-width . 1)
                            (internal-border-width . 1)
                            (undecorated . t))
	   :height 10
	   :width (window-width) ; (if full-width (window-width) nil)
		 :string contents
		 :position (- (point) (length company-prefix))
		 :x-pixel-offset (* -1 company-tooltip-margin (default-font-width))
		 :font company-posframe-font
		 :min-width company-tooltip-minimum-width
		 :background-color (face-attribute 'company-tooltip :background))))
	; integration with desktop package if installed
	(when (require 'desktop nil 'noerror)
		(push '(company-posframe-mode . nil)
			desktop-minor-mode-table)))
;; (use-package company-box :ensure t :after company
;; 	:hook (company-mode-hook . company-box-mode))
(use-package company-quickhelp :ensure t)
(use-package company-flx :ensure t)
;; (use-package company-lsp :ensure t :after lsp-mode
;; 	:config
;;   (setq-default company-backends
;;                 (cons #'company-lsp company-backends)))
(use-package company-ycmd :ensure t 
  :config
  (setq-default company-backends
                (cons #'company-ycmd company-backends)))

;; (require 'company-tabnine)
(use-package company-tabnine :ensure t :after company
  :config
  (setq-default company-backends
                (cons #'company-tabnine company-backends))

  ;; workaround for company-flx-mode and other transformers
  (setq company-tabnine--disable-next-transform nil)
  (defun my-company--transform-candidates (func &rest args)
    (if (not company-tabnine--disable-next-transform)
        (apply func args)
      (setq company-tabnine--disable-next-transform nil)
      (car args)))

  (defun my-company-tabnine (func &rest args)
    (when (eq (car args) 'candidates)
      (setq company-tabnine--disable-next-transform t))
    (apply func args))
  
  (advice-add #'company--transform-candidates :around #'my-company--transform-candidates)
  (advice-add #'company-tabnine :around #'my-company-tabnine)
  )

(use-package yasnippet :ensure t
  :bind
  (:map yas-minor-mode-map
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
(use-package flycheck-ycmd :ensure t)
(use-package yasnippet-snippets :ensure t)
(use-package powerline :ensure t)
(use-package powerline-evil :ensure t)
(use-package spaceline :ensure t)
(use-package spaceline-all-the-icons :ensure t :after all-the-icons spaceline
	:config
	;; (spaceline-all-the-icons-theme)
)
;; (use-package ecb :ensure t
;; 	:config
;; 	(require 'ecb)
;; 	(setq ecb-layout-name "right1")
;; )
;; (use-package sublimity :ensure t
;; 	:config
;; 	(require 'sublimity-scroll
;; 	(require 'sublimity-map)
;; 	(require 'sublimity-attractive)
;; )
;; (use-package minimap :ensure t
;; 	:config
;; )
(use-package winum :ensure t
	:config
	(winum-mode)
)
;; (use-package symon :ensure t
;; 	:config
;; 	(symon-mode)
;; )
(use-package which-func :ensure t)
(use-package workgroups :ensure t)
;; (use-package persp-mode :ensure t
;; 	:config
;; 	(persp-mode)
;; )
(use-package git-gutter :ensure t
  :config
  (setq
	 git-gutter:window-width 1
	 git-gutter:update-interval 5
	 git-gutter:modified-sign " "
	 git-gutter:added-sign " "
	 git-gutter:deleted-sign " "
   git-gutter:visual-line nil)
  (global-git-gutter-mode +1))
;; (use-package git-gutter-fringe :ensure t
;;   :config

;;   (fringe-helper-define 'git-gutter-fr:added 'center
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX...")

;;   (fringe-helper-define 'git-gutter-fr:deleted 'center
;;     "........"
;;     "........"
;;     "........"
;;     "XXXXXXXX"
;;     "XXXXXXXX"
;;     "........"
;;     "........"
;;     "........")

;;   (fringe-helper-define 'git-gutter-fr:modified 'center
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX..."
;;     "...XX...")
;;   (setq
;; 	 git-gutter:update-interval 5
;; 	 git-gutter:modified-sign ""
;; 	 git-gutter:added-sign ""
;; 	 git-gutter:deleted-sign ""
;;    git-gutter:window-width nil)

;;   (global-git-gutter-mode +1))
(use-package yascroll :ensure t)
(use-package color-identifiers-mode :ensure t
  :config
  ;; color-identifiers-mode
  (setq color-identifiers-avoid-faces
        '(font-lock-warning-face error))
  (setq color-identifiers-coloring-method 'sequential)
  (setq color-identifiers:max-color-saturation 0.3)
  (setq color-identifiers:min-color-saturation 0.3)
  (setq color-identifiers:timer (run-with-idle-timer 5 t 'color-identifiers:refresh))
  (add-hook 'prog-mode-hook (lambda () (color-identifiers-mode 1)))
  (global-color-identifiers-mode 1)

  ;; extra modes support
  (let ((extra-modes
         '(csharp-mode
           glsl-mode)))
    (dolist (maj-mode extra-modes)
      (color-identifiers:set-declaration-scan-fn
       maj-mode 'color-identifiers:cc-mode-get-declarations)
      (add-to-list
       'color-identifiers:modes-alist
       `(,maj-mode . (""
                      "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
                      (nil font-lock-variable-name-face))))))
  (let ((extra-modes
         '(typescript-mode)))
    (dolist (maj-mode extra-modes)
      (add-to-list
       'color-identifiers:modes-alist
       `(,maj-mode . ("[^.][[:space:]]*"
                     "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
                     (nil font-lock-variable-name-face)))))))
(use-package auto-highlight-symbol :ensure t
	:config
	(push 'sql-mode ahs-modes)
	(push 'racket-mode ahs-modes)
	(push 'haskell-mode ahs-modes)
	(push 'web-mode ahs-modes)
	(push 'js2-mode ahs-modes)
	(push 'glsl-mode ahs-modes)
	(push 'typescript-mode ahs-modes)
	(push 'shaderlab-mode ahs-modes)
	(global-auto-highlight-symbol-mode 1)
	;; (add-hook 'prog-mode-hook (auto-highlight-symbol-mode 1))
	;; (add-hook 'html-mode-hook (auto-highlight-symbol-mode 1))
	;; (add-hook 'nxml-mode-hook (auto-highlight-symbol-mode 1))
	(setq ahs-idle-interval 0.3)
	(setq ahs-case-fold-search nil)

	; patch to not clear highlight when moving
	(defun ahs-highlight (symbol beg end)
  "Highlight"
  (setq ahs-search-work  nil
        ahs-need-fontify nil)
  (let ((search-range (ahs-prepare-highlight symbol)))
    (when (consp search-range)
      ;;(msell-bench
       (ahs-search-symbol symbol search-range)
       (when ahs-need-fontify
         (ahs-fontify))
       (ahs-light-up)
      ;;)
      (when ahs-overlay-list
        (ahs-highlight-current-symbol beg end)
        (setq ahs-highlighted  t
              ahs-start-point  beg
              ahs-search-work  nil
              ahs-need-fontify nil) t))))

	; patch to fix avy bug
	(defun ahs-idle-function ()
  "Idle function. Called by `ahs-idle-timer'."
  (when (and auto-highlight-symbol-mode)
		(when ahs-highlighted
			(ahs-unhighlight))
    (let ((hl (ahs-highlight-p)))
      (when hl
        (ahs-highlight (nth 0 hl)
                       (nth 1 hl)
                       (nth 2 hl))))))

)
(use-package neotree :ensure t
	:config
	
	(setq neo-buffer-name "*Files*")
	(setq neo-theme (if (display-graphic-p) 'icons 'nerd))
	;; (setq neo-theme 'nerd)
	(setq neo-show-hidden-files t)
	(add-hook 'neotree-mode-hook (lambda ()
		(hl-line-mode 1)
    (yascroll-bar-mode -1)
    (make-local-variable 'face-remapping-alist)
    (add-to-list 'face-remapping-alist '(default sidebar-background))
    (setq-local left-fringe-width 0)
    (setq-local right-fringe-width 0)
		(setq-local use-line-nav t)
    (setq-local highlight-indentation-offset 2)
    (highlight-indentation-mode 1)))
	(setq neo-confirm-change-root 'off-p)
	(setq neo-banner-message "")
	(setq neo-show-updir-line nil)
	(setq neo-toggle-window-keep-p t)
	(setq neo-window-width 30)
	(setq neo-vc-integration '(face))
	(setq neo-mode-line-type 'default) ; for performance reason
	(setq neo-auto-indent-point t)
)
(use-package magit :ensure t
  :config
  (setq magit-display-buffer-function
        #'magit-display-buffer-fullframe-status-v1))
(use-package page-break-lines :ensure t)
(use-package dashboard :ensure t :after page-break-lines)
(use-package org :ensure org-plus-contrib)
(use-package org-super-agenda :ensure t)
(use-package org-journal :ensure t)
(use-package org-pomodoro :ensure t)
(use-package org-bullets :ensure t
	:config)
(use-package org-preview-html :ensure t
  :config)
(use-package helm-org-rifle :ensure t
  :config)
(use-package outshine :ensure t
  :config)
(use-package load-relative :ensure t)
;; (use-package fancy-battery :ensure t)
(use-package rainbow-mode :ensure t)
(use-package highlight-numbers :ensure t)
(use-package highlight-operators :ensure t
  :config)
(use-package hl-todo :ensure t
	:config
	(global-hl-todo-mode)) ; TODO 
(use-package emmet-mode :ensure t
	:config
	(add-hook 'sgml-mode-hook 'emmet-mode)
	(add-hook 'web-mode-hook 'emmet-mode)
	(add-hook 'css-mode-hook  'emmet-mode)
	(setq emmet-move-cursor-after-expanding t)
	(setq emmet-move-cursor-between-quotes t)
	(setq emmet-indentation 2)
	:bind (:map emmet-mode-keymap
		("C-j" . nil)))
(use-package imenu-list :ensure t :after neotree
	:config
	(setq imenu-list-position 'right)
	(setq imenu-list-size 30)
	(setq imenu-list-idle-update-delay 1)
	(setq imenu-list-buffer-name "*Outline*")
	(add-hook 'imenu-list-major-mode-hook (lambda ()
    (make-local-variable 'face-remapping-alist)
    (add-to-list 'face-remapping-alist '(default sidebar-background))
    (yascroll-bar-mode -1)
    (setq-local left-fringe-width 0)
    (setq-local right-fringe-width 0)
		(hl-line-mode 1)
		(setq tab-width 2)
		(whitespace-mode 1)
		(setq-local use-line-nav t)))
	(add-hook 'after-init-hook (lambda ()
		(setq imenu-list-mode-line-format mode-line-format)
		(imenu-list-get-buffer-create)
		(imenu-list-start-timer)
		(imenu-list-update nil t)
		(neotree-show)
		(display-buffer-in-side-window (get-buffer imenu-list-buffer-name) '((side . left)))))
	; patch to change appearance
	(defun imenu-list--depth-string (depth)
		"Return a prefix string representing an entry's DEPTH."
		(let ((indents (cl-loop for i from 1 to depth collect "\t")))
			(mapconcat #'identity indents "")))
	(defun imenu-list--insert-entry (entry depth)
  "Insert a line for ENTRY with DEPTH."
  (if (imenu--subalist-p entry)
      (progn
        (insert (imenu-list--depth-string depth))
        (insert-button (format "+ %s" (car entry))
                       'face (imenu-list--get-face depth t)
                       'help-echo (format "Toggle: %s"
                                          (car entry))
                       'follow-link t
                       'action ;; #'imenu-list--action-goto-entry
                       #'imenu-list--action-toggle-hs
                       )
        (insert "\n"))
    (insert (imenu-list--depth-string depth))
    (insert-button (format "● %s" (car entry))
                   'face (imenu-list--get-face depth nil)
                   'help-echo (format "Go to: %s"
                                      (car entry))
                   'follow-link t
                   'action #'imenu-list--action-goto-entry)
    (insert "\n")))
)
;; (use-package window-purpose :ensure t :after neotree imenu-list
;; 	:config
;; 	(purpose-mode)
;; )
(require 'companion)
(require 'smart-completer)

; language specific

(require 'shaderlab-mode)
(use-package auctex :defer t :ensure t)
(use-package kivy-mode :ensure t)
(use-package cc-mode :ensure t
  :config
  (setq c-default-style
        '((java-mode . "java")
          (awk-mode . "awk")
          (other . "linux"))))
(use-package ess :ensure t
  :config
  (add-hook 'ess-r-mode-hook
            (lambda ()
              (setq-local company-backends
                    (let ((b #'company-tabnine))
                      (cons b (remove b company-backends)))))))
(use-package csharp-mode :ensure t)
(use-package markdown-mode :ensure t)
(use-package markdown-mode+ :ensure t)
(use-package racket-mode :ensure t)
(use-package haskell-mode :ensure t)
(use-package haskell-snippets :ensure t)
(use-package rust-mode :ensure t)
(use-package csv-mode :ensure t)
(use-package tide :ensure t
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (setq-local flycheck-check-syntax-automatically '(save mode-enabled))
    (tide-hl-identifier-mode +1)
    (setq-local company-backends
                (let ((b #'company-tide))
                  (cons b (remove b company-backends))))
    (setq-local company-backends
                (let ((b #'company-tabnine))
                  (cons b (remove b company-backends)))))
  ;; formats the buffer before saving
  ;; TODO: we don't want this for now
  ;; (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'setup-tide-mode))
(use-package glsl-mode :ensure t
  :config
  ;; (add-to-list 'auto-mode-alist '("\\.vs\\'" . glsl-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.fs\\'" . glsl-mode))
)
(use-package json-mode :ensure t
  :config
  (setq json-reformat:indent-width 2))
;; (use-package vue-mode :ensure t
;;	   :config
;;	   (setq mmm-submode-decoration-level 0)
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
	(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
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
(use-package counsel-css :ensure t
	:config
	(add-hook 'css-mode-hook 'counsel-css-imenu-setup)
)
;; (use-package lsp-python :ensure t :after lsp-mode
;; 	:config
;; 	(add-hook 'python-mode-hook #'lsp-python-enable)
;; )
;; (use-package lsp-javascript-typescript :ensure t :after lsp-mode
;; 	:config
;; 	(add-hook 'js-mode-hook #'lsp-javascript-typescript-enable)
;; 	(add-hook 'typescript-mode-hook #'lsp-javascript-typescript-enable) ;; for typescript support
;; 	(add-hook 'js2-mode-hook #'lsp-javascript-typescript-enable) ;; for js2-mode support
;; 	(add-hook 'js3-mode-hook #'lsp-javascript-typescript-enable) ;; for js3-mode support
;; 	(add-hook 'rjsx-mode #'lsp-javascript-typescript-enable) ;; for rjsx-mode support
;; )
;; (use-package js2-refactor :ensure t)

; fun

(use-package landmark :ensure t)


;;; package settings

;; status lines
(load-relative "./tommyx-status-lines.el")

;; dashboard
(dashboard-setup-startup-hook)
(add-hook 'dashboard-mode-hook (lambda () (hl-line-mode 1) (setq-local use-line-nav t)))

;; yascroll
(global-yascroll-bar-mode 1)
(setq yascroll:last-state nil)
(setq yascroll:delay-to-hide nil)
(setq yascroll:scroll-bar '(right-fringe left-fringe text-area))
; disable in insert mode
(add-hook 'evil-insert-state-entry-hook
          (lambda ()
            (setq yascroll:last-state yascroll-bar-mode)
            (yascroll-bar-mode -1)))
(add-hook 'evil-insert-state-exit-hook
          (lambda () (yascroll-bar-mode (if yascroll:last-state 1 -1))))
; auto run on idle timer
(run-with-idle-timer
 0.5
 t
 (lambda ()
   (when (and (not (eq evil-state 'insert))
              yascroll-bar-mode)
     (yascroll:safe-show-scroll-bar))))

;; beacon
(setq beacon-blink-when-focused nil) ; may cause problem
(setq beacon-blink-when-buffer-changes t)
(setq beacon-blink-when-window-changes t)
(setq beacon-blink-when-window-scrolls t)
(setq beacon-blink-duration 0.15)
(setq beacon-blink-delay 0.15)
(setq beacon-size 15)
(setq beacon-color "#2499ff")
;; (beacon-mode 1)
; disable in insert mode
;; (add-hook 'evil-insert-state-entry-hook (lambda () (beacon-mode -1)))
;; (add-hook 'evil-insert-state-exit-hook (lambda () (beacon-mode 1)))

;; which-function
(which-function-mode 1)
(add-hook 'prog-mode-hook #'which-function-mode)
(add-hook 'prog-mode-hook (lambda () (interactive)
	))

;; dashboard
(setq dashboard-items '((recents  . 5)
			(projects . 5)
						(bookmarks . 5)))
; custom logo and message
(setq dashboard-banner-length 250)
(setq dashboard-banner-logo-title
	(concat "Emacs " emacs-version " (" system-configuration ")"))
(setq dashboard-startup-banner (expand-file-name "logo.png"
	(file-name-directory load-file-name)))

;; yasnippet
(add-hook 'after-init-hook 'yas-global-mode)
(add-to-list 'yas-snippet-dirs (expand-file-name "snippets"
	(file-name-directory load-file-name)))
(setq company-continue-commands (-snoc company-continue-commands 'yas-insert-snippet)) ; make company break completion

;; company
(add-hook 'after-init-hook 'global-company-mode)
(company-tng-configure-default)
;; (company-quickhelp-mode)
(setq my-company--company-command-p-override nil)
(defun my-company--company-command-p (func &rest args)
	"Patch company-mode to treat key sequences like \"jp\" not a company-mode command.

Since company-tng-frontend only complete selection when pressing any key that isn't
a company-mode command (checked with this function), and we want general-key-dispatch
to have \"j\" as a company-mode command (so do not complete) but not to have
\"jp\" as one (so do completion)."
	(if my-company--company-command-p-override
		nil ; treat all command as breaking company completion
		(let ((return (apply func args)))

			;; (message
			;; 	(concat "debug: "
			;; 					(prin1-to-string company-selection-changed) " "
			;; 					(prin1-to-string return) " "
			;; 					(prin1-to-string (and return (not (numberp return)))) " "
			;; 					(prin1-to-string args)))

			(and return (not (numberp return))))))
(advice-add #'company--company-command-p :around #'my-company--company-command-p)
;; make evil-normal-state abort completion. note that this works only if 'not is the
;; first element in company-continue-commands.
(setq company-continue-commands (-snoc company-continue-commands 'evil-normal-state))

(eval-after-load 'company
  '(progn
		 
		(global-set-key (kbd "M-1") (lambda (interactive) (company-complete-number 1)))
		(global-set-key (kbd "M-2") (lambda (interactive) (company-complete-number 2)))
		(global-set-key (kbd "M-3") (lambda (interactive) (company-complete-number 3)))
		(global-set-key (kbd "M-4") (lambda (interactive) (company-complete-number 4)))
		(global-set-key (kbd "M-5") (lambda (interactive) (company-complete-number 5)))
		(global-set-key (kbd "M-6") (lambda (interactive) (company-complete-number 6)))
		(global-set-key (kbd "M-7") (lambda (interactive) (company-complete-number 7)))
		(global-set-key (kbd "M-8") (lambda (interactive) (company-complete-number 8)))
		(global-set-key (kbd "M-9") (lambda (interactive) (company-complete-number 9)))
		(global-set-key (kbd "M-0") (lambda (interactive) (company-complete-number 0)))
		(define-key company-active-map (kbd "C-h") nil)
		(define-key company-active-map (kbd "C-z") 'company-show-doc-buffer)
			; C-z when company open will show help for that symbol in another window.
		(define-key company-active-map (kbd "S-TAB") 'company-select-previous)
		(define-key company-active-map (kbd "<S-tab>") 'company-select-previous)))
(setq company-frontends
	  '(company-tng-frontend
		company-pseudo-tooltip-frontend
		;; company-preview-frontend
		company-echo-metadata-frontend))
(setq company-idle-delay 0)
(setq company-tooltip-align-annotations t)
;; (setq company-idle-delay 0.2)
(setq company-selection-wrap-around t)
(setq company-show-numbers t)
(setq company-quickhelp-delay nil) ; we will manually trigger the help
(setq company-require-match 'never)
(setq company-dabbrev-downcase nil)
(setq company-dabbrev-ignore-case nil)
(setq company-dabbrev-other-buffers t)
;; (with-eval-after-load 'company
;;   (setq-default company-transformers '())
;;   ; fix bug with custom completer
;;   (company-flx-mode 1)
;;   (company-flx-mode -1)
;;   ;; (delete #'company-flx-transformer company-transformers)
;;   (advice-add 'company-capf :around #'company-flx-company-capf-advice)
;;   (add-hook 'emacs-lisp-mode-hook (lambda () (setq-local company-transformers '(company-flx-transformer))))
;;   (add-hook 'css-mode-hook (lambda () (setq-local company-transformers '(company-flx-transformer)))))
(with-eval-after-load 'company
  (company-flx-mode 1))
(setq company-flx-limit 256)

;; ycmd
(setq ycmd-global-config (expand-file-name "third_party/ycmd/.ycm_extra_conf.py"
	(file-name-directory load-file-name)))
(unless (boundp 'ycmd-server-python-command)
  (setq ycmd-server-python-command "python"))
(setq ycmd-server-command `(,ycmd-server-python-command "-u" ,(expand-file-name "third_party/ycmd/ycmd/"
	(file-name-directory load-file-name))))
; TODO disabled
;; (add-hook 'ycmd-mode-hook 'company-ycmd-setup) ; already manually added
(add-hook 'ycmd-mode-hook 'flycheck-ycmd-setup)
(add-hook 'ycmd-mode-hook (lambda () (interactive) (when (ycmd-major-mode-to-file-types major-mode) (ycmd-eldoc-setup))))
; attempt to improve performance
(setq company-ycmd-request-sync-timeout 0)
; generic file types
(add-hook 'prog-mode-hook (lambda () (ycmd-mode 1)))
(add-hook 'text-mode-hook (lambda () (ycmd-mode 1)))
; TODO: bug: remove eldoc mode from certain other modes. (ycmd freezes)
(add-hook 'text-mode-hook (lambda () (ycmd-eldoc-mode -1)))
(add-hook 'org-mode-hook (lambda () (ycmd-eldoc-mode -1)))
(add-hook 'racket-mode-hook (lambda () (ycmd-eldoc-mode -1)))
(add-hook 'haskell-mode-hook (lambda () (ycmd-eldoc-mode -1)))
(add-hook 'typescript-mode-hook (lambda () (ycmd-mode -1)))
; c/c++
(evil-define-key 'normal c-mode-map (kbd "C-]") 'ycmd-goto) ; goto
(evil-define-key 'normal c++-mode-map (kbd "C-]") 'ycmd-goto) ; goto
; c#
(evil-define-key 'normal csharp-mode-map (kbd "C-]") 'ycmd-goto) ; goto
; java
(add-to-list 'ycmd-file-type-map '(java-mode "java")) ; file type detection
(evil-define-key 'normal java-mode-map (kbd "C-]") 'ycmd-goto) ; goto
; python
(evil-define-key 'normal python-mode-map (kbd "C-]") 'ycmd-goto) ; goto
; elisp
(add-hook 'emacs-lisp-mode-hook (lambda () (ycmd-mode -1))) ; disable ycm
(add-hook 'java-mode-hook (lambda () (ycmd-mode -1))) ; TODO bug
; css
(add-hook 'css-mode-hook (lambda () (ycmd-mode -1)))

;; smartparens
(require 'smartparens-config)
(smartparens-global-mode 1)
(show-smartparens-global-mode 1)
(setq sp-show-pair-from-inside t) ; TODO can set to false if overlay problem concerns us
(setq smartparens-strict-mode nil)
(setq sp-cancel-autoskip-on-backward-movement nil)
;; (setq show-paren-style 'expression)
;; (show-paren-mode 1)
; auto expanison of brackets
(sp-local-pair 'prog-mode "{" nil :post-handlers '(("||\n[i]" "RET") ("||\n[i]" "<return>") ("| " "SPC") ("| " "<space>")))
(sp-local-pair 'text-mode "{" nil :post-handlers '(("||\n[i]" "RET") ("||\n[i]" "<return>") ("| " "SPC") ("| " "<space>")))
(sp-local-pair 'prog-mode "[" nil :post-handlers '(("||\n[i]" "RET") ("||\n[i]" "<return>") ("| " "SPC") ("| " "<space>")))
(sp-local-pair 'text-mode "[" nil :post-handlers '(("||\n[i]" "RET") ("||\n[i]" "<return>") ("| " "SPC") ("| " "<space>")))
(sp-local-pair 'prog-mode "(" nil :post-handlers '(("||\n[i]" "RET") ("||\n[i]" "<return>") ("| " "SPC") ("| " "<space>")))
(sp-local-pair 'text-mode "(" nil :post-handlers '(("||\n[i]" "RET") ("||\n[i]" "<return>") ("| " "SPC") ("| " "<space>")))
;; (defun remove-parens-overlay (&rest _) (sp-remove-active-pair-overlay))
;; (advice-add 'evil-normal-state :after #'remove-parens-overlay)

;; evil
(evil-mode 1) ; use evil-mode at startup
(global-undo-tree-mode -1) ; do not use unto tree due to bugs
; split to the right and below
(setq evil-split-window-below t)
(setq evil-vsplit-window-right t)
(setq evil-ex-substitute-global t)
(setq evil-move-cursor-back nil)
(setq evil-move-beyond-eol t)
(setq-default evil-symbol-word-search t)
; auto center after search
(defun my-center-line (&rest _) (evil-scroll-line-to-center nil))
(defun flash-cursor (&rest _)
  (hl-line-highlight)
  (let ((ov (make-overlay (point) (- (point) 1))))
       (overlay-put ov 'priority 9999)
	     (overlay-put ov 'window (selected-window))
	     (overlay-put ov 'face 'cursor)
       (sit-for 1)
       (delete-overlay ov)))
;; (advice-add 'evil-ex-search-next :after #'flash-cursor)
;; (advice-add 'evil-ex-search-previous :after #'flash-cursor)
(advice-add 'evil-ex-search-word-forward :after #'evil-ex-search-previous)
(advice-add 'evil-ex-search-unbounded-word-forward :after #'evil-ex-search-previous)
(advice-add 'evil-ex-search-word-backward :after #'evil-ex-search-next)
(defun my-search-previous (&rest _) (evil-ex-search-previous))
(defun my-search-next (&rest _) (evil-ex-search-next))
(advice-add 'evil-visualstar/begin-search-forward :after #'my-search-previous)
(advice-add 'evil-visualstar/begin-search-backward :after #'my-search-next)
; no magic for search
(setq evil-magic nil) ; doesn't work
; search highlight persist
(global-evil-search-highlight-persist) ; doesn't work
(setq evil-search-highlight-persist-all-windows t)
; no echoing
(setq evil-insert-state-message nil)
(setq evil-visual-state-message nil)
(setq evil-replace-state-message nil)
; custom cursor
;; (setq evil-insert-state-cursor '((bar . 4)))
;; moved to theme definition
; push jump list every time entering insert mode
(add-hook 'evil-insert-state-entry-hook 'evil-set-jump)
; do not remove space when leaving insert mode
(setq evil-allow-remove-spaces t) ;; TODO still allow it.
(defun my-evil-maybe-remove-spaces (func &rest args)
  (if evil-allow-remove-spaces
      (apply func args)
    (setq evil-maybe-remove-spaces nil)
    (apply func args)))
(advice-add #'evil-maybe-remove-spaces :around #'my-evil-maybe-remove-spaces)

;; highlight numbers
(add-hook 'prog-mode-hook #'highlight-numbers-mode)
(add-hook 'json-mode-hook (lambda ()
                            (highlight-numbers-mode -1)))
;; (add-hook 'text-mode-hook #'highlight-numbers-mode)

;; evil-surround
(global-evil-surround-mode 1)

;; avy
(setq avy-keys '(?w ?e ?r ?u ?i ?o ?p ?a ?s ?d ?g ?h ?j ?k ?l ?v ?n))
(setq avy-all-windows nil)
(setq avy-goto-word-0-regexp "\\(\\<\\sw\\|\n\\)")

;; undo-tree
; attempt to fix bug
(setq undo-tree-enable-undo-in-region nil)
; persistent undo
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/history")))

;; volatile-highlight
;; (vhl/define-extension 'evil
;; 						 'evil-normal-state)
;; (with-eval-after-load 'evil
;; 	   (vhl/install-extension 'evil)
;; 	   (vhl/load-extension 'evil)) 
(vhl/define-extension 'undo-tree
					  'undo-tree-move
					  'undo-tree-yank)
(with-eval-after-load 'undo-tree
	(vhl/install-extension 'undo-tree)
	(vhl/load-extension 'undo-tree))
(volatile-highlights-mode 1)

;; evil-goggles
(setq evil-goggles-pulse nil)
(setq evil-goggles-duration 1)
(setq evil-goggles-async-duration 1)
(setq evil-goggles-blocking-duration 1)
(setq evil-goggles--commands
	    '((evil-yank
         :face evil-goggles-yank-face
         :switch evil-goggles-enable-yank
         :advice evil-goggles--generic-async-advice)
        (evil-join
         :face evil-goggles-join-face
         :switch evil-goggles-enable-join
         :advice evil-goggles--join-advice)
        (evil-join-whitespace
         :face evil-goggles-join-face
         :switch evil-goggles-enable-join
         :advice evil-goggles--join-advice)
        (evil-fill-and-move
         :face evil-goggles-fill-and-move-face
         :switch evil-goggles-enable-fill-and-move
         :advice evil-goggles--generic-async-advice)
        (evil-shift-left
         :face evil-goggles-shift-face
         :switch evil-goggles-enable-shift
         :advice evil-goggles--generic-async-advice)
        (evil-shift-right
         :face evil-goggles-shift-face
         :switch evil-goggles-enable-shift
         :advice evil-goggles--generic-async-advice)
        (evil-org-<
         :face evil-goggles-shift-face
         :switch evil-goggles-enable-shift
         :advice evil-goggles--generic-async-advice)
        (evil-org->
         :face evil-goggles-shift-face
         :switch evil-goggles-enable-shift
         :advice evil-goggles--generic-async-advice)
        (evil-surround-region
         :face evil-goggles-surround-face
         :switch evil-goggles-enable-surround
         :advice evil-goggles--generic-async-advice)
        (evil-commentary
         :face evil-goggles-commentary-face
         :switch evil-goggles-enable-commentary
         :advice evil-goggles--generic-async-advice)
        (evilnc-comment-operator
         :face evil-goggles-nerd-commenter-face
         :switch evil-goggles-enable-nerd-commenter
         :advice evil-goggles--generic-async-advice)
        (evil-replace-with-register
         :face evil-goggles-replace-with-register-face
         :switch evil-goggles-enable-replace-with-register
         :advice evil-goggles--generic-async-advice-1)
        (evil-set-marker
         :face evil-goggles-set-marker-face
         :switch evil-goggles-enable-set-marker
         :advice evil-goggles--set-marker-advice)
        (evil-record-macro
         :face evil-goggles-record-macro-face
         :switch evil-goggles-enable-record-macro
         :advice evil-goggles--record-macro-advice)
        (evil-paste-before
         :face evil-goggles-paste-face
         :switch evil-goggles-enable-paste
         :advice evil-goggles--paste-advice :after t)
        (evil-paste-after
         :face evil-goggles-paste-face
         :switch evil-goggles-enable-paste
         :advice evil-goggles--paste-advice :after t)))
(evil-goggles-mode 1)

;; evil-collection
(delete 'neotree evil-collection-mode-list)
(delete 'company evil-collection-mode-list)
(evil-collection-init)

;; flyspell lazy
(flyspell-lazy-mode 1)
(setq flyspell-lazy-idle-seconds 2.5)
(setq flyspell-lazy-window-idle-seconds 5)

;; highlight indent guides (currently disabled)

;; visual indentation mode
;; (add-hook 'prog-mode-hook 'visual-indentation-mode)

;; evil-visualstar
(global-evil-visualstar-mode 1)

;; general
(general-evil-setup)
(general-auto-unbind-keys)

;; flycheck
(global-flycheck-mode 1)
; BUG: temporarily disable for some modes
(add-hook 'haskell-mode-hook (lambda () (flycheck-mode -1)))

;; helm
(require 'helm-config)
;; (setq helm-display-function 'helm-display-buffer-in-own-frame
;;		   helm-display-buffer-reuse-frame t
;;		   helm-use-undecorated-frame-option t)
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

;; origami
(add-to-list 'origami-parser-alist
	'(python-mode . origami-indent-parser))
;; (global-origami-mode 1)

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
		(t		. ivy--regex-fuzzy)))
; enable wrapping
(setq ivy-wrap t)
(setq ivy-action-wrap t)
; add recent files and bookmarks to ivy-switch-buffer
(setq ivy-use-virtual-buffers t)
; display functions
(setq ivy-posframe-parameters '(
	(width . 50)
	(border-width . 1)
	(internal-border-width . 1)
  (undecorated . t)
	(min-width . 50)
	(refresh . 1)
	))
; misc
(setq ivy-height 12)
(setq ivy-height-alist nil) ; all ivy should have same height
(setq ivy-posframe-height (truncate (* ivy-height 1.1)))
(setq ivy-posframe-border-width 1)
(defun ivy-posframe--display (str &optional poshandler full-width) ; override
  "Show STR in ivy's posframe."
  (if (not (posframe-workable-p))
	    (ivy-display-function-fallback str)
    (setq ivy-posframe--display-p t)
    (with-ivy-window
	    (posframe-show
	     ivy-posframe-buffer
	     :font ivy-posframe-font
	     :string
       (with-current-buffer (window-buffer (active-minibuffer-window))
         (let ((point (point))
               (string (if ivy-posframe--ignore-prompt
                           str
                         (concat (buffer-string) "  " str))))
           (add-text-properties (- point 1) point '(face ivy-posframe-cursor) string)
           string))
	     :position (point)
	     :poshandler poshandler
       :background-color (face-attribute 'ivy-posframe :background nil t)
       :foreground-color (face-attribute 'ivy-posframe :foreground nil t)
	     ;; :height (truncate (* 1.1 ivy-height))
	     ;; :width (window-width) ; (if full-width (window-width) nil)
	     ;; :min-height 10
	     ;; :min-width 50
       :height ivy-posframe-height
       :width (window-width)
       :min-height (or ivy-posframe-min-height (+ ivy-height 1))
       :min-width (or ivy-posframe-min-width (round (* (frame-width) 0.62)))
       :internal-border-width ivy-posframe-border-width
	     :override-parameters ivy-posframe-parameters))))
(defun posframe-poshandler-adaptive-top-bottom (info)
  "Posframe's position handler.

Get a position which let posframe stay onto current window's
top or bottom side without blocking center content.
Useful for a search overview popup."
  (let* (
				(posframe (plist-get info :posframe))
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
						(+ window-top window-height)
					)
				)
				(cons ; top
					window-left
					(max 0
						(- window-top posframe-height)
					)
				)
			)
		)
		;; (t
		;; 	(cons ; window bottom
		;; 		window-left
		;; 		(+ window-top window-height (- 0 modeline-height posframe-height)))
		;; )
		)))
(defun ivy-posframe-display-swiper (str)
  (ivy-posframe--display str #'posframe-poshandler-adaptive-top-bottom t))
(setq ivy-display-function nil)
(setq ivy-display-functions-alist
	  '((swiper . ivy-posframe-display-swiper)
		(swiper-multi . ivy-posframe-display-swiper)
		(counsel-ag . nil)
		(counsel-rg . nil)
		(counsel-grep . nil)
		(t . ivy-posframe-display-at-point)))
(ivy-posframe-enable)
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
(defun ivy-format-function-custom (cands)
  "Transform CANDS into a string for minibuffer."
  (ivy--format-function-generic
   (lambda (str)
     (concat "> " (ivy--add-face (concat str "\n") 'ivy-current-match)))
   (lambda (str)
     (concat "  " str "\n"))
   cands
   ""))
(setq ivy-format-function 'ivy-format-function-custom)
;; (setq ivy-format-function 'ivy-format-function-default)
(setq ivy-count-format "%d/%d | ")


;;; heavy tasks
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


;;; key bindings util / helper functions and motion
;; (defun fit-window-to-region ()
;; 	(interactive)
;; 	TODO)
(defun emms-echo-no-error (&optional insertp)
  "Describe the current EMMS track in the minibuffer.
If INSERTP is non-nil, insert the description into the current buffer instead.
This function uses `emms-show-format' to format the current track."
  (interactive "P")
  (condition-case nil
      (progn
        (let ((string (format emms-show-format
                              (emms-track-description
                               (emms-playlist-current-selected-track)))))
          (message "%s" string))
        t)
    (error nil)))
(defun emms-next-and-echo ()
  (interactive)
  (emms-next)
  (emms-echo-no-error)
  (sit-for 2))
(defun emms-previous-and-echo ()
  (interactive)
  (emms-previous)
  (emms-echo-no-error)
  (sit-for 2))
(defun emms-seek-backward-more ()
  (interactive)
  (emms-seek -30))
(defun emms-seek-forward-more ()
  (interactive)
  (emms-seek 30))
(defun emms-restart ()
  (interactive)
  (emms-seek-to 0))
(defun counsel-emms-get-playlist-items ()
  (let (items)
    (with-current-emms-playlist
      (save-excursion
        (beginning-of-buffer)
        (while (< (point) (point-max))
          (let ((pos (point))
                (name (file-name-base
                       (buffer-substring-no-properties (point)
                                                       (line-end-position)))))
            (push (propertize name 'property pos) items))
          (forward-line)))
      ;; (split-string (buffer-string) "[\n\r]+" t)
      )
    items))
(defun counsel-emms-play-item (item)
  (let ((pos (get-text-property 0 'property item)))
    (with-current-emms-playlist
      (save-excursion
        (goto-char pos)
        (emms-playlist-mode-play-current-track)))))
(defun counsel-emms-play ()
  (interactive)
  (ivy-read "Play track: "
            (counsel-emms-get-playlist-items)
            :action #'counsel-emms-play-item
            :require-match t))
(defun emms-show-progress (&rest _)
  (let* ((total-playing-time (emms-track-get
                              (emms-playlist-current-selected-track)
                              'info-playing-time))
         (playing-time emms-playing-time)
         (elapsed/total (/ (* 100 emms-playing-time) total-playing-time)))
    (with-temp-message (format "[%-100s] [%02d:%02d/%02d:%02d] %2d%%"
                               (make-string elapsed/total ?=)
                               (/ playing-time 60)
                               (% playing-time 60)
                               (/ total-playing-time 60)
                               (% total-playing-time 60)
                               elapsed/total)
      (sit-for 2))))
(add-hook 'emms-player-seeked-functions #'emms-show-progress 'append)

(defun company-smart-complete ()
  (interactive)
  (cond
   (company-selection-changed
    (company-complete-selection))
   (company-candidates
    (company-select-next))
   (t
    (company-auto-begin)
    (company-select-next))))
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
  (interactive) (company-complete-number 0))
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
  (flash-cursor))
(evil-define-command evil-ex-search-previous-flash () :repeat nil
  (evil-ex-search-previous)
  (flash-cursor))
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
(defun selection-or-word-at-point (&optional no-symbol)
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
   (t
    (save-excursion
      (if no-symbol
          (word-at-point)
        (when (not (looking-at "\\sw"))
          (while (and (> (point) (point-min)) (= (char-before) ? ))
            (backward-char)))
        (format "\\<%s\\>"
			          (or (word-at-point)
				            "")))))))
(evil-define-motion swiper-movement () :type exclusive
	(swiper))
(evil-define-command evil-noh-blink () :repeat nil (interactive)
	(evil-ex-nohighlight) (beacon-blink))
(evil-define-command evil-comfortable-recenter () :repeat nil
  (recenter-top-bottom (/ (* (window-total-height) 2) 7)))
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
		(setq kill-ring-yank-pointer kill-ring))
)
(defun call-with-command-hooks (command &optional enforce-keys)
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
command (ran after) is mysteriously incorrect."
	(let ((old-command this-command))
		(setq this-command command)
		(run-hooks 'pre-command-hook)
		(call-interactively this-command)
		(when (and (eq command this-command) enforce-keys)
			(set--this-command-keys enforce-keys))
		(run-hooks 'post-command-hook)
		(setq this-command old-command)
	))

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

;;; key bindings

;; profiler
(general-define-key
  :keymaps 'override

	"C-M-p" (lambda () (interactive) (profiler-start 'cpu+mem))
	"C-M-S-p" (lambda () (interactive)
		(profiler-report) (profiler-stop) (profiler-reset))
)

;; use esc (same as "C-[") for escape
(global-set-key (kbd "C-[") 'keyboard-escape-quit)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; hydra
(defhydra hydra-emms-control ()
  "EMMS control"
  ("r" emms-random "random track")
  ("k" emms-previous-and-echo "previous track")
  ("j" emms-next-and-echo "next track")
  ("G" emms-restart "restart")
  (":" emms-next-and-echo "next track")
  ("H" emms-seek-backward-more "seek left more")
  ("L" emms-seek-forward-more "seek right more")
  ("h" emms-seek-backward "seek left")
  ("l" emms-seek-forward "seek right")
  ("p" emms-pause "pause/play"))
(defhydra hydra-zoom ()
  "zoom"
  ("=" text-scale-increase "in")
  ("-" text-scale-decrease "out"))
(defhydra hydra-move ()
  "move"
  ("a" evil-backward-char)
  ("s" evil-backward-char)
  ("d" evil-backward-char)
  ("f" evil-backward-char)
  ("h" evil-forward-char)
  ("j" evil-forward-char)
  ("k" evil-forward-char)
  ("l" evil-forward-char)
  ("q" evil-previous-visual-line)
  ("w" evil-previous-visual-line)
  ("e" evil-previous-visual-line)
  ("r" evil-previous-visual-line)
  ("t" evil-previous-visual-line)
  ("y" evil-previous-visual-line)
  ("u" evil-previous-visual-line)
  ("i" evil-previous-visual-line)
  ("o" evil-previous-visual-line)
  ("p" evil-previous-visual-line)
  ("[" evil-previous-visual-line)
  ("]" evil-previous-visual-line)
  ("z" evil-next-visual-line)
  ("x" evil-next-visual-line)
  ("c" evil-next-visual-line)
  ("v" evil-next-visual-line)
  ("b" evil-next-visual-line)
  ("n" evil-next-visual-line)
  ("m" evil-next-visual-line)
  ("," evil-next-visual-line)
  ("." evil-next-visual-line)
  ("/" evil-next-visual-line)
)
(evil-define-key 'normal 'global "t" 'hydra-move/body)

;; global leader
;; (define-prefix-command 'global-leader)
(define-prefix-command 'global-leader-window)
(define-prefix-command 'global-leader-helm)
(define-prefix-command 'global-leader-navigation)
(define-prefix-command 'global-leader-project)
(define-prefix-command 'global-leader-org)
(define-prefix-command 'global-leader-appearance)
(define-prefix-command 'global-leader-appearance-theme)
(define-prefix-command 'global-leader-appearance-font)
(define-prefix-command 'global-leader-mode-specific)
(define-prefix-command 'global-leader-companion)
(define-prefix-command 'global-leader-sidebar)
(define-prefix-command 'global-leader-templates)
(define-prefix-command 'global-leader-files)
(define-prefix-command 'global-leader-battery)
(define-prefix-command 'global-leader-music)
(define-prefix-command 'global-leader-music-playlist)
(define-prefix-command 'global-leader-version-control)
; prefix keys
(general-define-key
  :keymaps 'override
  :states '(motion normal visual)
  
  "SPC" 'global-leader)

(general-define-key
  :keymaps 'override
	:states '(motion normal visual)
  :prefix "SPC"

	"w" '(global-leader-window
		:which-key "window")
	"h" '(global-leader-helm
		:which-key "helm")
	"i" '(global-leader-navigation
		:which-key "navigation")
	"o" '(global-leader-org
		:which-key "org")
	"j" '(global-leader-mode-specific
		:which-key "mode specific")
	"a" '(global-leader-appearance
		:which-key "appearance")
	"at" '(global-leader-appearance-theme
		:which-key "theme")
	"af" '(global-leader-appearance-font
		:which-key "font")
	"p" '(global-leader-project
		:which-key "project + workspace")
	"c" '(global-leader-companion
		:which-key "companion")
	"s" '(global-leader-sidebar
		:which-key "side bar")
	"t" '(global-leader-templates
		:which-key "templates")
	"e" '(global-leader-files
		:which-key "files")
  "b" '(global-leader-battery
    :which-key "battery")
  "m" '(global-leader-music
    :which-key "music")
  "v" '(global-leader-version-control
    :which-key "version control")
)
(general-define-key
  :keymaps 'override
	:states '(motion normal visual)
	:prefix "SPC"
	:non-normal-prefix "M-SPC"

	"is" '(counsel-semantic-or-imenu
		:which-key "semantic item")

	"iS" '((lambda () (interactive) (call-interactively 'imenu))
		:which-key "semantic item tree")

	"x" '(counsel-M-x
		:which-key "counsel M-x")

	"g" '(google-this-search
		:which-key "Google")

	";" '(eval-expression
		:which-key "eval lisp")

	"hx" '(helm-M-x
		:which-key "helm M-x")
	"ho" '(helm-occur
		:which-key "helm occur")
	"hs" '(helm-swoop
		:which-key "helm swoop")

	"atl" '((lambda () (interactive) (change-theme light-theme))
		:which-key "light theme")
	"atd" '((lambda () (interactive) (change-theme dark-theme))
		:which-key "dark theme")
  "aF" '((lambda () (interactive) (toggle-frame-fullscreen))
    :which-key "toggle full-screen")
	
	"afs" '(set-to-small-font
		:which-key "small font")
	"afb" '(set-to-big-font
		:which-key "big font")
	"afz" '(hydra-zoom/body
		:which-key "zoom buffer font")
	"afr" '(toggle-readable-buffer-font
		:which-key "toggle readable buffer font")

	"cd" '(companion-notif-dismiss
		:which-key "dismiss notification")
	"cq" '(companion-show-last-qod
		:which-key "quote of the day")
	"cy" '(companion-copy-qod
		:which-key "copy quote of the day")
	"cQ" '(companion-fetch-qod
		:which-key "fetch quote of the day")

	"sf" '((lambda () (interactive)
		(display-buffer-in-side-window (get-buffer neo-buffer-name) '((side . left))))
		:which-key "files")

	"so" '((lambda () (interactive)
		(display-buffer-in-side-window (get-buffer imenu-list-buffer-name) '((side . left))))
		:which-key "outline")

	"tn" '(yas-new-snippet
		:which-key "new")
	"te" '(yas-visit-snippet-file
		:which-key "edit")
	"tr" '(yas-reload-all
		:which-key "reload")
  "ti" '(insert-file
    :which-key "insert file content")

	"ew" '(write-file
		:which-key "write file")
	"er" '(rename-buffer
		:which-key "rename buffer")
	"ea" '(evil-write-all
		:which-key "write all files")

  "bc" '((lambda () (interactive) (setq company-idle-delay 0))
    :which-key "instant completion")
  "bC" '((lambda () (interactive) (setq company-idle-delay 0.2))
    :which-key "delayed completion")

  "ml" '(global-leader-music-playlist
    :which-key "playlist")
  "mll" '(emms-add-playlist
    :which-key "add playlist")
  "mla" '(emms-add-file
    :which-key "add file")
  "mld" '(emms-add-directory
    :which-key "add directory")
  "mlD" '(emms-add-directory-tree
    :which-key "add directory recursively")
  "mls" '(emms-playlist-save
    :which-key "save playlist")
  "mlg" '((lambda () (interactive)
            (emms-playlist-mode-go)
            (emms-playlist-mode-center-current))
    :which-key "go to playlist")
  "mls" '(emms-sort
    :which-key "sort playlist")
  "mlS" '(emms-shuffle
    :which-key "shuffle playlist")
  "mlu" '(emms-uniq
    :which-key "remove playlist duplicates")
  "mlc" '(emms-playlist-clear
    :which-key "clear playlist")
  "mf" '(counsel-emms-play
    :which-key "play music")
  "mn" '(hydra-emms-control/body
    :which-key "music control")
  "mp" '(emms-pause
    :which-key "pause music")
  "mP" '(emms-stop
    :which-key "stop music")
  "mr" '(emms-random
    :which-key "play random music")
  "mt" '(emms-toggle-repeat-track
    :which-key "toggle repeat track")

  "vs" '(magit-status
    :which-key "magit status")
)
(general-define-key
  :keymaps 'override
	:states '(visual)
	:prefix "SPC"
	:non-normal-prefix "M-SPC"

	"f" '(swiper-movement
		:which-key "search")

	"F" '((lambda () (interactive) (swiper (selection-or-word-at-point)))
		:which-key "search selection")
	"C-f" '((lambda () (interactive) (swiper-all (selection-or-word-at-point)))
		:which-key "search selection in all buffers")

  ; not working
  ;; "wp" '(peek-region-in-split
  ;;   :which-key "peek region in split")
)
(general-define-key
  :keymaps 'override
	:states '(motion normal)
	:prefix "SPC"
	:non-normal-prefix "M-SPC"

	"f" '(swiper
		:which-key "search")
	"C-f" '(swiper-all
		:which-key "search in all buffers")

	"F" '((lambda () (interactive) (swiper (selection-or-word-at-point)))
		:which-key "search cursor word")
	"C-S-f" '((lambda () (interactive) (swiper-all (selection-or-word-at-point)))
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
  "wm" '(ace-swap-window
		:which-key "swap window")
	"wq" '((lambda () (interactive) (evil-quit) (delayed-mode-line-update))
		:which-key "close window")
	"wu" '(winner-undo
		:which-key "undo window config")
	"wU" '(winner-redo
		:which-key "redo window config")
  "wp" '(peek-region-in-split
    :which-key "peek region in split")
	"wt" '((lambda () (interactive) (evil-window-set-height 12))
		:which-key "make into terminal height")

	"n" '(ivy-switch-buffer
		:which-key "switch buffer")

	"ii" '(ivy-resume
		:which-key "ivy resume")
	;; "ip" '(counsel-projectile
	;; 	:which-key "counsel projectile")
	"ip" '(counsel-projectile-find-file
		:which-key "project files")
	"i <tab>" '(projectile-find-other-file
		:which-key "other file")
	"i TAB" '(projectile-find-other-file
		:which-key "other file")
	"ir" '(counsel-recentf
		:which-key "recent files")
	"ig" '(counsel-projectile-grep
		:which-key "project search")
	"if" '(counsel-find-file
		:which-key "files")

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

	"0" '((lambda () (interactive) (winum-select-window-0-or-10) (delayed-mode-line-update))
		:which-key "move to window 0")
	"1"	 '((lambda () (interactive) (winum-select-window-1) (delayed-mode-line-update))
		:which-key "move to window 1")
	"2"	 '((lambda () (interactive) (winum-select-window-2) (delayed-mode-line-update))
		:which-key "move to window 2")
	"3"	 '((lambda () (interactive) (winum-select-window-3) (delayed-mode-line-update))
		:which-key "move to window 3")
	"4"	 '((lambda () (interactive) (winum-select-window-4) (delayed-mode-line-update))
		:which-key "move to window 4")
	"5"	 '((lambda () (interactive) (winum-select-window-5) (delayed-mode-line-update))
		:which-key "move to window 5")
	"6"	 '((lambda () (interactive) (winum-select-window-6) (delayed-mode-line-update))
		:which-key "move to window 6")
	"7"	 '((lambda () (interactive) (winum-select-window-7) (delayed-mode-line-update))
		:which-key "move to window 7")
	"8"	 '((lambda () (interactive) (winum-select-window-8) (delayed-mode-line-update))
		:which-key "move to window 8")
	"9"	 '((lambda () (interactive) (winum-select-window-9) (delayed-mode-line-update))
		:which-key "move to window 9")

	"TAB" '((lambda () (interactive) (switch-to-buffer (other-buffer)) (delayed-mode-line-update))
		:which-key "switch to other buffer")

  "q" '(kill-this-buffer
		:which-key "kill current buffer")
)
; create fake key to represent move to window keys
(push '(("SPC 0") . ("SPC [0-9]" . "move to window [0-9]")) which-key-replacement-alist)
; hide other keys
(push '(("SPC [1-9]") . t) which-key-replacement-alist)

;; mode specific leader

(general-define-key
  :keymaps 'emacs-lisp-mode-map
	:states '(motion normal)
	:prefix "SPC j"

  "e" '(eval-defun
    :which-key "eval defun")
  "E" '(eval-buffer
    :which-key "eval buffer")
)
(general-define-key
  :keymaps 'emacs-lisp-mode-map
	:states '(visual)
	:prefix "SPC j"

  "e" '((lambda () (interactive)
          (message "Evaluating region.")
          (call-interactively 'eval-region))
    :which-key "eval region")
)

;; evil

; disable C-z
(global-set-key (kbd "C-z") nil)
(evil-define-key 'motion 'global (kbd "C-z") nil)
(evil-define-key 'normal 'global (kbd "C-z") nil)
(evil-define-key 'visual 'global (kbd "C-z") nil)
(evil-define-key 'insert 'global (kbd "C-z") nil)
; text zooming
(evil-define-key 'motion 'global (kbd "C--") 'text-scale-decrease)
(evil-define-key 'motion 'global (kbd "C-=") 'text-scale-increase)
; leader
(define-prefix-command 'leader-map)
(evil-define-key 'motion 'global "," 'leader-map)
; manually update things
(evil-define-key 'motion 'global ",r" #'update-heavy-tasks)
; use Q for macro record and q for playback
(evil-define-key 'normal 'global "q" 'evil-execute-macro)
(evil-define-key 'visual 'global "q" (lambda () (interactive) (evil-ex "'<,'>norm @")))
(evil-define-key 'motion 'global "Q" 'evil-record-macro)
(evil-define-key 'visual 'global "Q" 'evil-record-macro)
(evil-define-key 'normal 'global ",q" (lambda () (interactive) (evil-execute-macro 1 (evil-get-register ?q t))))
(evil-define-key 'visual 'global ",q" (lambda () (interactive) (evil-ex "'<,'>norm @q")))
(evil-define-key 'motion 'global ",Q" (lambda () (interactive) (evil-record-macro ?q)))
(evil-define-key 'visual 'global ",Q" (lambda () (interactive) (evil-record-macro ?q)))
; allow repeat in visual mode
(evil-define-key 'visual 'global "." (kbd ";norm . RET"))
; open line above
(defun smart-open-line-above ()
  (interactive)
  ;; (move-beginning-of-line nil)
  (save-excursion
    (newline))
  ; TODO: if we want to indent, uncomment this
  ;; (save-excursion
  ;;   (newline-and-indent))
  ;; (indent-according-to-mode)
)
; use enter and S-enter to open blank lines. TODO implement numeric prefix arg
(evil-define-key 'insert 'global (kbd "S-RET") 'smart-open-line-above)
(evil-define-key 'insert 'global (kbd "<S-return>") 'smart-open-line-above)
; undo redo
(evil-define-key 'normal 'global "u" 'undo)
(evil-define-key 'normal 'global "U" 'redo)
;; (evil-define-key 'normal 'global "U" 'undo-tree-redo)
; use ; for :
(evil-define-key 'motion 'global ";" 'evil-ex)
; repeat last ex command
(evil-define-key 'motion 'global ",." "@:")
; save all
(evil-define-key 'motion 'global ",w" 'evil-write-all)
; align
(evil-define-key 'visual 'global ",a"
  (lambda () (interactive)
    (let ((current-prefix-arg 4)) ;; emulate C-u
      (call-interactively 'align-regexp))))
; start cmd
(evil-define-key 'motion 'global ",;" (lambda () (interactive) (start-process-shell-command (format "cmd(%s)" default-directory) nil "start cmd")))
; sane tabbing
(evil-define-key 'insert 'global (kbd "TAB") 'tab-to-tab-stop)
(evil-define-key 'insert 'global (kbd "<tab>") 'tab-to-tab-stop)
; highlight after searching
(dolist (state '(motion normal visual))
  (evil-define-key state 'global (kbd "n") 'evil-ex-search-next-flash)
  (evil-define-key state 'global (kbd "N") 'evil-ex-search-previous-flash))
; use t instead of * for symbol search
(evil-define-key 'normal 'global "F" (lambda () (interactive) (save-excursion (evil-ex-search-word-forward))))
(evil-define-key 'normal 'global "gF" (lambda () (interactive) (save-excursion (evil-ex-search-unbounded-word-forward))))
(evil-define-key 'visual 'global "F" 'evil-visualstar/begin-search-forward)
; faster surround
(evil-define-key 'normal 'global "s" 'evil-surround-edit)
(evil-define-key 'normal 'global "S" 'evil-Surround-edit)
(evil-define-key 'visual 'global "s" 'evil-surround-region)
; reindent region
(evil-define-key 'visual 'global (kbd ", <tab>")'indent-region)
(evil-define-key 'visual 'global (kbd ", TAB")'indent-region)
(evil-define-key 'normal 'global (kbd ", <tab>") 'indent-buffer)
(evil-define-key 'normal 'global (kbd ", TAB") 'indent-buffer)
; change to last buffer
;; (evil-define-key 'motion 'global (kbd ", TAB") 'evil-buffer)
;; (evil-define-key 'motion 'global (kbd ", <tab>") 'evil-buffer)
; ,d delete line content
(evil-define-key 'normal 'global ",d"
  (lambda ()
    (interactive)
    (if (save-excursion
          (beginning-of-line)
          (looking-at "[ \t]*$"))
        (beginning-of-line)
    (evil-first-non-blank))
    (call-interactively 'evil-delete-line)))
; ,f fix spelling
(evil-define-key 'normal 'global ",f" 'flyspell-auto-correct-word)
(evil-define-key 'normal 'global ",F" (lambda () (interactive) (flyspell-lazy-check-visible) (flyspell-auto-correct-previous-word (point))))
; ,v select line content
;; (evil-define-key 'normal 'global ",v" (lambda () (interactive) (evil-first-non-blank) (evil-visual-char) (evil-last-non-blank)))
(evil-define-key 'normal 'global ",v" 'evil-visual-restore)
(general-define-key
 :states '(motion normal visual)
 
 ;; basic movement
 "k" 'evil-previous-visual-line
 "j" 'evil-next-visual-line
 "h" 'evil-backward-word-begin
 "l" 'evil-forward-word-end
 
 ;; faster movement
 "K" 'fast-move-up
 "J" 'fast-move-down
 "H" 'evil-backward-WORD-begin
 "L" 'evil-forward-WORD-end
 "G" 'evil-first-non-blank
 ":" 'evil-end-of-line

 ;; more comfortable word movement
 "w" 'evil-backward-char
 "e" 'evil-forward-char
 "W" 'fast-move-left
 "E" 'fast-move-right
 "b" 'evil-forward-word-begin
 "B" 'evil-forward-WORD-begin)

(general-define-key
  :states '(visual)
  
  ;; visual selection should not go over the last char
  ":" (lambda () (interactive) (evil-end-of-line)))
; scrolling
(evil-define-key 'motion 'global (kbd "M-j") 'evil-scroll-down)
(evil-define-key 'motion 'global (kbd "M-k") 'evil-scroll-up)
; use M-S-j/k to go to top bottom
(evil-define-key 'motion 'global (kbd "M-J") 'evil-goto-line)
(evil-define-key 'motion 'global (kbd "M-K") 'evil-goto-first-line)
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
; use ( and ) to do parenthesis motion
(evil-define-motion move-to-next-parens () :type exclusive
	(down-list))
(evil-define-motion move-to-prev-parens () :type exclusive
	(backward-up-list))
(evil-define-key 'normal 'global "(" 'evil-sp-backward-sexp)
(evil-define-key 'normal 'global ")" 'evil-sp-forward-sexp)
(evil-define-key 'visual 'global "(" 'evil-sp-backward-sexp)
(evil-define-key 'visual 'global ")" 'evil-sp-forward-sexp)
; move cursor to comfortable reading position
(evil-define-key 'motion 'global ",z" 'evil-comfortable-recenter)
; do not re-copy when pasting in visual mode
(evil-define-key 'visual 'global "p" (lambda () (interactive) (call-interactively 'evil-visual-paste) (pop-kill-ring)))
; substitute command
(evil-define-key 'normal 'global ",s" (lambda () (interactive) (evil-ex "s/")))
(evil-define-key 'normal 'global ",S" (lambda () (interactive) (evil-ex "%s/")))
(evil-define-key 'visual 'global ",s" (lambda () (interactive) (evil-ex "'<,'>s/")))
; flycheck error
(dolist (state '(motion normal visual))
  (evil-define-key state 'global "[f" 'flycheck-previous-error)
  (evil-define-key state 'global "]f" 'flycheck-next-error))
; argument text object
(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
; narrowing
(evil-define-key 'motion 'global ",,n" 'narrow-to-defun)
(evil-define-key 'visual 'global ",,n" 'narrow-to-region)
(evil-define-key 'motion 'global ",,N" 'widen)
; TODO: if we want to indent, add (indent-according-to-mode) after inserting newline
(evil-define-key 'normal 'global (kbd "M-o") (lambda () (interactive) (save-excursion (evil-insert-newline-below))))
(evil-define-key 'normal 'global (kbd "C-o") (lambda () (interactive) (save-excursion (evil-insert-newline-above))))
(evil-define-key 'normal 'global (kbd "C-M-o") (lambda () (interactive) (save-excursion (evil-insert-newline-above)) (save-excursion (evil-insert-newline-below))))
; neo tree
(evil-define-key 'motion 'global ",n" (lambda () (interactive) (neotree-show)))
(evil-define-key 'motion 'global ",N" (lambda () (interactive) (neotree-find)))
(evil-define-key 'normal neotree-mode-map
	; apparently writing neotree-mode-map instead of 'neotree-mode-map works
	;; "h" (neotree-make-executor :dir-fn 'neo-open-dir)
	;; "l" (neotree-make-executor :dir-fn 'neo-open-dir)
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
;; (evil-define-key 'motion 'global (kbd ", SPC") (lambda () (interactive) (evil-ex-nohighlight) (beacon-blink) (mouse-avoidance-banish-mouse)))
(evil-define-key 'motion 'global (kbd ", SPC") 'evil-noh-blink)
; easy quit visual mode
;; (evil-define-key 'visual 'global (kbd ", SPC") (lambda () (interactive) (evil-exit-visual-state) (beacon-blink) (mouse-avoidance-banish-mouse)))
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
;; (evil-define-key 'visual 'global "y" 'evil-yank)
(general-nmap "x" (general-key-dispatch 'evil-exchange
				   :timeout 0.5
			  "w" (general-simulate-key ('evil-exchange "iw"))
			  "W" (general-simulate-key ('evil-exchange "iW"))
			  ")" (general-simulate-key ('evil-exchange "i)"))
			  "]" (general-simulate-key ('evil-exchange "i]"))
			  "}" (general-simulate-key ('evil-exchange "i}"))
			  ">" (general-simulate-key ('evil-exchange "i>"))
			  "'" (general-simulate-key ('evil-exchange "i'"))
			  "\"" (general-simulate-key ('evil-exchange "i\""))
			  "t" (general-simulate-key ('evil-exchange "it"))
			  "n" (general-simulate-key ('evil-exchange "gn"))
))
;; (evil-define-key 'visual 'global "x" 'evil-exchange)
; copying in visual mode goes to the end of the region
(evil-define-key 'visual 'global "y" (lambda () (interactive) (call-interactively 'evil-yank) (evil-goto-mark ?>)))
(evil-define-key 'visual 'global "Y" 'evil-yank)
; pasting goes to the end of the region
(evil-define-key 'normal 'global "p" (lambda () (interactive) (call-interactively 'evil-paste-after) (evil-goto-mark ?\])))
; C-p paste then select region (for easy replace)
(evil-define-key 'normal 'global (kbd "C-p") (lambda () (interactive) (call-interactively 'evil-paste-after) (evil-goto-mark ?\[) (evil-visual-char) (evil-goto-mark ?\])))
; use ivy to select kill ring
(evil-define-key 'normal 'global (kbd ",p") 'counsel-yank-pop)
; same for exchange
(evil-define-key 'visual 'global "x" (lambda () (interactive) (call-interactively 'evil-exchange) (evil-goto-mark ?>)))
(evil-define-key 'visual 'global "X" 'evil-exchange)
; join with ,j
(evil-define-key 'normal 'global ",j" 'evil-join)
(evil-define-key 'visual 'global ",j" 'evil-join)
; break with ,h
(evil-define-key 'normal 'global ",h" 'newline)

;; latex
(general-define-key
	:keymaps '(latex-mode-map TeX-mode-map)
	:states '(motion normal)
	:prefix "SPC"
	:non-normal-prefix "M-SPC"

	"jp" '(preview-buffer
		:which-key "preview buffer")

	"jP" '(preview-clearout-buffer
		:which-key "clear preview buffer")
)

;; eshell
;; TODO apparently not working
(evil-define-key 'normal eshell-mode-map (kbd "C-j") 'eshell-previous-prompt)
(evil-define-key 'normal eshell-mode-map (kbd "C-k") 'eshell-next-prompt)
(evil-define-key 'insert eshell-mode-map (kbd "M-j") 'eshell-previous-matching-input-from-input)
(evil-define-key 'insert eshell-mode-map (kbd "M-k") 'eshell-next-matching-input-from-input)

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
(define-key helm-map (kbd "M-j") 'helm-next-line)
(define-key helm-map (kbd "M-k") 'helm-previous-line)
; in file window, move up one level using C-h
(define-key helm-find-files-map (kbd "C-h") 'helm-find-files-up-one-level)

;; ivy, counsel and swiper
(general-define-key
	:keymaps '(swiper-map ivy-minibuffer-map counsel-imenu-map)
	"M-j" 'ivy-next-line
	"M-k" 'ivy-previous-line
	"M-J" 'ivy-scroll-up-command
	"M-K" 'ivy-scroll-down-command
	; ivy-next-history-element allows inserting cursor symbol.
	"C-j" 'ivy-next-history-element
	"C-k" 'ivy-previous-history-element
	"M-RET" 'ivy-dispatching-done
	"<M-return>" 'ivy-dispatching-done
	"M-S-RET" 'ivy-dispatching-call ; do not exit after. useful for copy.
	"<M-S-return>" 'ivy-dispatching-call
	"S-RET" 'ivy-immediate-done ; use exact input, not candidate
	"<S-return>" 'ivy-immediate-done
	"M-l" 'ivy-done
	"C-M-l" 'ivy-immediate-done
	"M-L" 'ivy-dispatching-done
	"M-n" 'ivy-call
	"M-N" 'ivy-dispatching-call
	"M-h" 'ivy-backward-kill-word
	"M-o" 'ivy-occur ; save to temp buffer for manipulation

	"j" (general-key-dispatch 'self-insert-command
		:timeout 0.25
		"j" 'self-insert-command
		;; "l" 'ivy-done
		"k" 'minibuffer-keyboard-quit
		"h" 'ivy-backward-kill-word
		"p" 'ivy-partial ; complete text
	)
)
(general-define-key
	:keymaps '(swiper-map)
	"M-s" 'swiper-query-replace
)
(general-define-key
	:keymaps '(swiper-all-map)
	"M-s" 'swiper-all-query-replace
)
(general-define-key ; use / to enter directory, not ENTER.
	:keymaps '(counsel-find-file-map)
	; use return for opening directory
  ; TODO: temporarily disabled
	;; "RET" 'ivy-alt-done
	;; "<return>" 'ivy-alt-done
	"S-RET" 'ivy-immediate-done ; use exact input, not candidate
	"<S-return>" 'ivy-immediate-done
	;; "M-l" 'ivy-alt-done
)

;; minibuffer
(general-define-key
  :keymaps '(minibuffer-local-shell-command-map)
  "M-k" 'previous-line-or-history-element
  "M-j" 'next-line-or-history-element
  "M-l" 'exit-minibuffer
)
;; ex mode
(general-define-key
  :keymaps '(minibuffer-local-map evil-ex-completion-map)
  "M-k" 'previous-complete-history-element
  "M-j" 'next-complete-history-element
  "M-l" 'exit-minibuffer
)


;; help mode
(evil-define-key 'motion help-mode-map (kbd "u") 'help-go-back)
(evil-define-key 'normal help-mode-map (kbd "u") 'help-go-back)
(evil-define-key 'motion help-mode-map (kbd "U") 'help-go-forward)
(evil-define-key 'normal help-mode-map (kbd "U") 'help-go-forward)

;; web mode
(evil-define-key 'normal web-mode-map (kbd "C-h") nil)
(evil-define-key 'normal web-mode-map (kbd "C-l") nil)
;; (evil-define-key 'normal web-mode-map (kbd "C-h") 'web-mode-fold-or-unfold)
;; (evil-define-key 'normal web-mode-map (kbd "C-l") 'web-mode-fold-or-unfold)

;; outshine mode
(dolist (state '(motion normal visual))
  (evil-define-key state 'global (kbd "C-h") 'outline-up-heading-custom)
  (evil-define-key state 'global (kbd "C-l") 'outline-next-heading)
  (evil-define-key state 'global (kbd "C-k") 'outline-backward-heading-same-level-custom)
  (evil-define-key state 'global (kbd "C-j") 'outline-forward-same-level)
  (evil-define-key state 'global (kbd "C-S-h") 'outline-promote)
  (evil-define-key state 'global (kbd "C-S-l") 'outline-demote)
  (evil-define-key state 'global (kbd "C-S-k") 'outline-move-subtree-up)
  (evil-define-key state 'global (kbd "C-S-j") 'outline-move-subtree-down))
(dolist (key '("M-RET" "<M-return>"))
  (evil-define-key 'insert 'global (kbd key) 'outline-insert-heading))

;; origami mode (disabled due to performance)
;; (evil-define-key 'normal 'global (kbd "C-g") 'origami-close-node-recursively)
;; (evil-define-key 'normal 'global (kbd "C-j") 'origami-forward-fold)
;; (evil-define-key 'normal 'global (kbd "C-k") 'origami-previous-fold)
;; (evil-define-key 'normal 'global (kbd "C-;") 'origami-recursively-toggle-node)
;; (evil-define-key 'normal 'global "Z" 'origami-close-all-nodes)
;; (evil-define-key 'normal 'global "X" (lambda () (interactive) (origami-open-all-nodes (current-buffer)) (origami-mode -1) (origami-mode 1)))
;; (evil-define-key 'normal 'global "zx" (lambda () (interactive)
;; 	(origami-show-only-node (current-buffer) (point))
;; 	(origami-open-node-recursively (current-buffer) (point))))
;; (evil-define-key 'normal 'global "zu" 'origami-undo)
;; (evil-define-key 'normal 'global "zU" 'origami-redo)

;; insert mode mappings
; yas
(evil-define-key 'insert 'global (kbd "M-j") 'yas-next-field)
(evil-define-key 'insert 'global (kbd "M-k") 'yas-prev-field)
(evil-define-key 'insert 'global (kbd "M-l") 'yas-insert-snippet)
(evil-define-key 'insert yas-minor-mode-map (kbd "M-l") yas-maybe-expand)
; emmet
(evil-define-key 'insert emmet-mode-keymap (kbd "M-j") 'yas-next-field)
(evil-define-key 'insert emmet-mode-keymap (kbd "M-k") 'yas-prev-field)
(evil-define-key 'insert emmet-mode-keymap (kbd "M-l") 'yas-insert-snippet)
(evil-define-key 'insert yas-minor-mode-map (kbd "M-l") yas-maybe-expand)
(evil-define-key 'insert emmet-mode-keymap (kbd "C-j") 'emmet-next-edit-point)
(evil-define-key 'insert emmet-mode-keymap (kbd "C-k") 'emmet-prev-edit-point)
(evil-define-key 'insert emmet-mode-keymap (kbd "C-l") 'emmet-expand-line)
(evil-define-key 'visual emmet-mode-keymap (kbd "C-l") 'emmet-wrap-with-markup)
; spell correction
(evil-define-key 'insert 'global (kbd "C-SPC") (lambda () (interactive) (save-excursion (flyspell-lazy-check-pending) (flyspell-auto-correct-previous-word (point)))))
(evil-define-key 'insert 'global (kbd "<C-space>") (lambda () (interactive) (save-excursion (flyspell-lazy-check-pending) (flyspell-auto-correct-previous-word (point)))))
; insert space and move left
(evil-define-key 'insert 'global (kbd "S-SPC") (lambda () (interactive) (save-excursion (insert " "))))
(evil-define-key 'insert 'global (kbd "<S-space>") (lambda () (interactive) (save-excursion (insert " "))))
; use c-hjkl to move around
(evil-define-key 'insert 'global (kbd "M-S-g") 'evil-first-non-blank)
(evil-define-key 'insert 'global (kbd "M-S-h") 'left-word)
(evil-define-key 'insert 'global (kbd "M-S-j") 'next-line)
(evil-define-key 'insert 'global (kbd "M-S-k") 'previous-line)
(evil-define-key 'insert 'global (kbd "M-S-l") 'right-word)
(evil-define-key 'insert 'global (kbd "M-S-;") 'end-of-line)
; use M-j/k/l to do completion
;; (evil-define-key 'insert 'global (kbd "M-j") 'company-complete-common-or-cycle)
;; (evil-define-key 'insert 'global (kbd "M-k") 'company-select-previous)
;; (evil-define-key 'insert 'global (kbd "M-l") 'company-complete-selection)
;; (define-key company-active-map (kbd "M-j") 'company-complete-common-or-cycle)
;; (define-key company-active-map (kbd "M-k") 'company-select-previous)
;; (define-key company-active-map (kbd "M-l") 'company-complete-selection)
; j mappings
(setq insert-mode-j-mapping-func (general-key-dispatch
	; fallback
	(lambda () (interactive)
		(let ((my-company--company-command-p-override t))
			(call-with-command-hooks (lambda () (interactive) (self-insert-or-send-raw "j")))))
	:timeout 1.0
	"j" (lambda () (interactive) (call-with-command-hooks (lambda () (interactive) (self-insert-or-send-raw "j")) "jj"))
	"t" (lambda () (interactive) (call-with-command-hooks 'insert-todo "jt"))
	"f" (lambda () (interactive) (call-with-command-hooks 'insert-backslash "jf"))
	; jk quit insert mode
	"k" (lambda () (interactive) (call-with-command-hooks 'evil-normal-state "jk"))
	; jh delete word
	"h" (lambda () (interactive) (call-with-command-hooks 'evil-delete-backward-word "jh"))
	; jg move to start of line
	"g" (lambda () (interactive) (call-with-command-hooks 'evil-first-non-blank "jg"))
	; jl move to end of line
	"l" (lambda () (interactive) (call-with-command-hooks 'move-end-of-line "jl"))
	; jp complete
	"p" 'company-smart-complete
	;; "p" (lambda () (interactive) (call-with-command-hooks 'company-smart-complete "jp"))
  ; j[ skip TabNine
  "[" (lambda () (interactive) (call-with-command-hooks 'company-tabnine-call-other-backends "j["))
	"0" (lambda () (interactive) (call-with-command-hooks 'company-complete-number-0 "j0"))
	"1" (lambda () (interactive) (call-with-command-hooks 'company-complete-number-1 "j1"))
	"2" (lambda () (interactive) (call-with-command-hooks 'company-complete-number-2 "j2"))
	"3" (lambda () (interactive) (call-with-command-hooks 'company-complete-number-3 "j3"))
	"4" (lambda () (interactive) (call-with-command-hooks 'company-complete-number-4 "j4"))
	"5" (lambda () (interactive) (call-with-command-hooks 'company-complete-number-5 "j5"))
	"6" (lambda () (interactive) (call-with-command-hooks 'company-complete-number-6 "j6"))
	"7" (lambda () (interactive) (call-with-command-hooks 'company-complete-number-7 "j7"))
	"8" (lambda () (interactive) (call-with-command-hooks 'company-complete-number-8 "j8"))
	"9" (lambda () (interactive) (call-with-command-hooks 'company-complete-number-9 "j9"))
	; j[ context complete (TODO)
	;; "[" 'evil-complete-next
	; j[ insert snippet
	;; "[" (lambda () (interactive) (call-with-command-hooks 'yas-insert-snippet))
	; jv to paste from default register
	"v" (lambda () (interactive) (call-with-command-hooks 'paste-from-default-register "jv"))
))
(setq insert-mode-J-mapping-func (general-key-dispatch
	(lambda () (interactive)
		(let ((my-company--company-command-p-override t))
			(call-with-command-hooks (lambda () (interactive) (self-insert-or-send-raw "J")))))
	:timeout 1.0
	"J" (lambda () (interactive) (call-with-command-hooks (lambda () (interactive) (self-insert-or-send-raw "J")) "JJ"))
	; JV to use counsel yank-pop
	"V" (lambda () (interactive) (call-with-command-hooks 'counsel-yank-pop "JV"))
))
(evil-define-command insert-mode-j-mapping () :repeat nil (interactive)
	(call-interactively insert-mode-j-mapping-func))
(evil-define-command insert-mode-J-mapping () :repeat nil (interactive)
	(call-interactively insert-mode-J-mapping-func))
;; make sure company-continue-commands allow insert-mode-j-mapping (such as having 'not at first)
;; setting :repeat to nil because we don't want the "j" part to be repeatable, only the actual commands invoked afterwards.
(general-imap "j" 'insert-mode-j-mapping)
(general-imap "J" 'insert-mode-J-mapping)
(define-key company-active-map "j" 'insert-mode-j-mapping)
(define-key company-active-map "J" 'insert-mode-J-mapping)
(put 'insert-mode-j-mapping 'company-keep t)
(put 'insert-mode-J-mapping 'company-keep t)
"Since company-tng-frontend only complete selection when pressing any key that isn't
a company-mode command (checked with `my-company--company-command-p` function), and we want `general-key-dispatch`
to have \"j\" as a company-mode command (so do not complete) but not to have
\"jp\" as one (so do completion)."
;; (eval-after-load 'company
;;   '(progn
;; 	 (define-key company-active-map (kbd "C-z") 'company-quickhelp-manual-begin)))

;; window management
(evil-define-key 'motion 'global (kbd "C-w C-h") (lambda () (interactive) (evil-window-left 1) (delayed-mode-line-update)))
(evil-define-key 'motion 'global (kbd "C-w C-j") (lambda () (interactive) (evil-window-down 1) (delayed-mode-line-update)))
(evil-define-key 'motion 'global (kbd "C-w C-k") (lambda () (interactive) (evil-window-up 1) (delayed-mode-line-update)))
(evil-define-key 'motion 'global (kbd "C-w C-l") (lambda () (interactive) (evil-window-right 1) (delayed-mode-line-update)))
;; (evil-define-key 'motion 'global ",wv" (lambda () (interactive) (evil-window-vsplit) (delayed-mode-line-update)))
;; (evil-define-key 'motion 'global ",wh" (lambda () (interactive) (evil-window-split) (delayed-mode-line-update)))
;; (evil-define-key 'motion 'global ",wq" (lambda () (interactive) (evil-quit) (delayed-mode-line-update)))
(evil-define-key 'motion 'global ",0" (lambda () (interactive) (winum-select-window-0) (delayed-mode-line-update)))
(evil-define-key 'motion 'global ",1" (lambda () (interactive) (winum-select-window-1) (delayed-mode-line-update)))
(evil-define-key 'motion 'global ",2" (lambda () (interactive) (winum-select-window-2) (delayed-mode-line-update)))
(evil-define-key 'motion 'global ",3" (lambda () (interactive) (winum-select-window-3) (delayed-mode-line-update)))
(evil-define-key 'motion 'global ",4" (lambda () (interactive) (winum-select-window-4) (delayed-mode-line-update)))
(evil-define-key 'motion 'global ",5" (lambda () (interactive) (winum-select-window-5) (delayed-mode-line-update)))
(evil-define-key 'motion 'global ",6" (lambda () (interactive) (winum-select-window-6) (delayed-mode-line-update)))
(evil-define-key 'motion 'global ",7" (lambda () (interactive) (winum-select-window-7) (delayed-mode-line-update)))
(evil-define-key 'motion 'global ",8" (lambda () (interactive) (winum-select-window-8) (delayed-mode-line-update)))
(evil-define-key 'motion 'global ",9" (lambda () (interactive) (winum-select-window-9) (delayed-mode-line-update)))

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
(setq-default use-line-nav nil)
(evil-define-motion adaptive-avy () :type exclusive :repeat nil :jump t
	(if use-line-nav (evil-avy-goto-line) (evil-avy-goto-word-0 nil)))
(evil-define-key 'motion 'global "f" 'adaptive-avy)
;; (evil-define-key 'motion 'global "f" 'evil-avy-goto-word-0)
(evil-define-key 'motion 'global "F" 'evil-avy-goto-char-2)

;; imenu-list
(evil-define-key 'normal imenu-list-major-mode-map "o" 'imenu-list-goto-entry)
(evil-define-key 'normal imenu-list-major-mode-map (kbd "TAB") 'imenu-list-display-entry)
(evil-define-key 'normal imenu-list-major-mode-map (kbd "<tab>") 'imenu-list-display-entry)

;; misc bindings
; use alt-h for help instead of ctrl-h
(bind-key* (kbd "C-M-h") help-map)
(bind-key* (kbd "C-M-h C-M-h") 'counsel-apropos)
(bind-key* (kbd "C-M-x") 'execute-extended-command)


;;; misc settings

;; shell
(cond
 ((eq system-type 'darwin)
  (setq explicit-shell-file-name "/usr/local/bin/zsh"))
 ((not (eq system-type 'windows-nt))
  (setq explicit-shell-file-name "/usr/bin/zsh")))

;; no alert sounds
(setq ring-bell-function 'ignore)

;; file-type based syntax entry
(add-hook 'sql-mode-hook (lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'prog-mode-hook (lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'lisp-mode-hook (lambda () (modify-syntax-entry ?- "w")))
(add-hook 'emacs-lisp-mode-hook (lambda () (modify-syntax-entry ?- "w")))
(add-hook 'racket-mode-hook (lambda () (modify-syntax-entry ?- "w")))
(add-hook 'html-mode-hook (lambda () (modify-syntax-entry ?- "w") (modify-syntax-entry ?_ "w")))
(add-hook 'web-mode-hook (lambda () (modify-syntax-entry ?- "w") (modify-syntax-entry ?_ "w")))
(add-hook 'nxml-mode-hook (lambda () (modify-syntax-entry ?- "w") (modify-syntax-entry ?_ "w")))
(add-hook 'css-mode-hook (lambda () (modify-syntax-entry ?- "w") (modify-syntax-entry ?_ "w")))

;; indent settings
(setq default-indent-tabs-mode nil)
(setq-default indent-tabs-mode default-indent-tabs-mode) ; use tabs instead of space
(setq-default tab-width 4)
(setq-default evil-shift-width tab-width)
(setq-default python-indent 4)
(setq-default c-basic-offset 4)
(setq-default backward-delete-char-untabify-method 'nil)
(add-hook 'prog-mode-hook (lambda () (setq evil-shift-width tab-width)))
(add-hook 'python-mode-hook (lambda ()
	(setq-local tab-width 4)
	(setq-local indent-tabs-mode default-indent-tabs-mode)
	(setq-local python-indent-offset tab-width)
	(setq-local highlight-indentation-offset tab-width)
	(setq-local python-indent tab-width)
	(setq-local evil-shift-width tab-width)
	(setq-local yas-indent-line 'auto)
) t)
(add-hook 'web-mode-hook (lambda ()
  (setq-local tab-width 2)
  (setq-local evil-shift-width tab-width)
	(setq-local highlight-indentation-offset 4)
))
(add-hook 'json-mode-hook (lambda ()
  (setq-local tab-width 2)
  (setq-local evil-shift-width tab-width)
	(setq-local highlight-indentation-offset 4)
  (setq-local js-indent-level 2)
))
(add-hook 'css-mode-hook (lambda ()
  (setq-local tab-width 2)
  (setq-local evil-shift-width tab-width)
	(setq-local highlight-indentation-offset 4)
  (setq-local web-mode-css-indent-offset 2)
  (setq-local css-indent-offset 2)
))
(add-hook 'typescript-mode-hook (lambda ()
  (setq-local tab-width 2)
  (setq-local typescript-indent-level 2)
  (setq-local evil-shift-width tab-width)
	(setq-local highlight-indentation-offset 4)
  (setq-local js-indent-level 2)
))
(add-hook 'latex-mode-hook (lambda ()
  (setq-local tab-width 2)
	(setq-local indent-tabs-mode default-indent-tabs-mode)
  (setq-local evil-shift-width tab-width)
))
(add-hook 'TeX-mode-hook (lambda ()
  (setq-local tab-width 2)
	(setq-local indent-tabs-mode default-indent-tabs-mode)
  (setq-local evil-shift-width tab-width)
))
(add-hook 'emacs-lisp-mode-hook (lambda ()
	(setq-local indent-tabs-mode nil)
	(setq-local tab-width 2)
	(setq-local evil-shift-width tab-width)
))
(add-hook 'haskell-mode-hook (lambda ()
	(setq-local tab-width 4)
	(setq-local evil-shift-width tab-width)
	(setq-local haskell-indentation-starter-offset tab-width)
	(setq-local haskell-indentation-left-offset tab-width)
	(setq-local haskell-indentation-layout-offset tab-width)
))

;; set frame title
(setq frame-title-format (concat "TommyX's Emacs " emacs-version))

;; attempt to improve font-lock performance
;; (setq jit-lock-defer-time 0)

;; attempt to improve subprocess performance
(setq process-adaptive-read-buffering nil)

;; attempt to improve font performance
(setq inhibit-compacting-font-caches t)

;; attempt to improve long line performance
(setq-default bidi-display-reordering nil)

;; UTF-8 as default encoding
(setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8-mac) ; For old Carbon emacs on OS X only
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(if (eq system-type 'windows-nt)
	(set-selection-coding-system 'utf-16-le) ; fix inability to paste non-ascii char
	(set-selection-coding-system 'utf-8))
(prefer-coding-system 'utf-8)

;; scroll-off emulation
;; (setq scroll-margin (/ (* (window-total-height) 2) 7))
(setq scroll-margin 16)

;; wrap lines
(add-hook 'prog-mode-hook (lambda () (setq truncate-lines nil)))
(add-hook 'text-mode-hook (lambda () (setq truncate-lines nil)))

;; subword motion
;; (global-subword-mode t) ; TODO this makes evil cursor word search not work

;; auto load if changed
(global-auto-revert-mode t)

;; profiler
(setq profiler-max-stack-depth 64)

;; auto start server if on GUI
(and window-system (server-start))

;; server use different window
(setq server-window 'pop-to-buffer)

;; no auto saving
(add-hook 'prog-mode-hook (lambda () (auto-save-mode -1)))
(add-hook 'text-mode-hook (lambda () (auto-save-mode -1)))

;; no backup file
(setq make-backup-files nil)

;; auto display line numbers (turned off for performance)
;; (add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode)))
;; (add-hook 'sgml-mode-hook (lambda () (display-line-numbers-mode)))
(setq display-line-numbers-width-start t)
(setq display-line-numbers-grow-only nil)

;; indicate end of buffer
(add-hook 'prog-mode-hook (lambda () (setq indicate-buffer-boundaries t)))
(add-hook 'text-mode-hook (lambda () (setq indicate-buffer-boundaries t)))

;; disable blink
(blink-cursor-mode 0)

;; hl-line-mode for some modes
(add-hook 'buffer-menu-mode-hook (lambda () (hl-line-mode 1) (setq-local use-line-nav t)))
(add-hook 'profiler-report-mode-hook (lambda () (hl-line-mode 1) (setq-local use-line-nav t)))
(add-hook 'org-agenda-mode-hook (lambda () (hl-line-mode 1) (setq-local use-line-nav t)))
; disable in insert and visual mode
(add-hook 'prog-mode-hook (lambda () (hl-line-mode 1)))
(add-hook 'text-mode-hook (lambda () (hl-line-mode 1)))
(add-hook 'evil-insert-state-entry-hook (lambda () (hl-line-mode -1)))
(add-hook 'evil-insert-state-exit-hook (lambda () (hl-line-mode 1)))
(add-hook 'evil-visual-state-entry-hook (lambda () (hl-line-mode -1)))
(add-hook 'evil-visual-state-exit-hook (lambda () (hl-line-mode 1)))

;; cursor line (right now disabled)
;; (global-hl-line-mode 1)
;; (setq global-hl-line-sticky-flag t)

;; garbage collection (improve some performance)
(setq gc-cons-threshold 200000000)
(run-with-idle-timer 5 t (lambda () (garbage-collect)))
(add-hook 'focus-out-hook (lambda () (garbage-collect)))

;; save clipboard onto kill ring
(setq save-interprogram-paste-before-kill t)

;; fringe bitmap
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
   #b00011000])

;; word wrap
(add-hook 'prog-mode-hook (lambda () (toggle-word-wrap 1)))
(add-hook 'text-mode-hook (lambda () (toggle-word-wrap 1)))
(add-hook 'sgml-mode-hook (lambda () (toggle-word-wrap 1)))

;; window divider
(setq window-divider-default-places 't)
(setq window-divider-default-right-width 7)
(setq window-divider-default-bottom-width 7)
(window-divider-mode 1)

;; minibuffer background
;; TODO: failed
;; (defface minibuffer-background
;;   '((t (:inherit default)))
;;   "*Face used for the minibuffer."
;;   :group 'appearence)
;; (add-hook 'minibuffer-setup-hook
;;           (lambda ()
;;             (make-local-variable 'face-remapping-alist)
;;             (add-to-list 'face-remapping-alist '(default minibuffer-background))))

;; sidebar face
(defface sidebar-background
  '((t (:inherit default)))
  "*Face used for the sidebar."
  :group 'appearence)

;; undo limits
(setq undo-limit 1000000)
(setq undo-strong-limit 1000000)

;; fringe margin
(setq-default left-fringe-width 16)
(setq-default right-fringe-width 10)

;; open buffer performance
;; (add-hook 'find-file-hooks 'vc-find-file-hook)
;; (setq vc-handled-backends nil)

;; input response (experimental)
;; (setq input-feedback-ov nil)
;; (defun before-insert-advice (&rest _)
;; 	"Flash input feedback."
;; 	;; (when input-feedback-ov
;; 	;; 	(delete-overlay input-feedback-ov)
;; 	;; )
;; 	;; (when (eq evil-state 'insert)
;; 	;; 	(setq input-feedback-ov (make-overlay (point) (- (point) 1)))
;; 	;; 	(overlay-put input-feedback-ov 'priority 9999)
;; 	;; 	(overlay-put input-feedback-ov 'window (selected-window))
;; 	;; 	(overlay-put input-feedback-ov 'face 'evil-goggles-yank-face)
;; 	;; 	(redisplay)
;; 	;; )
;; )
(setq eager-redisplay-allowed t)
;; (defun eager-redisplay-insert-advice (&rest _)
;; 	(when (and (eq evil-state 'insert) eager-redisplay-allowed)
;; 		(redisplay t)))
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
		yas-expand))
(defun eager-redisplay-mode ()
  "Minor mode that force redraw after command."
	(interactive)
	(if eager-redisplay-mode-on
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
(eager-redisplay-mode)

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

;; compilation
(setq compilation-scroll-output 'first-error)

;; indentation guide using whitespace mode
(setq whitespace-style '(
	tab-mark face tabs
))
(setq whitespace-display-mappings '(
	(tab-mark ?\t	[?\| ?\t])
))
;; (global-whitespace-mode 1)

;; tabify only leading whitespace
(setq tabify-regexp "^\t* [ \t]+")

;; mouse avoidance (move to top right corner)
(setq make-pointer-invisible t)
;; (setq mouse-avoidance-banish-position
;; 	'((frame-or-window . frame)
;; 	(side . right)
;; 	(side-pos . -5)
;; 	(top-or-bottom . top)
;; 	(top-or-bottom-pos . -5)))
;; (mouse-avoidance-mode 'banish)
;; (mouse-avoidance-mode 'none)

;; flyspell
(setq flyspell-issue-message-flag nil)

;; tramp
(setq tramp-default-method "ssh")

;; blink matching parens
(setq blink-matching-paren nil) ; we have smartparens, don't need this

;; winner mode (record window config change so can undo)
(winner-mode 1)

;; mac use correct modifier keys
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

;; encourage taking a break
(setq type-break-health-quotes
      '("Decrease chance of death from heart disease"
        "Increase lifespan"
        "Decrease chance of dementia"
        "No longer reverse effect of exercise"
        "Decrease chance of diabetes"
        "Decrease chance of leg deep vein thrombosis (DVT), clot that kills"
        "Decrease chance of anxiety"
        "Decrease back pain and permanent damage"
        "Decrease chance of varicose veins"
        "Decrease chance of death from all types of cancer"
        "Decrease blood pressure and blood sugar"))
(defun type-break-my-query-function (prompt)
  (yes-or-no-p
   (concat
    prompt
    (propertize (format "(!! %s !!) "
                        (get-random-element
                         type-break-health-quotes))))))
(defun type-break-schedule-check (&rest _)
  (when (null type-break-time-next-break)
      (type-break-schedule)))
(setq type-break-query-function 'type-break-my-query-function)
(setq type-break-interval 1800)
(setq type-break-good-rest-interval 300)
(setq type-break-demo-boring-stats t)
(setq type-break-keystroke-threshold '(nil . nil))
(setq type-break-warning-repeat 0)
(setq type-break-demo-functions '(type-break-demo-boring))
(type-break-mode 1)
(type-break-query-mode 1)
(run-at-time 0 120 'type-break-schedule-check)

;; eldoc
(global-eldoc-mode 1)

;; enable some modes
; flyspell
(dolist (hook '(prog-mode-hook))
	(add-hook hook (lambda () (flyspell-prog-mode))))
(dolist (hook '(text-mode-hook))
	(add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
	(add-hook hook (lambda () (flyspell-mode -1))))

;; companion
(companion-open)

;;; org
(load-relative "./tommyx-org.el")

;;; project + workspace
(load-relative "./tommyx-project.el")
