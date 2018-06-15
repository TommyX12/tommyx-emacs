
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

(eval-when-compile
  (require 'use-package))

;;; install the following packages
(use-package evil :ensure t)
(use-package helm :ensure t)
(use-package helm-flx :ensure t)
(use-package which-key :ensure t)
(use-package spacemacs-theme :ensure t :defer t
             :init (load-theme 'spacemacs-dark t))


;;; package settings

;; evil-mode
(evil-mode 1) ; use evil-mode at startup

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
(evil-define-key 'normal 'global (kbd "C-n") 'helm-mini)
; use ctrl-p for find files
(evil-define-key 'normal 'global (kbd "C-p") 'helm-find-files)

;; evil
; allow repeat in visual mode
(evil-define-key 'visual 'global "." (kbd ":norm . RET")) 
; use U for redo
(evil-define-key 'normal 'global "U" 'undo-tree-redo)
; use ; for :
(evil-define-key 'motion 'global ";" 'evil-ex)


;;; misc settings

;; auto display line numbers
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode)))

;; set font
(set-face-attribute 'default nil
                    :family "Consolas"
                    :height 150
                    :weight 'normal
                    :width 'normal)

;; full screen automatically
(toggle-frame-fullscreen)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)





;;; the following may load my stuff
;; (setq user-init-file (or load-file-name (buffer-file-name)))
;; (setq user-emacs-directory (file-name-directory user-init-file))
