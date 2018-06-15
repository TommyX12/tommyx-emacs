
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
(use-package spacemacs-theme :defer t
             :init (load-theme 'spacemacs-dark t))


;;; package settings

;; evil-mode
(evil-mode 1) ; use evil-mode at startup

;; which key
(which-key-mode)
(setq which-key-idle-delay 0.5)

;; helm
(require 'helm-config)
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

;; evil
; allow repeat in visual mode
(evil-define-key 'visual 'global "." (kbd ":norm . RET")) 
; use U for redo
(evil-define-key 'normal 'global "U" 'undo-tree-redo)


;;; misc settings

(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode)))

;; full screen automatically
(toggle-frame-fullscreen)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)





;;; the following may load my stuff
;; (setq user-init-file (or load-file-name (buffer-file-name)))
;; (setq user-emacs-directory (file-name-directory user-init-file))
