;; TODO: deal with dependencies between these packages.
;; TODO: deal with the init statements in packages
;; use-package is fine, but requires aren't.
;; so we should automatically find the require statements in the packages imported through require.

;; initialize
(setq load-prefer-newer t)
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)
;; TODO temporarily disabled
;; (package-refresh-contents)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(use-package auto-compile :ensure t
  :config
  (auto-compile-on-load-mode))
(defun enable-auto-compilation (file)
  (when (symbolp file)
    (setq file (packed-locate-library (symbol-name file))))
  (let (dest)
    (when (and file
               (setq dest (byte-compile-dest-file file))
               (not (file-exists-p dest)))
      (message "Compiling %s..." file)
      (byte-compile-file file)
      (message "Compilation of %s done." file))))

;; packages
(add-to-list 'load-path (file-name-directory load-file-name))
(add-to-list 'load-path
             (expand-file-name "packages"
                               (file-name-directory load-file-name)))
(add-to-list 'load-path
             (expand-file-name "packages/external"
                               (file-name-directory load-file-name)))
(add-to-list 'load-path
             (expand-file-name "infinity-theme"
                               (file-name-directory load-file-name)))
(add-to-list 'custom-theme-load-path
             (expand-file-name "infinity-theme"
                               (file-name-directory load-file-name)))
(use-package auto-package-update :ensure t)
(enable-auto-compilation 'redo+)
(require 'redo+)
(enable-auto-compilation 'font-lock+)
(require 'font-lock+)
(enable-auto-compilation 'hl-line+)
(require 'hl-line+)
(enable-auto-compilation 'info+)
(require 'info+)
(use-package package-lint :ensure t)
(use-package highlight-function-calls :ensure t)
(use-package dash :ensure t :pin melpa)
(use-package ht :ensure t)
(use-package s :ensure t)
(use-package f :ensure t)
(use-package cl-lib :ensure t)
(use-package htmlize :ensure t)
(use-package request :ensure t)
(use-package json :ensure t)
(use-package unicode-escape :ensure t)
(use-package alert :ensure t)
(use-package emms :ensure t :after evil
  :config
  (require 'emms-setup)
  (require 'emms-player-simple))
;; (use-package undo-tree :ensure t
;;   :config
;;   ;; attempt to fix bug
;;   (setq undo-tree-enable-undo-in-region nil)
;;   ;; persistent undo
;;   (setq undo-tree-auto-save-history t)
;;   (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/history"))))
(use-package all-the-icons :ensure t)
(use-package evil :ensure t
  :init
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-want-integration nil)
  ;; https://github.com/emacs-evil/evil-collection/issues/60
  (setq evil-want-keybinding nil)
  (setq evil-search-module 'evil-search))
(use-package evil-collection :ensure t :after evil
  :init
  (setq evil-collection-company-use-tng nil)
  (setq evil-collection-setup-minibuffer nil)
  ;; do not allow certain keys to be used by evil-collection
  ;; TODO: We disabled J and K to encourage ivy use
  (setq evil-collection-key-blacklist
        '("SPC" "K")))
(use-package evil-visualstar :ensure t)
(use-package evil-surround :ensure t)
(use-package evil-args :ensure t)
(use-package evil-matchit :ensure t :defer t)
(use-package evil-numbers :ensure t)
(use-package evil-exchange :ensure t)
(use-package evil-search-highlight-persist :ensure t)
(use-package evil-nerd-commenter :ensure t)
(use-package hydra :ensure t)
(use-package projectile :ensure t)
(require 'per-frame-header-mode-line)
(use-package smex :ensure t)
(use-package helm :ensure t
  :config
  (require 'helm-config))
(use-package helm-flx :ensure t)
(use-package helm-descbinds :ensure t)
(use-package helm-describe-modes :ensure t)
(use-package helm-swoop :ensure t)
(use-package helm-projectile :ensure t :after projectile)
(use-package swiper-helm :ensure t)
(use-package ivy :ensure t)
(use-package ivy-posframe :ensure t :after ivy)
(use-package all-the-icons-ivy :ensure t :after ivy)
(use-package ivy-rich :ensure t)
;; TODO: There is a bug. Might cause closing some window to close emacs.
;; (use-package popwin :ensure t
;;  :config
;;  (setq popwin:adjust-other-windows t)
;;  (popwin-mode 1)
;;  (add-hook 'popwin:after-popup-hook (lambda () (delayed-mode-line-update))))
(use-package counsel :ensure t)
(use-package counsel-projectile :ensure t :after projectile)
(use-package google-this :ensure t)
(use-package swiper :ensure t)
(use-package which-key :ensure t)
(use-package spacemacs-theme :ensure t :defer t)
(use-package doom-themes :ensure t :defer t)
(use-package ace-window :ensure t)
(use-package general :ensure t)
(use-package beacon :ensure t)
;; TODO: loading our own version
(add-to-list 'load-path (expand-file-name
                         "packages/Highlight-Indentation-for-Emacs"
                         (file-name-directory load-file-name)))
(enable-auto-compilation 'highlight-indentation)
(require 'highlight-indentation)
(require 'origami)
(require 'crosshairs)
(use-package format-all :ensure t)
(use-package volatile-highlights :ensure t)
(use-package evil-goggles :ensure t)
(use-package flycheck :ensure t)
(use-package flyspell-lazy :ensure t)
(use-package rainbow-delimiters :ensure t)
(use-package vdiff :ensure t)
(use-package avy :ensure t)
(use-package smartparens :ensure t
  :config
  (require 'smartparens-config)
  :diminish smartparens-mode)
(use-package lsp-mode :ensure t)
(use-package lsp-ui :ensure t :after lsp-mode)
(use-package company :ensure t
  :config
  (require 'company-tng))
(use-package company-posframe :ensure t :after company)
;; (use-package company-box :ensure t :after company
;;  :hook (company-mode-hook . company-box-mode))
(use-package company-quickhelp :ensure t)
(use-package company-flx :ensure t :after company)
;; (use-package company-lsp :ensure t :after lsp-mode
;;  :config
;;   (setq-default company-backends
;;                 (cons #'company-lsp company-backends)))
(use-package company-ycmd :ensure t)
(add-to-list 'load-path (expand-file-name
                         "packages/company-tabnine"
                         (file-name-directory load-file-name)))
(enable-auto-compilation 'company-tabnine)
(require 'company-tabnine)
;; (use-package company-tabnine :ensure t :after company)
(use-package yasnippet :ensure t)
(use-package ycmd :ensure t)
(use-package flycheck-ycmd :ensure t)
(use-package yasnippet-snippets :ensure t)
(use-package powerline :ensure t)
(use-package powerline-evil :ensure t)
(use-package spaceline :ensure t)
(use-package spaceline-all-the-icons :ensure t :after all-the-icons spaceline)
;; (use-package ecb :ensure t
;;  :config
;;  (require 'ecb)
;;  (setq ecb-layout-name "right1"))
;; (use-package sublimity :ensure t
;;  :config
;;  (require 'sublimity-scroll
;;  (require 'sublimity-map)
;;  (require 'sublimity-attractive))
;; (use-package minimap :ensure t)
(use-package winum :ensure t)
;; (use-package symon :ensure t
;;  :config
;;  (symon-mode))
(use-package which-func :ensure t)
(use-package workgroups :ensure t)
(use-package window-layout :ensure t)
;; (use-package persp-mode :ensure t
;;  :config
;;  (persp-mode))
(use-package git-gutter :ensure t)
(use-package yascroll :ensure t)
(use-package color-identifiers-mode :ensure t)
(use-package auto-highlight-symbol :ensure t)
(use-package neotree :ensure t)
(use-package magit :ensure t)
;; (use-package page-break-lines :ensure t)
;; (use-package dashboard :ensure t :after page-break-lines)
(use-package org :ensure org-plus-contrib)
(use-package org-super-agenda :ensure t)
(use-package org-journal :ensure t)
(use-package org-pomodoro :ensure t)
(use-package org-bullets :ensure t)
(use-package org-preview-html :ensure t)
(require 'org-notify)
(require 'org-habit)
(use-package helm-org-rifle :ensure t)
(use-package outshine :ensure t)
(use-package load-relative :ensure t)
;; (use-package fancy-battery :ensure t)
(use-package rainbow-mode :ensure t)
(use-package highlight-numbers :ensure t)
(use-package highlight-operators :ensure t)
(use-package hl-todo :ensure t)
(use-package emmet-mode :ensure t)
(use-package imenu-list :ensure t :after neotree)
;; (use-package window-purpose :ensure t :after neotree imenu-list
;;  :config
;;  (purpose-mode))
(enable-auto-compilation 'companion)
(require 'companion)
;; (require 'smart-completer)
(require 'shaderlab-mode)
(use-package auctex :defer t :ensure t)
(use-package kivy-mode :ensure t)
(use-package protobuf-mode :ensure t)
(use-package cc-mode :ensure t)
(require 'cc-mode)
(use-package ess :ensure t)
(use-package csharp-mode :ensure t)
(use-package markdown-mode :ensure t)
(use-package markdown-mode+ :ensure t)
(use-package racket-mode :ensure t)
(use-package haskell-mode :ensure t)
(use-package haskell-snippets :ensure t)
(use-package rust-mode :ensure t)
(use-package csv-mode :ensure t)
(use-package sql :ensure t)
(use-package elpy :ensure t
  :init
  (elpy-enable))
(use-package tide :ensure t)
(use-package glsl-mode :ensure t)
(use-package json-mode :ensure t)
(use-package sgml-mode :ensure t)
(use-package web-mode :ensure t)
(use-package counsel-css :ensure t)
(use-package js2-mode :ensure t)
;; (use-package js2-refactor :ensure t)
(add-to-list 'load-path
             (expand-file-name "packages/org-life"
                               (file-name-directory load-file-name)))
(enable-auto-compilation 'org-life)
(require 'org-life)
(add-to-list 'load-path
             (expand-file-name "packages/org-catalyst"
                               (file-name-directory load-file-name)))
(enable-auto-compilation 'org-catalyst)
(require 'org-catalyst)
(require 'eon)
(require 'tommyx-music)
(require 'tommyx-layout)
(require 'tommyx-extensions)
(require 'tommyx-patches)


(provide 'tommyx-packages)

;;; tommyx-packages.el ends here
