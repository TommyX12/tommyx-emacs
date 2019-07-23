;; TODO: refactor status line, project, org, to here.

;; emergency key setting for debug
(global-set-key (kbd "C-M-x") 'execute-extended-command)

;; some load paths
(setq tommyx-config-path (file-name-directory load-file-name))
(add-to-list 'load-path tommyx-config-path)
(add-to-list 'load-path
             (expand-file-name "infinity-theme" tommyx-config-path))
(add-to-list 'custom-theme-load-path
             (expand-file-name "infinity-theme" tommyx-config-path))
(add-to-list 'load-path
             (expand-file-name "packages/org-life"
                               (file-name-directory load-file-name)))
(add-to-list 'load-path
             (expand-file-name "packages/org-catalyst"
                               (file-name-directory load-file-name)))

;;; startup appearance
;; theme
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)
(setq dark-theme 'infinity-dark)
(setq light-theme 'infinity-light)
(if (and (boundp 'use-light-theme) use-light-theme)
    (load-theme light-theme t)
  (load-theme dark-theme t))
;; font
(if (not (boundp 'selected-font))
    (progn
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
;; full screen
(toggle-frame-fullscreen)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;;; main

(require 'tommyx-config-framework)
(require 'tommyx-packages)
(require 'tommyx-modules)
(require 'tommyx-key-bindings)

($install-modules
 '(
   tommyx-main
   tommyx-default-major-modes
   tommyx-log-modes
   tommyx-emacs-lisp-mode
   tommyx-sgml-mode
   tommyx-emacs-internal-modes
   tommyx-emms-playlist-mode
   tommyx-neotree-mode
   tommyx-imenu-list-mode
   tommyx-dashboard-mode
   tommyx-shaderlab-mode
   tommyx-latex-mode
   tommyx-protobuf-mode
   tommyx-java-mode
   tommyx-c++-mode
   tommyx-r-mode
   tommyx-csharp-mode
   tommyx-glsl-mode
   tommyx-json-mode
   tommyx-html-mode
   tommyx-css-mode
   tommyx-racket-mode
   tommyx-haskell-mode
   tommyx-csv-mode
   tommyx-sql-mode
   tommyx-python-mode
   tommyx-javascript-mode
   tommyx-typescript-mode
   ))

(tommyx-bind-keys)

;;; extra things not yet refactored

(require 'tommyx-status-lines)
(require 'tommyx-org)
(require 'tommyx-project)


(provide 'tommyx)

;;; tommyx.el ends here
