;; emergency key setting for debug
(global-set-key (kbd "C-M-x") 'execute-extended-command)


;;; add directories to load-path

(setq tommyx-config-path (file-name-directory load-file-name))
;; TODO make sure these overrides package archive install directories
(add-to-list 'load-path tommyx-config-path)
(add-to-list 'load-path
             (expand-file-name "infinity-theme" tommyx-config-path))
(add-to-list 'custom-theme-load-path
             (expand-file-name "infinity-theme" tommyx-config-path))
(add-to-list 'load-path
             (expand-file-name "packages" tommyx-config-path))
(add-to-list 'load-path
             (expand-file-name "packages/company-tabnine" tommyx-config-path))
(add-to-list 'load-path
	           (expand-file-name "packages/org-life" (file-name-directory load-file-name)))
(add-to-list 'load-path
	           (expand-file-name "packages/org-catalyst" (file-name-directory load-file-name)))


;;; startup appearence

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

;; full screen automatically

(toggle-frame-fullscreen)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)


;;; initialize packages

(setq load-prefer-newer t)

(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(package-refresh-contents)
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


;;; main config

(require 'tommyx-main)


;;; status line config

(require 'tommyx-status-lines)


;;; org config

(require 'tommyx-org)


;;; project + workspace config

(require 'tommyx-project)


;;; finalize

;; Note: remove advice for auto compile
(advice-remove #'auto-compile-on-load #'auto-compile-on-load-force)


(provide 'tommyx)

;;; tommyx.el ends here
