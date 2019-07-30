
;;; before-load configurations

(setq tommyx-config-path "~/tommyx-emacs/")

;; flyspell (for windows)
;; (setq ispell-program-name (expand-file-name "third_party/hunspell/bin/hunspell.exe" tommyx-config-path))
;; (setq ispell-really-hunspell t)

;; org
(setq org-directory "~/notes/org") ; set this to nil if you are not me.
(setq use-light-theme nil) ; set to t if you want to startup with light theme.

;; ycmd
;; (setq ycmd-server-python-command "python3")

;; emms
;; (setq mpg123-path "D:/data/projects/new/Tools/tommyx-emacs/third_party/mpg123/mpg123.exe")
(setq emms-default-music-dir "~/data/files/music") ; set this to nil if you don't have one.

;; others
;; (setq font-size-small 130)
;; (setq font-size-big 150)

;;; load tommyx configuration

(add-to-list 'load-path tommyx-config-path)
(require 'tommyx)

;;; after-load configurations

;; flyspell
;; (add-to-list 'ispell-extra-args "--sug-mode=ultra")

;; curl (windows)
;; (setq request-curl (expand-file-name "third_party/curl/curl.exe" tommyx-config-path))

;; org
(setq org-agenda-files (append org-agenda-files '()))
