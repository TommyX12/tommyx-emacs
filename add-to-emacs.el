
;; pre-load configurations
(setq org-directory "~/notes/agenda") ; remove this if you are not me.
(setq use-light-theme nil) ; set to t if you want to startup with light theme.

(load "~/tommyx-emacs/tommyx.el")

;; post-load configurations
; add things to org agenda files
(setq org-agenda-files (append org-agenda-files '()))
