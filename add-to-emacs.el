;;; Dependencies

(setq tommyx-config-path "~/tommyx-emacs/")
(push tommyx-config-path load-path)

(require 'tommyx-packages)
(require 'tommyx-config-framework)
(require 'tommyx-key-binding-framework)
(require 'tommyx)

;;; Extra dependencies

;;; Extra extensions

;;; Extra modules

($define-module init-module
  '(:settings
    ;; ('font-size-small 130)
    ('use-light-theme nil)
    ;; Set this to nil if you are not me.
    ('org-directory "~/notes/org")
    ((:require tommyx-music)
     ;; Set this to nil if you don't have one.
     ('emms-default-music-dir "~/data/files/music"))))

($define-module config-module
  ;; Extra configurations
  )

($define-module key-binding-module
  ;; Extra key bindings
  )

;;; Install modules

($install-modules
 (list 'init-module
       tommyx-main-modules-list
       tommyx-key-binding-modules-list
       'config-module
       'key-binding-module))
