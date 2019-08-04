(require 'tommyx-config-framework)
(require 'tommyx-extensions)
(require 'tommyx-patches)

;;; Helper definition

($define-settings-macro set-indent (width &optional highlight-width)
  `(('tab-width ,width)
    ((:require evil)
     ('evil-shift-width ,width))
    ((:require highlight-indentation)
     ('highlight-indentation-offset ,(or highlight-width width)))))

($define-settings-macro use-side-bar-background (&optional left-fringe right-fringe)
  (append
   (unless left-fringe
     '(('left-fringe-width 0)))
   (unless right-fringe
     '(('right-fringe-width 0)))
   '(((:require tommyx-extensions)
      ('face-remapping-alist :append-front
                             '(default sidebar-background)
                             '(hl-line sidebar-hl-line)
                             ;; '(fringe sidebar-fringe)
                             )))))

;;; Config definition

($define-module tommyx-appearance
  '(:on-init
    (setup-appearance)
    (toggle-frame-fullscreen)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (menu-bar-mode -1)))

($define-module tommyx-main
  '(:settings
    ;; internal
    ((:require request)
     ('request-backend (if (eq system-type 'windows-nt)
                           'url-retrieve
                         'curl)))
    ((:require alert)
     ('alert-default-style 'companion))
    ((:require hydra)
     ('lv-use-separator t)
     ('hydra-hint-display-type 'message))
    ((:require auto-package-update)
     ('auto-package-update-interval 14)
     ('auto-package-update-prompt-before-update t))

    ;; control center
    ((:require helm)
     ('helm-autoresize-max-height 40)
     ('helm-autoresize-min-height 40)
     ('helm-split-window-inside-p t)
     ('helm-full-frame nil)
     ('helm-recentf-fuzzy-match t)
     ('helm-buffers-fuzzy-matching t)
     ('helm-recentf-fuzzy-match t)
     ('helm-locate-fuzzy-match t)
     ('helm-M-x-fuzzy-match t)
     ('helm-semantic-fuzzy-match t)
     ('helm-imenu-fuzzy-match t)
     ('helm-apropos-fuzzy-match t)
     ('helm-lisp-fuzzy-completion t)
     ('helm-session-fuzzy-match t)
     ('helm-etags-fuzzy-match t)
     ('helm-mode-fuzzy-match t)
     ('helm-completion-in-region-fuzzy-match t)
     ('helm-move-to-line-cycle-in-source nil)
     ('helm-ff-file-name-history-use-recentf t)
     ('helm-follow-mode-persistent t)
     ('helm-source-names-using-follow '("Occur")))
    ((:require helm-flx)
     ('helm-flx-for-helm-find-files t)
     ('helm-flx-for-helm-locate t))
    ((:require helm-swoop)
     ('helm-swoop-split-with-multiple-windows t))
    ((:require ivy)
     ('ivy-wrap t)
     ('ivy-action-wrap t)
     ('ivy-height 20)
     ('ivy-height-alist nil)
     ;; add recent files and bookmarks to ivy-switch-buffer
     ('ivy-use-virtual-buffers t)
     ('ivy-re-builders-alist
      '((swiper . ivy--regex-ignore-order)
        (counsel-rg . ivy--regex-ignore-order)
        (swiper-multi . ivy--regex-plus)
        (t . ivy--regex-fuzzy)))
     ('ivy-initial-inputs-alist nil)
     ;; ('ivy-format-functions-alist #'$ivy-format-function-custom)
     ('ivy-count-format "%d/%d | "))
    ((:require ivy-posframe)
     ('ivy-posframe-parameters
      '((width . 50)
        (border-width . 1)
        (internal-border-width . 1)
        (undecorated . t)
        (min-width . 50)
        (refresh . 1)))
     ('ivy-posframe-height 21)
     ('ivy-posframe-border-width 2)
     ('ivy-display-function nil)
     ((:require tommyx-extensions)
      ('ivy-posframe-size-function #'$ivy-posframe-get-size)
      ('ivy-posframe-display-functions-alist
       '((swiper . $ivy-posframe-display-swiper)
         (swiper-multi . nil)
         (swiper-all . nil)
         (ivy-cs . nil)
         (counsel-ag . nil)
         (counsel-rg . nil)
         (counsel-grep . nil)
         (t . $ivy-posframe-display-at-point-horizontal)))))
    ((:require all-the-icons-ivy)
     ('all-the-icons-spacer "  ")
     ('all-the-icons-ivy-buffer-commands
      '(ivy-switch-buffer
        ivy-switch-buffer-other-window
        counsel-projectile-switch-to-buffer))
     ('all-the-icons-ivy-file-commands
      '(counsel-find-file
        counsel-file-jump
        counsel-recentf
        counsel-projectile
        counsel-projectile-find-file
        counsel-projectile-find-dir
        counsel-git)))
    ((:require ivy-rich)
     ('ivy-rich-display-transformers-list
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
           (:face font-lock-comment-face)))))))
    ((:require which-key)
     ('which-key-popup-type 'side-window)
     ('which-key-sort-order 'which-key-prefix-then-key-order-reverse)
     ('which-key-idle-delay 0.5)
     ('which-key-idle-secondary-delay 0.1)
     ;; ('which-key-allow-evil-operators t)
     ('which-key-show-operator-state-maps nil)
     ((:require tommyx-extensions)
      ('which-key-binding-filter-function #'$which-key-window-number-filter)))

    ;; compilation
    ('compilation-scroll-output 'first-error)
    ('compilation-window-height 20)
    ((:require tommyx-extensions)
     ('compilation-finish-functions :append-front
                                    #'$bury-compile-buffer-if-successful))

    ;; whitespace
    ('show-trailing-whitespace nil)
    ('whitespace-style '(tab-mark face tabs))
    ('whitespace-display-mappings '((tab-mark ?\t [?\| ?\t])))
    ('tabify-regexp "^\t* [ \t]+")

    ;; media
    ((:require emms)
     ('emms-repeat-playlist t)
     ('emms-random-playlist nil)
     ('emms-playlist-mode-center-when-go t)
     ((:require evil)
      ('evil-emacs-state-modes :delete #'emms-playlist-mode)))

    ;; appearance
    ((:require winum)
     ('winum-auto-setup-mode-line nil))
    ('frame-title-format (concat "TommyX's Emacs " emacs-version))
    ('make-pointer-invisible t)
    ('blink-matching-paren t)
    ('blink-matching-delay 0.35)
    ('display-line-numbers-width-start t)
    ('display-line-numbers-grow-only nil)
    ('indicate-buffer-boundaries t)
    ('scroll-margin 16)
    ('hl-line-sticky-flag t)
    ('window-divider-default-places 'right-only)
    ('window-divider-default-right-width 1)
    ;; ('window-divider-default-places 't)
    ;; ('window-divider-default-right-width 5)
    ;; ('window-divider-default-bottom-width 5)
    ('left-fringe-width 16)
    ('right-fringe-width 8)
    ('ring-bell-function 'ignore)
    ((:require evil)
     ('evil-insert-state-message nil)
     ('evil-visual-state-message nil)
     ('evil-replace-state-message nil)
     ('evil-search-highlight-persist-all-windows t))
    ((:require beacon)
     ('beacon-blink-when-focused nil) ; may cause problem
     ('beacon-blink-when-buffer-changes t)
     ('beacon-blink-when-window-changes t)
     ('beacon-blink-when-window-scrolls t)
     ('beacon-blink-duration 0.15)
     ('beacon-blink-delay 0.15)
     ('beacon-size 15)
     ('beacon-color "#2499ff"))
    ((:require highlight-indentation)
     ('highlight-indentation-blank-lines t)
     ('highlight-indentation-offset 4)
     ('highlight-indentation--defer-redraw t))
    ((:require evil-goggles)
     ('evil-goggles-pulse nil)
     ('evil-goggles-duration 1)
     ('evil-goggles-async-duration 1)
     ('evil-goggles-blocking-duration 1)
     ('evil-goggles--commands
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
         :advice evil-goggles--paste-advice :after t))))
    ((:require yascroll)
     ('yascroll:last-state nil)
     ('yascroll:delay-to-hide nil)
     ('yascroll:scroll-bar '(right-fringe left-fringe text-area)))
    ((:require color-identifiers-mode)
     ((:require tommyx-extensions)
      ('color-identifiers-avoid-faces
       '(color-identifiers-avoid-face-1
         color-identifiers-avoid-face-2
         color-identifiers-avoid-face-3
         font-lock-warning-face
         error)))
     ('color-identifiers-coloring-method 'sequential)
     ('color-identifiers:max-color-saturation 0.45)
     ('color-identifiers:min-color-saturation 0.2))
    ((:require auto-highlight-symbol)
     ('ahs-idle-interval 0.3)
     ('ahs-case-fold-search nil))

    ;; navigation
    ((:require avy)
     ('avy-handler-function #'$avy-handler-tommyx)
     ('avy-keys '(?w ?e ?r ?u ?i ?o ?p ?a ?s ?d ?g ?h ?j ?k ?l ?v ?n))
     ('avy-all-windows nil)
     ('avy-goto-word-0-regexp "\\(\\<\\sw\\|\n\\)"))
    
    ;; spell-check
    ('flyspell-issue-message-flag nil)
    ('flyspell-lazy-idle-seconds 2.5)
    ('flyspell-lazy-window-idle-seconds 5)

    ;; performance
    ('process-adaptive-read-buffering nil)
    ('inhibit-compacting-font-caches t)
    ('bidi-display-reordering nil)
    ('gc-cons-threshold 200000000)
    ('jit-lock-defer-time 0.1)

    ;; file tree
    ((:require neotree)
     ('neo-autorefresh nil)
     ('neo-confirm-change-root 'off-p)
     ('neo-banner-message "")
     ('neo-show-updir-line nil)
     ('neo-toggle-window-keep-p t)
     ('neo-window-width 30)
     ('neo-vc-integration '(face))
     ('neo-mode-line-type 'default) ; for performance reason
     ('neo-auto-indent-point nil)
     ('neo-buffer-name "*Files*")
     ('neo-theme (if (display-graphic-p) 'icons 'nerd))
     ('neo-show-hidden-files t))

    ;; outline tree
    ((:require imenu-list)
     ('imenu-list-position 'right)
     ('imenu-list-size 30)
     ('imenu-list-idle-update-delay 1)
     ('imenu-list-buffer-name "*Outline*"))

    ;; editing
    ((:macro set-indent) 4)
    ('indent-tabs-mode nil)
    ('backward-delete-char-untabify-method nil)
    ('save-interprogram-paste-before-kill t)
    ('undo-limit 1000000)
    ('undo-strong-limit 1000000)
    ((:require evil)
     ('evil-ex-substitute-global t)
     ('evil-move-cursor-back nil)
     ('evil-move-beyond-eol t)
     ('evil-symbol-word-search t)
     ('evil-magic nil)
     ('evil-allow-remove-spaces t)
     ((:require evil-collection)
      ('evil-collection-mode-list :delete 'neotree 'company))
     ((:require evil-exchange)
      ('evil-exchange-key (kbd "x"))
      ('evil-exchange-cancel-key (kbd ",x"))))
    ((:require origami)
     ('origami-parser-alist :append-front '(python-mode . origami-indent-parser)))
    ((:require smartparens)
     ('sp-show-pair-from-inside t)
     ('smartparens-strict-mode nil)
     ('sp-cancel-autoskip-on-backward-movement nil))
    ((:require cc-mode)
     ('c-basic-offset 4)
     ('c-default-style '((other . "linux"))))

    ;; dashboard
    ;; ((:require dashboard)
    ;;  ('dashboard-items '((recents  . 5)
    ;;                      (projects . 5)
    ;;                      (bookmarks . 5)))
    ;;  ('dashboard-banner-length 250)
    ;;  ('dashboard-banner-logo-title
    ;;   (concat "Emacs " emacs-version
    ;;           " (" system-configuration ")"))
    ;;  ('dashboard-startup-banner
    ;;   (expand-file-name "logo.png" tommyx-config-path)))

    ;; snippets
    ((:require yasnippet)
     ('yas-snippet-dirs :append-front
                        (expand-file-name "snippets" tommyx-config-path)))
    ((:require emmet-mode)
     ('emmet-move-cursor-after-expanding t)
     ('emmet-move-cursor-between-quotes t)
     ('emmet-indentation 2))

    ;; linter
    ((:require flycheck)
     ('flycheck-idle-change-delay 3)
     ('flycheck-check-syntax-automatically '(idle-change save mode-enabled)))

    ;; auto-completion
    ((:require company)
     ((:require company-tng)
      ('company-frontends
       '(company-tng-frontend
         company-pseudo-tooltip-frontend
         company-preview-frontend
         company-echo-metadata-frontend)))
     ('company-idle-delay 0)
     ('company-tooltip-align-annotations t)
     ('company-selection-wrap-around t)
     ('company-show-numbers t)
     ('company-quickhelp-delay nil) ; we will manually trigger the help
     ('company-require-match 'never)
     ('company-dabbrev-downcase nil)
     ('company-dabbrev-ignore-case nil)
     ('company-dabbrev-other-buffers t))
    ((:require company-flx)
     ('company-flx-limit 256))
    ((:require company-tabnine)
     ('company-backends :append-front #'company-tabnine))
    ((:require ycmd)
     ('ycmd-global-config (expand-file-name
                           "third_party/ycmd/.ycm_extra_conf.py"
                           tommyx-config-path))
     ('ycmd-server-python-command :default "python")
     ('ycmd-server-command
      `(,ycmd-server-python-command
        "-u" ,(expand-file-name "third_party/ycmd/ycmd/"
                                tommyx-config-path)))
     ('ycmd-file-type-map '(java-mode "java")))
    ((:require company-ycmd)
     ('company-ycmd-request-sync-timeout 0))

    ;; layout
    ((:require evil)
     ('evil-split-window-below t)
     ('evil-vsplit-window-right t)
     ('evil-auto-balance-windows nil))

    ;; version control
    ((:require git-gutter)
     ('git-gutter:window-width 1)
     ('git-gutter:update-interval 5)
     ('git-gutter:modified-sign "|")
     ('git-gutter:added-sign "|")
     ('git-gutter:deleted-sign "-")
     ('git-gutter:visual-line nil))
    ((:require magit)
     ('magit-display-buffer-function
      #'magit-display-buffer-fullframe-status-v1))

    ;; backup
    ('make-backup-files nil)

    ;; encoding
    ('utf-translate-cjk-mode nil)
    ('locale-coding-system 'utf-8)

    ;; debug
    ('profiler-max-stack-depth 64)

    ;; file navigation
    ('recentf-max-menu-items 500)
    ('recentf-max-saved-items 500)

    ;; emacs server
    ('server-window 'pop-to-buffer)

    ;; tramp
    ('tramp-default-method "ssh")

    ;; shell
    ('explicit-shell-file-name
     (cond
      ((eq system-type 'darwin)
       "/usr/local/bin/zsh")
      ((not (eq system-type 'windows-nt))
       "/usr/bin/zsh")))

    ;; correct modifier keys
    ('mac-option-modifier 'super)
    ('mac-command-modifier 'meta)

    ;; key bindings
    ('tommyx-key-chord-timeout 1.0)

    ;; typebreak
    ((:require tommyx-extensions)
     ('type-break-query-function '$type-break-my-query-function))
    ('type-break-interval 1800)
    ('type-break-good-rest-interval 300)
    ('type-break-demo-boring-stats t)
    ('type-break-keystroke-threshold '(nil . nil))
    ('type-break-warning-repeat 0)
    ('type-break-demo-functions '(type-break-demo-boring))
    ('type-break-health-quotes
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
       "Decrease blood pressure and blood sugar")))

  '(:minor-modes
    ;; internal
    ((:require winum)
     (winum-mode 1))

    ;; appearance
    (window-divider-mode 1)
    (eager-redisplay-mode 1)
    (blink-cursor-mode -1)
    ;; ((:require evil-search-highlight-persist)
    ;;  (global-evil-search-highlight-persist 1))
    ((:require volatile-highlights)
     (volatile-highlights-mode 1))
    ((:require evil-goggles)
     (evil-goggles-mode 1))
    ((:require yascroll)
     (global-yascroll-bar-mode 1))
    ((:require color-identifiers-mode)
     (global-color-identifiers-mode 1))
    ((:require hl-todo)
     (global-hl-todo-mode 1))

    ;; version control
    ((:require git-gutter)
     (global-git-gutter-mode 1))

    ;; documentation
    (global-eldoc-mode 1)
    ((:require which-func)
     (which-function-mode 1))

    ;; layout
    (winner-mode 1)

    ;; type break
    ;; TODO: disabled
    ;; (type-break-mode 1)
    ;; (type-break-query-mode 1)

    ;; snippets
    ((:require yasnippet)
     (yas-global-mode 1))

    ;; linter
    ((:require flycheck)
     (global-flycheck-mode 1))

    ;; spell-check
    ((:require flyspell-lazy)
     (flyspell-lazy-mode 1))

    ;; auto-completion
    ((:require company)
     (global-company-mode 1))
    ((:require company-flx)
     (company-flx-mode 1))

    ;; control center
    ((:require helm)
     (helm-autoresize-mode 1))
    ((:require helm-flx)
     (helm-flx-mode 1))
    ((:require helm-descbinds)
     (helm-descbinds-mode 1))
    ((:require ivy-rich)
     (ivy-rich-mode 1))
    ((:require which-key)
     (which-key-mode 1))
    ((:require ivy)
     (ivy-mode 1)
     (counsel-mode 1))

    ;; editing
    ((:require evil)
     (evil-mode 1))
    ((:require undo-tree)
     (global-undo-tree-mode -1))
    (global-auto-revert-mode 1)
    ((:require evil-visualstar)
     (global-evil-visualstar-mode 1))
    ((:require evil-surround)
     (global-evil-surround-mode 1))
    ((:require smartparens)
     (smartparens-global-mode 1)
     (show-smartparens-global-mode 1)))

  '(:on-init ;; in global context, this runs at config time
    ;; encoding
    ((:require tommyx-extensions)
     ($use-utf8-encoding))

    ;; emacs server
    ((:require tommyx-extensions)
     ($start-emacs-server))

    ;; appearance
    (all-the-icons-ivy-setup)
    ((:require volatile-highlights undo-tree)
     ((:require tommyx-extensions)
      ($volatile-highlights-undo-tree-setup)))
    ((:require evil smartparens)
     ($remove-parens-overlay-on-insert-exit))

    ;; media
    ((:require emms)
     (emms-all)
     (emms-default-players)
     (when (bound-and-true-p emms-default-music-dir)
       (emms-add-directory-tree emms-default-music-dir))
     ((:require tommyx-music)
      (emms-setup-show-progress-on-seek)))

    ;; linter
    ((:require flycheck ycmd tommyx-extensions)
     ($setup-ycmd-flycheck))

    ;; documentation
    ((:require ycmd tommyx-extensions)
     ($setup-ycmd-eldoc))

    ;; dashboard
    ;; TODO: has bug
    ;;((:require dashboard)
    ;; (dashboard-setup-startup-hook))

    ;; auto-completion

    ;; editing
    ((:require evil-collection)
     (evil-collection-init))
    ((:require evil-exchange)
     (evil-exchange-install))
    ((:require smartparens)
     ((:require tommyx-extensions)
      ($setup-smartparens-expansion)))
    ((:require evil-args)
     ($setup-evil-args))
    ((:require evil)
     ($disable-hl-line-in-insert-and-visual-mode)
     ($disable-trailing-whitespace-in-insert-mode)
     ($disable-yascroll-in-insert-mode)
     ;; ($defer-hl-indentation-in-insert-mode)
     ($set-jump-on-insert-mode))

    ;; key bindings
    ((:require general)
     (general-evil-setup)
     (general-auto-unbind-keys))

    ;; control center
    ((:require helm-projectile)
     (helm-projectile-on))
    ((:require helm-describe-modes)
     ($setup-helm-describe-modes))
    ((:require ivy-posframe)
     (ivy-posframe-enable)))

  '(:after-init
    ;; appearance
    ((:require tommyx-extensions)
     ($improve-fringe-bitmaps))
    ;; performance
    ((:require tommyx-extensions)
     ($remove-vc-hooks))
    ((:require tommyx-extensions imenu-list neotree)
     ;; We use eays-layout to manage these
     ($start-imenu-list-and-neotree))
    ((:require companion)
     (companion-compile)
     (companion-open))
    ((:require auto-package-update)
     (auto-package-update-maybe)))

  '(:on-idle
    ((:require evil yascroll tommyx-extensions)
     (0.5 ($show-yascroll-if-not-in-insert-mode)))
    (5 (garbage-collect)))

  ;; TODO: disabled
  ;; '(:on-interval
  ;;   ((:require tommyx-extensions)
  ;;    (120 ($type-break-schedule-check))))

  '(:on-focus-out
    ;; performance
    (garbage-collect))

  '(:patches
    ((:require tommyx-patches)
     ((:require ivy-posframe)
      ;; TODO: bug
      ;; ($ivy-posframe-patch)
      )
     ((:require ivy)
      ($ivy-format-function-patch))
     ((:require auto-highlight-symbol)
      ($ahs-persistent-highlight-patch)
      ($ahs-bug-patch))
     ((:require company)
      ($company-preview-patch)
      ($company-tng-frontend-patch)
      ($company-echo-metadata-frontend-patch)
      ($company-general-compatibility-patch)
      ((:require yasnippet)
       ($company-yasnippet-compatibility-patch)))
     ((:require company-posframe)
      ($company-posframe-patch))
     ((:require company-tabnine)
      ($company-tabnine-patch))
     ((:require imenu-list)
      ($imenu-list-appearance-patch)
      ($imenu-list-mode-line-patch))
     ((:require all-the-icons)
      ($all-the-icons-dir-patch)))))

($define-module tommyx-default-major-modes
  '((:mode-local prog-mode text-mode)

    (:settings
     ('show-trailing-whitespace t)
     ('truncate-lines nil))

    (:minor-modes
     (toggle-word-wrap 1)
     (auto-save-mode -1)
     (hl-line-mode 1)
     ((:require highlight-indentation)
      (highlight-indentation-mode 1))
     ((:require rainbow-delimiters)
      (rainbow-delimiters-mode 1))))

  '((:mode-local text-mode)

    (:minor-modes
     (flyspell-mode 1)))

  '((:mode-local prog-mode)

    (:minor-modes
     (flyspell-prog-mode)
     ((:require color-identifiers-mode)
      (color-identifiers-mode 1))
     ((:require auto-highlight-symbol)
      (auto-highlight-symbol-mode 1))
     ((:require highlight-numbers)
      (highlight-numbers-mode 1)))

    (:on-init
     ((:require tommyx-extensions)
      ($extra-word-char '(?_))))))

($define-module tommyx-log-modes
  '((:mode-local change-log-mode-hook)

    (:minor-modes
     (flyspell-mode -1)))

  '((:mode-local log-edit-mode-hook)

    (:minor-modes
     (flyspell-mode -1))))

($define-module tommyx-emacs-lisp-mode
  '((:mode-local emacs-lisp-mode)

    (:settings
     ((:macro set-indent) 2 4)
     ((:require company-tabnine)
      ('company-tabnine-auto-fallback nil)))

    (:minor-modes
     ((:require highlight-function-calls)
      (highlight-function-calls-mode 1)))

    (:on-init
     ((:require tommyx-extensions)
      ($extra-word-char '(?-)))))

  '((:mode-local lisp-interaction-mode)

    (:settings
     ((:require git-gutter)
      ('git-gutter:disabled-modes :append-front 'lisp-interaction-mode))
     ((:macro use-side-bar-background)))

    (:minor-modes
     (yascroll-bar-mode -1))))

($define-module tommyx-sh-mode
  '((:mode-local sh-mode)

    (:settings
     ((:require company-tabnine)
      ('company-tabnine-auto-fallback nil)))))

($define-module tommyx-sgml-mode
  '((:mode-local nxml-mode)

    (:on-init
     ((:require tommyx-extensions)
      ($extra-word-char '(?- ?_)))))

  '((:mode-local sgml-mode)

    (:minor-modes
     (toggle-word-wrap 1)
     ((:require emmet-mode)
      (emmet-mode 1)))))

($define-module tommyx-emacs-internal-modes
  '((:mode-local buffer-menu-mode)

    (:settings
     ('use-line-nav t))

    (:minor-modes
     (hl-line-mode 1)))

  '((:mode-local profiler-report-mode)

    (:settings
     ('use-line-nav t))

    (:minor-modes
     (hl-line-mode 1))))

($define-module tommyx-emms-playlist-mode
  '((:mode-local emms-playlist-mode-hook)

    (:settings
     ('use-line-nav t))

    (:minor-modes
     (hl-line-mode 1))))

($define-module tommyx-neotree-mode
  '((:mode-local neotree-mode)

    (:settings
     ('use-line-nav t)
     ((:require git-gutter)
      ('git-gutter:disabled-modes :append-front 'neotree-mode))
     ((:macro use-side-bar-background))
     ((:macro set-indent) 2))

    (:minor-modes
     (hl-line-mode 1)
     ((:require yascroll)
      (yascroll-bar-mode -1))
     ((:require highlight-indentation)
      (highlight-indentation-mode -1)))))

($define-module tommyx-imenu-list-mode
  '((:mode-local imenu-list-major-mode)

    (:settings
     ('use-line-nav t)
     ((:require git-gutter)
      ('git-gutter:disabled-modes :append-front 'imenu-list-major-mode))
     ((:macro use-side-bar-background))
     ((:macro set-indent) 2))

    (:minor-modes
     (hl-line-mode 1)
     ;; (whitespace-mode 1)
     ((:require yascroll)
      (yascroll-bar-mode -1))
     ((:require highlight-indentation)
      (highlight-indentation-mode 1)))))

($define-module tommyx-dashboard-mode
  '((:mode-local dashboard-mode)

    (:settings
     ('use-line-nav t))

    (:minor-modes
     (hl-line-mode 1))))

($define-module tommyx-shaderlab-mode
  '((:mode-local shaderlab-mode)

    (:minor-modes
     ((:require auto-highlight-symbol)
      (auto-highlight-symbol-mode 1)))))

($define-module tommyx-latex-mode
  '((:mode-local latex-mode)

    (:settings
     ((:macro set-indent) 2 4))))

($define-module tommyx-protobuf-mode
  '((:mode-local protobuf-mode)

    (:settings
     ((:macro set-indent) 2 4))

    (:minor-modes
     ((:require auto-highlight-symbol)
      (auto-highlight-symbol-mode 1)
      (highlight-indentation-mode 1)))))

($define-module tommyx-java-mode
  '(:settings
    ((:require cc-mode)
     ('c-default-style :append-front '(java-mode . "java"))))

  '((:mode-local java-mode)

    (:settings
     ((:macro set-indent) 2))))

($define-module tommyx-c++-mode
  '((:mode-local c++-mode)

    (:settings
     ((:require company ycmd company-tabnine)
      ('company-backends :ensure-front 'company-tabnine 'company-ycmd))
     ((:macro set-indent) 4))

    (:minor-modes
     ((:require ycmd)
      (ycmd-mode 1)))))

($define-module tommyx-r-mode
  '((:mode-local ess-r-mode)

    (:settings
     ((:require company company-tabnine)
      ('company-backends :ensure-front 'company-tabnine))
     ((:macro set-indent) 2))))

($define-module tommyx-csharp-mode
  '((:mode-local csharp-mode)

    (:settings
     ((:require company ycmd company-tabnine)
      ('company-backends :ensure-front 'company-tabnine 'company-ycmd)))

    (:minor-modes
     (ycmd-mode 1))

    (:on-init
     ((:require tommyx-extensions color-identifiers-mode)
      ($setup-color-identifiers-parser 'c 'csharp-mode)))))

($define-module tommyx-glsl-mode
  '((:mode-local glsl-mode)

    (:minor-modes
     ((:require auto-highlight-symbol)
      (auto-highlight-symbol-mode 1)))

    (:on-init
     ((:require tommyx-extensions color-identifiers-mode)
      ($setup-color-identifiers-parser 'c 'csharp-mode)))))

($define-module tommyx-json-mode
  '((:mode-local json-mode)

    (:minor-modes
     ((:require highlight-numbers)
      (highlight-numbers-mode -1)))

    (:settings
     ((:macro set-indent) 2 4)
     ((:require json-mode)
      ('json-reformat:indent-width 2)
      ('js-indent-level 2)))

    (:on-before-save
     (let ((pos (point)))
       (with-demoted-errors "Error: %s"
         (json-pretty-print-buffer))
       (goto-char pos)))))

($define-module tommyx-html-mode
  '(:settings
    ('web-mode-enable-auto-expanding t)
    ('web-mode-markup-indent-offset 2)
    ('auto-mode-alist
     :append-front
     '("\\.phtml\\'" . web-mode)
     '("\\.tpl\\.php\\'" . web-mode)
     '("\\.[agj]sp\\'" . web-mode)
     '("\\.as[cp]x\\'" . web-mode)
     '("\\.erb\\'" . web-mode)
     '("\\.mustache\\'" . web-mode)
     '("\\.hbs\\'" . web-mode)
     '("\\.djhtml\\'" . web-mode)
     '("\\.html?\\'" . web-mode)
     '("\\.xml?\\'" . web-mode)
     '("\\.vue\\'" . web-mode)))

  '(:on-init
    ((:require flycheck)
     (flycheck-add-mode 'html-tidy 'web-mode)))

  '((:mode-local html-mode web-mode)

    (:settings
     ((:macro set-indent) 2 4))

    (:minor-modes
     ((:require emmet-mode)
      (emmet-mode 1))
     ((:require auto-highlight-symbol)
      (auto-highlight-symbol-mode 1)))

    (:on-init
     ((:require tommyx-extensions)
      ($extra-word-char '(?- ?_))))))

($define-module tommyx-css-mode
  '((:mode-local css-mode scss-mode)

    (:settings
     ((:macro set-indent) 2)
     ('web-mode-css-indent-offset 2)
     ('css-indent-offset 2))

    (:minor-modes
     ((:require emmet-mode)
      (emmet-mode 1))
     ((:require auto-highlight-symbol)
      (auto-highlight-symbol-mode 1)))

    (:on-init
     ((:require counsel-css)
      (counsel-css-imenu-setup))
     ((:require tommyx-extensions)
      ($extra-word-char '(?- ?_))))))

($define-module tommyx-racket-mode
  '((:mode-local racket-mode)

    (:minor-modes
     ((:require auto-highlight-symbol)
      (auto-highlight-symbol-mode 1)))

    (:on-init
     ($extra-word-char '(?-)))))

($define-module tommyx-haskell-mode
  '((:mode-local haskell-mode)

    (:settings
     ((:macro set-indent) 4)
     ((:require haskell-mode)
      ('haskell-indentation-starter-offset tab-width)
      ('haskell-indentation-left-offset tab-width)
      ('haskell-indentation-layout-offset tab-width)))

    (:minor-modes
     ((:require flycheck)
      (flyspell-mode -1))
     ((:require auto-highlight-symbol)
      (auto-highlight-symbol-mode 1)))

    (:on-init
     ($extra-word-char '(?-)))))

($define-module tommyx-csv-mode
  '(:settings
    ((:require csv-mode)
     ('csv-align-style 'auto)
     ('csv-invisibility-default nil)))

  '((:mode-local csv-mode)

    (:minor-modes
     (toggle-truncate-lines 1))

    (:on-init
     (call-interactively #'csv-align-fields))))

($define-module tommyx-sql-mode
  '((:mode-local sql-mode)

    (:minor-modes
     ((:require auto-highlight-symbol)
      (auto-highlight-symbol-mode 1)))

    (:on-init
     ($extra-word-char '(?-)))))

($define-module tommyx-python-mode
  '(:settings
    ('python-auto-format-code t))

  '((:mode-local python-mode)

    (:settings
     ((:macro set-indent) 4)
     ('python-indent 4)
     ('python-indent-offset 4)
     ((:require yasnippet)
      ('yas-indent-line 'auto))
     ((:require elpy)
      ('elpy-rpc-timeout 2.5)))

    (:minor-modes
     ;; Note: elpy is automatically enabled.
     ((:require flycheck)
      (flycheck-mode -1)))

    (:on-before-save
     ((:require elpy)
      (python-format-code)))

    ((:mode-local elpy-mode)

     (:settings
      ((:require company)
       ('company-idle-delay 0)
       ((:require company-tabnine)
        ('company-backends :ensure-front 'company-tabnine)))))))

($define-module tommyx-javascript-mode
  '(:settings
    ('auto-mode-alist :append-front
                      '("\\.js\\'" . js2-mode))
    ((:require js2-mode)
     ('js2-strict-missing-semi-warning nil)))

  '((:mode-local js2-mode)

    (:settings
     ((:require tide)
      ('typescript-indent-level 4))
     ('js-indent-level 4)
     ((:require company company-tabnine tide)
      ('company-backends :ensure-front 'company-tabnine 'company-tide)))

    (:minor-modes
     ((:require auto-highlight-symbol)
      (auto-highlight-symbol-mode 1))
     ((:require tide)
      (tide-hl-identifier-mode 1)))

    (:on-init
     ((:require tide)
      (tide-setup)))))

($define-module tommyx-typescript-mode
  '((:mode-local typescript-mode)

    (:settings
     ((:macro set-indent) 2 4)
     ((:require tide)
      ('typescript-indent-level 2))
     ('js-indent-level 2)
     ((:require company company-tabnine tide)
      ('company-backends :ensure-front 'company-tabnine 'company-tide)))

    (:minor-modes
     ((:require auto-highlight-symbol)
      (auto-highlight-symbol-mode 1))
     ((:require tide)
      (tide-hl-identifier-mode 1)))

    (:on-init
     ((:require tommyx-extensions color-identifiers-mode)
      ($setup-color-identifiers-parser 'js 'typescript-mode))
     ((:require tide)
      (tide-setup)))))

($define-module tommyx-term-mode
  '((:mode-local term-mode)

    (:settings
     ((:require git-gutter)
      ('git-gutter:disabled-modes :append-front 'term-mode))
     ('scroll-margin 0)
     ((:macro use-side-bar-background)))

    (:minor-modes
     (yascroll-bar-mode -1))))

($define-module tommyx-compilation-mode
  '((:mode-local compilation-mode)

    (:settings
     ((:require git-gutter)
      ('git-gutter:disabled-modes :append-front 'compilation-mode))
     ((:macro use-side-bar-background)))

    (:minor-modes
     (yascroll-bar-mode -1))))

($define-module tommyx-help-mode
  '((:mode-local help-mode)

    (:settings
     ((:require git-gutter)
      ('git-gutter:disabled-modes :append-front 'help-mode))
     ((:macro use-side-bar-background)))

    (:minor-modes
     (yascroll-bar-mode -1))))

($define-module tommyx-message-mode
  '((:mode-local message-mode)

    (:settings
     ((:require git-gutter)
      ('git-gutter:disabled-modes :append-front 'message-mode))
     ((:macro use-side-bar-background)))

    (:minor-modes
     (yascroll-bar-mode -1))))

($define-module tommyx-org-mode
  '(:settings
    ((:require org)
     ((:require git-gutter)
      ('git-gutter:disabled-modes :append-front 'org-mode))
     ('org-startup-indented t)
     ('org-startup-folded nil)
     ('org-log-done 'time)
     ('org-log-into-drawer t)
     ('org-clock-into-drawer t)
     ('org-list-allow-alphabetical t)
     ('org-tags-column 0)
     ('org-descriptive-links t)
     ('org-link-file-path-type 'relative)
     ('org-id-link-to-org-use-id nil)
     ('org-highlight-latex-and-related '(latex))
     ('org-startup-with-latex-preview t)
     ('org-format-latex-options
      `(
        :foreground default
        :background default
        :scale ,(if (eq system-type 'darwin)
                    1.6
                  1.5)
        :html-foreground "Black"
        :html-background "Transparent"
        :html-scale 1.0
        :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
     ('org-preview-latex-default-process 'dvipng)
     ('org-format-latex-header-default
      org-format-latex-header)
     ('org-format-latex-header
      (concat
       org-format-latex-header-default
       "\n\\DeclareMathOperator*{\\argmax}{arg\\,max}"
       "\n\\DeclareMathOperator*{\\argmin}{arg\\,min}"))
     ('org-clock-report-include-clocking-task t)
     ('org-clock-persist t)
     ('org-clock-history-length 25)
     ('org-enforce-todo-dependencies t)
     ('org-enforce-todo-checkbox-dependencies t)
     ('org-image-actual-width nil)
     ('org-M-RET-may-split-line nil)
     ('org-fontify-done-headline t)
     ('org-fontify-whole-heading-line t)
     ('org-imenu-depth 4)
     ((:require org-agenda)
      ((:require org-super-agenda)
       ('org-super-agenda-fontify-whole-header-line t)
       ('org-super-agenda-header-map (make-sparse-keymap)))
      ('org-agenda-dim-blocked-tasks t)
      ('org-agenda-window-setup 'only-window)
      ('org-agenda-start-with-clockreport-mode t)
      ('org-agenda-start-with-log-mode t)
      ('org-agenda-log-mode-items '(closed clock state))
      ('org-agenda-restore-windows-after-quit t)
      ('org-agenda-use-tag-inheritance t)
      ('org-agenda-sticky t)
      ('org-agenda-span 'day)
      ('org-agenda-skip-deadline-if-done t)
      ('org-agenda-skip-scheduled-if-done t)
      ('org-agenda-skip-scheduled-if-done t)
      ('org-agenda-skip-scheduled-if-deadline-is-shown t)
      ('org-agenda-move-date-from-past-immediately-to-today t)
      ('org-agenda-time-grid
       '((daily today)
         (800 1000 1200 1400 1600 1800 2000)
         "......" "----------------"))
      ('org-agenda-entry-text-leaders "    > ")
      ('org-agenda-prefix-format
       '((agenda  . " %i %?-12t %(org-agenda-special-prefix) %-12:c")
         ;; (agenda  . " %i %?-12t % s %-12:c")
         (todo  . " %i %-12:c")
         (tags  . " %i %-12:c")
         (search . " %i %-12:c")))
      ('org-agenda-timerange-leaders '("" "(%d/%d): "))
      ('org-agenda-scheduled-leaders '("[S]        : "
                                       "[S]     -%2d: "))
      ('org-agenda-deadline-leaders '("@0  " "@%-3d" "@-%-2d"))
      ('org-agenda-inactive-leader "[")
      ('org-agenda-entry-text-exclude-regexps
       '("^- State.*\n" "^[ \t]*\n")))
     ((:require org-habit)
      ('org-modules :ensure-front 'org-habit))
     ((:require org-bullets)
      ('org-bullets-bullet-list '("●")))
     ((:require counsel)
      ('counsel-org-headline-display-style 'path)
      ('counsel-org-headline-path-separator "/"))))

  '(:minor-modes
    (org-super-agenda-mode 1))

  '(:on-init
    ((:require org)
     (org-clock-persistence-insinuate)
     ((:require tommyx-extensions)
      ($ask-for-clock-out-on-quit)
      ((:require smartparens)
       ($setup-org-mode-local-pairs)))
     ((:require evil)
      (evil-set-initial-state 'org-agenda-mode 'motion))
     ((:require companion org-notify)
      (org-notify-add 'default
                      '(:time "1h" :actions nil :period "2m" :duration 60))
      (companion-notif-create-stream 'org-notify 120))))

  '(:patches
    ((:require tommyx-patches org smartparens)
     ($org-mode-angular-brackets-patch)
     ($org-mode-heading-coloring-patch)))

  '((:when (bound-and-true-p org-directory))

    (:settings
     ((:require org-life)
      ('org-life-config-file-path
       (expand-file-name "org-life-config.org" org-directory))
      ((:require org-agenda)
       ('org-agenda-custom-commands
        :append-front '("x" "org-life agenda"
                        ((org-life-agenda ""
                                          ()))))))
     ((:require org-catalyst)
      ('org-catalyst-save-path
       (expand-file-name "org-catalyst" org-directory))))

    (:minor-modes
     ((:require org-catalyst)
      (org-catalyst-auto-save-mode 1)))

    (:on-init
     ((:require tommyx-extensions)
      (update-all-org-directory-files)
      ($load-external-org-config-if-exist))
     ((:require org-catalyst)
      (org-catalyst-setup-evil-status-bindings)))

    ((:mode-local org-catalyst-mode)

     (:settings
      ('use-line-nav t))

     (:minor-modes
      (hl-line-mode -1))))

  '((:mode-local org-mode)

    (:settings
     ((:macro set-indent) 2 4)
     ((:require company-tabnine)
      ('company-tabnine-auto-fallback nil)))

    (:minor-modes
     ((:require org-bullets)
      (org-bullets-mode 1))))

  '((:mode-local org-agenda-mode)

    (:settings
     ('use-line-nav t))

    (:minor-modes
     (hl-line-mode 1))))

(provide 'tommyx-main-modules)

;;; tommyx-main-modules.el ends here

