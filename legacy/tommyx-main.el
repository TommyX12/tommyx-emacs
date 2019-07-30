;;; imports

(enable-auto-compilation 'tommyx-main-def)
(require 'tommyx-main-def)


;;; general settings before package load

;; add _ as word syntax
(add-hook 'prog-mode-hook (lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'emacs-lisp-mode-hook (lambda () (modify-syntax-entry ?- "w")))
(add-hook
 'nxml-mode-hook
 (lambda ()
   (modify-syntax-entry ?- "w")
   (modify-syntax-entry ?_ "w")))

;; compilation
(setq compilation-scroll-output 'first-error)
(setq compilation-window-height 20)
(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings."
  (when (and
         (buffer-live-p buffer)
         (string-match "compilation" (buffer-name buffer))
         (string-match "finished" string)
         (not
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "warning" nil t))))
    (run-with-timer 1 nil
                    (lambda (buf window)
                      (with-selected-window window
                        (quit-window)))
                    buffer
                    (get-buffer-window buffer))))
(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

;; show trailing whitespace by default
(setq-default show-trailing-whitespace nil)
(add-hook 'prog-mode-hook (lambda ()
                            (setq-local show-trailing-whitespace t)))

;; disable trailing whitespace on evil insert mode

;; indentation guide using whitespace mode
(setq whitespace-style '(
                         tab-mark face tabs
                         ))
(setq whitespace-display-mappings '(
                                    (tab-mark ?\t [?\| ?\t])
                                    ))
;; (global-whitespace-mode 1)

;; tabify only leading whitespace
(setq tabify-regexp "^\t* [ \t]+")

;; mouse avoidance (move to top right corner)
(setq make-pointer-invisible t)
;; (setq mouse-avoidance-banish-position
;;  '((frame-or-window . frame)
;;  (side . right)
;;  (side-pos . -5)
;;  (top-or-bottom . top)
;;  (top-or-bottom-pos . -5)))
;; (mouse-avoidance-mode 'banish)
;; (mouse-avoidance-mode 'none)

;; indent settings
(setq default-indent-tabs-mode nil)
(setq-default indent-tabs-mode default-indent-tabs-mode) ; use tabs instead of space
(setq-default tab-width 4)
(setq-default evil-shift-width tab-width)
(setq-default backward-delete-char-untabify-method 'nil)
(add-hook 'prog-mode-hook (lambda () (setq evil-shift-width tab-width)))
(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   (setq-local indent-tabs-mode nil)
   (setq-local tab-width 2)
   (setq-local evil-shift-width tab-width)))

;; flyspell
(setq flyspell-issue-message-flag nil)

;; tramp
(setq tramp-default-method "ssh")

;; blink matching parens
(setq blink-matching-paren t)
(setq blink-matching-delay 0.35)

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
;; flyspell
(dolist (hook '(prog-mode-hook))
  (add-hook hook (lambda () (flyspell-prog-mode))))
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;; attempt to improve font-lock performance
;; (setq jit-lock-defer-time 0)

;; attempt to improve subprocess performance
(setq process-adaptive-read-buffering nil)

;; attempt to improve font performance
(setq inhibit-compacting-font-caches t)

;; attempt to improve long line performance
(setq-default bidi-display-reordering nil)

;; wrap lines
(add-hook 'prog-mode-hook (lambda () (setq truncate-lines nil)))
(add-hook 'text-mode-hook (lambda () (setq truncate-lines nil)))

;; subword motion
;; (global-subword-mode t) ; TODO this makes evil cursor word search not work

;; auto load if changed
(global-auto-revert-mode t)

;; profiler
(setq profiler-max-stack-depth 128)

;; auto start server if on GUI
(when window-system
  (server-start))

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

;; key-chord timeout
(setq tommyx-key-chord-timeout 1.0)

;; set frame title
(setq frame-title-format (concat "TommyX's Emacs " emacs-version))

;; recent files
(setq recentf-max-menu-items 500)
(setq recentf-max-saved-items 500)

;; line highlight
(setq hl-line-sticky-flag t)
;; hl-line-mode for some modes
(add-hook 'buffer-menu-mode-hook (lambda () (hl-line-mode 1) (setq-local use-line-nav t)))
(add-hook 'profiler-report-mode-hook (lambda () (hl-line-mode 1) (setq-local use-line-nav t)))

;; garbage collection (improve some performance)
(setq gc-cons-threshold 200000000)
(run-with-idle-timer 5 t (lambda () (garbage-collect)))
(add-hook 'focus-out-hook (lambda () (garbage-collect)))

;; save clipboard onto kill ring
(setq save-interprogram-paste-before-kill t)

;; word wrap
(add-hook 'prog-mode-hook (lambda () (toggle-word-wrap 1)))
(add-hook 'text-mode-hook (lambda () (toggle-word-wrap 1)))

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
;;   :group 'appearance)
;; (add-hook 'minibuffer-setup-hook
;;           (lambda ()
;;             (make-local-variable 'face-remapping-alist)
;;             (add-to-list 'face-remapping-alist '(default minibuffer-background))))

;; sidebar face
(defface sidebar-background
  '((t :inherit default))
  "*Face used for the sidebar."
  :group 'appearance)

;; undo limits
(setq undo-limit 1000000)
(setq undo-strong-limit 1000000)

;; fringe margin
(setq-default left-fringe-width 16)
(setq-default right-fringe-width 10)

;; open buffer performance
;; (add-hook 'find-file-hooks 'vc-find-file-hook)
;; (setq vc-handled-backends nil)

;; shell
(cond
 ((eq system-type 'darwin)
  (setq explicit-shell-file-name "/usr/local/bin/zsh"))
 ((not (eq system-type 'windows-nt))
  (setq explicit-shell-file-name "/usr/bin/zsh")))

;; no alert sounds
(setq ring-bell-function 'ignore)

;; input response
(eager-redisplay-mode)


;;; general packages

(require 'tommyx-bind-def)

(enable-auto-compilation 'redo+)
(require 'redo+)
(enable-auto-compilation 'font-lock+)
(require 'font-lock+)
(enable-auto-compilation 'hl-line+)
(require 'hl-line+)
(enable-auto-compilation 'info+)
(require 'info+)

(use-package package-lint :ensure t)

(use-package highlight-function-calls :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (highlight-function-calls-mode))))

(use-package dash :ensure t
  :pin melpa)

(use-package ht :ensure t)

(use-package s :ensure t)

(use-package f :ensure t)

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

(use-package emms :ensure t :after evil
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
    (emms-add-directory-tree emms-default-music-dir))
  (setq evil-emacs-state-modes (delete 'emms-playlist-mode evil-emacs-state-modes)))

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
                                        ; https://github.com/emacs-evil/evil-collection/issues/60
  (setq evil-want-keybinding nil)
  (setq evil-search-module 'evil-search)

  :config
  (evil-mode 1) ; use evil-mode at startup

  ;; disable line highlight in insert and visual mode
  (add-hook 'prog-mode-hook (lambda () (hl-line-mode 1)))
  (add-hook 'text-mode-hook (lambda () (hl-line-mode 1)))
  (add-hook 'evil-insert-state-entry-hook (lambda () (hl-line-mode -1)))
  (add-hook 'evil-insert-state-exit-hook (lambda () (hl-line-mode 1)))
  (add-hook 'evil-visual-state-entry-hook (lambda () (hl-line-mode -1)))
  (add-hook 'evil-visual-state-exit-hook (lambda () (hl-line-mode 1)))
  (defvar-local show-trailing-whitespace-temp nil)
  (add-hook 'evil-insert-state-entry-hook
            (lambda ()
              (setq-local show-trailing-whitespace-temp
                          show-trailing-whitespace)
              (setq-local show-trailing-whitespace nil)))
  (add-hook 'evil-insert-state-exit-hook
            (lambda () (setq-local show-trailing-whitespace
                                   show-trailing-whitespace-temp)))

  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (setq evil-ex-substitute-global t)
  (setq evil-move-cursor-back nil)
  (setq evil-move-beyond-eol t)
  (setq-default evil-symbol-word-search t)
  ;; auto center after search
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
  ;; (advice-add 'evil-ex-search-word-forward :after #'evil-ex-search-previous)
  ;; (advice-add 'evil-ex-search-unbounded-word-forward :after #'evil-ex-search-previous)
  ;; (advice-add 'evil-ex-search-word-backward :after #'evil-ex-search-next)
  ;; (defun my-search-previous (&rest _) (evil-ex-search-previous))
  ;; (defun my-search-next (&rest _) (evil-ex-search-next))
  ;; (advice-add 'evil-visualstar/begin-search-forward :after #'my-search-previous)
  ;; (advice-add 'evil-visualstar/begin-search-backward :after #'my-search-next)
  ;; no magic for search
  (setq evil-magic nil) ; doesn't work
  ;; search highlight persist
  (global-evil-search-highlight-persist) ; doesn't work
  (setq evil-search-highlight-persist-all-windows t)
  ;; no echoing
  (setq evil-insert-state-message nil)
  (setq evil-visual-state-message nil)
  (setq evil-replace-state-message nil)
  ;; custom cursor
  ;; (setq evil-insert-state-cursor '((bar . 4)))
  ;; moved to theme definition
  ;; push jump list every time entering insert mode
  (add-hook 'evil-insert-state-entry-hook 'evil-set-jump)
  ;; do not remove space when leaving insert mode
  (setq evil-allow-remove-spaces t) ;; TODO still allow it.
  (defun my-evil-maybe-remove-spaces (func &rest args)
    (if evil-allow-remove-spaces
        (apply func args)
      (setq evil-maybe-remove-spaces nil)
      (apply func args)))
  (advice-add #'evil-maybe-remove-spaces :around #'my-evil-maybe-remove-spaces)
  (add-hook 'after-init-hook
            (lambda ()
              ;; do not use unto tree due to bugs
              (global-undo-tree-mode -1))))

(use-package evil-collection :ensure t :after evil
  :init
  (setq evil-collection-company-use-tng nil)
  (setq evil-collection-setup-minibuffer nil)
  ;; do not allow certain keys to be used by evil-collection
  ;; TODO: We disabled J and K to encourage ivy use
  (setq evil-collection-key-blacklist
        '("SPC" "K"))

  :config
  (delete 'neotree evil-collection-mode-list)
  (delete 'company evil-collection-mode-list)
  (evil-collection-init))

(use-package evil-visualstar :ensure t
  :config
  (global-evil-visualstar-mode 1))

(use-package evil-surround :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-args :ensure t
  :config
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))

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

(use-package helm :ensure t
  :config
  (require 'helm-config)
  ;; (setq helm-display-function 'helm-display-buffer-in-own-frame
  ;;       helm-display-buffer-reuse-frame t
  ;;       helm-use-undecorated-frame-option t)
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
  (setq helm-move-to-line-cycle-in-source nil)
  (setq helm-ff-file-name-history-use-recentf t)
  (setq helm-follow-mode-persistent t)
  (setq helm-source-names-using-follow '("Occur"))

  ;; bindings
  (tommyx-bind-keys
   `(:case
     :keymaps helm-map
     (:bindings

      next-item helm-next-line
      previous-item helm-previous-line)))

  (tommyx-bind-keys
   `(:case
     :keymaps helm-find-files-map
     (:bindings

      "C-h" helm-find-files-up-one-level)))

  (tommyx-bind-keys
   `(:bindings

     helm-prefix
     (:bindings
      "h" (:def
           helm-resume
           :which-key "Helm Resume")
      "m" (:def
           helm-mini
           :which-key "Helm Mini")
      "p" (:def
           helm-projectile
           :which-key "Helm Projectile")
      "P" (:def
           helm-projectile-switch-project
           :which-key "Helm Projectile Project")
      "C-p" (:def
             helm-projectile-find-file-in-known-projects
             :which-key "Helm Projectile All")
      "<tab>" (:def
               helm-projectile-find-other-file ; cpp vs h switching
               :which-key "Helm Projectile Other File")
      "TAB" (:def
             helm-projectile-find-other-file ; cpp vs h switching
             :which-key "Helm Projectile Other File")
      "r" (:def
           helm-recentf
           :which-key "Helm Recentf")
      "g" (:def
           helm-projectile-grep
           :which-key "Helm Projectile Grep")
      "f" (:def
           helm-find-files
           :which-key "Helm Find Files")
      "F" (:def
           helm-for-files
           :which-key "Helm For Files")
      "x" (:def
           helm-M-x
           :which-key "Helm M-x")
      "o" (:def
           helm-occur
           :which-key "Helm Occur")
      "s" (:def
           helm-swoop
           :which-key "Helm Swoop")))))

(use-package helm-flx :ensure t
  :config
  (setq helm-flx-for-helm-find-files t
        helm-flx-for-helm-locate t)
  (helm-flx-mode +1))

(use-package helm-descbinds :ensure t
  :config
  (helm-descbinds-mode))

(use-package helm-describe-modes :ensure t
  :config
  (global-set-key [remap describe-mode] #'helm-describe-modes))

(use-package helm-swoop :ensure t
  :config
  (setq helm-swoop-split-with-multiple-windows t))

(use-package helm-projectile :ensure t :after projectile
  (helm-projectile-on))

(use-package swiper-helm :ensure t)

(use-package ivy :ensure t
  :config
  ;; main
  (ivy-mode 1)
  (counsel-mode 1)
  ;; remove initial input in ivy commands
  (setq ivy-initial-inputs-alist nil)
  ;; enable fuzzy, except for swiper
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-ignore-order)
          (counsel-rg . ivy--regex-ignore-order)
          (swiper-multi . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  ;; enable wrapping
  (setq ivy-wrap t)
  (setq ivy-action-wrap t)
  ;; add recent files and bookmarks to ivy-switch-buffer
  (setq ivy-use-virtual-buffers t)
  ;; misc
  (setq ivy-height 12)
  (setq ivy-height-alist nil) ; all ivy should have same height
  ;; display functions
  ;; better UI
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

  ;; bindings

  (tommyx-bind-keys
   `(:case
     :keymaps (swiper-map ivy-minibuffer-map counsel-imenu-map)
     (:bindings

      next-item ivy-next-line
      previous-item ivy-previous-line
      next-multiple-item ivy-scroll-up-command
      previous-multiple-item ivy-scroll-down-command
      ;; ivy-next-history-element allows inserting cursor symbol.
      next-history-item ivy-next-history-element
      previous-history-item ivy-previous-history-element
      "<S-return>" ivy-dispatching-done
      "<C-return>" ivy-immediate-done ; use exact input, not candidate
      select-action ivy-done
      "C-M-l" ivy-immediate-done
      "M-L" ivy-dispatching-done
      "M-n" ivy-call
      "M-N" ivy-dispatching-call
      "M-h" ivy-backward-kill-word
      "M-o" ivy-occur ; save to temp buffer for manipulation
      "<tab>" ivy-posframe-avy

      "j" ,(general-key-dispatch 'self-insert-command
             :timeout tommyx-key-chord-timeout
             "j" 'self-insert-command
             ;; "l" 'ivy-done
             "k" 'minibuffer-keyboard-quit
             "v" 'yank
             "h" 'ivy-backward-kill-word
             "p" 'ivy-partial))

     :keymaps (ivy-occur-grep-mode-map ivy-occur-mode-map)
     (:bindings

      global-leader (:case
                     :states nil
                     nil)

      "<tab>" ivy-occur-press
      open-item (:case
                 :states (motion normal)
                 ivy-occur-press-and-switch)
      "<S-return>" ivy-occur-dispatch))))

(use-package ivy-posframe :ensure t :after ivy
  :config

  (setq ivy-posframe-parameters '(
                                  (width . 50)
                                  (border-width . 1)
                                  (internal-border-width . 1)
                                  (undecorated . t)
                                  (min-width . 50)
                                  (refresh . 1)
                                  ))
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
                  (+ window-top window-height)))
          (cons ; top
           window-left
           (max 0
                (- window-top posframe-height)))))
       ;; (t
       ;;   (cons ; window bottom
       ;;     window-left
       ;;     (+ window-top window-height (- 0 modeline-height posframe-height)))
       ;; )
       )))
  (defun ivy-posframe-display-swiper (str)
    (ivy-posframe--display str #'posframe-poshandler-adaptive-top-bottom))
  (setq ivy-display-function nil)
  (setq ivy-display-functions-alist
        '((swiper . ivy-posframe-display-swiper)
          (swiper-multi . nil)
          (swiper-all . nil)
          (ivy-cs . nil)
          (counsel-ag . nil)
          (counsel-rg . nil)
          (counsel-grep . nil)
          (t . ivy-posframe-display-at-point)))
  (ivy-posframe-enable))

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
;;  :config
;;  (setq popwin:adjust-other-windows t)
;;  (popwin-mode 1)
;;  (add-hook 'popwin:after-popup-hook (lambda () (delayed-mode-line-update))))

(use-package counsel :ensure t
  :config

  ;; bindings

  (tommyx-bind-keys
   `(:case
     :keymaps (counsel-find-file-map)
     (:bindings

      ;; Note: use / to enter directory, not ENTER.
      ;; If we want to use ENTER, uncomment below.
      ;; "RET" ivy-alt-done
      ;; "<return>" ivy-alt-done
      ;; "M-l" ivy-alt-done
      "S-RET" ivy-immediate-done ; use exact input, not candidate
      "<S-return>" ivy-immediate-done))))

(use-package counsel-projectile :ensure t :after projectile)

(use-package google-this :ensure t)

(use-package swiper :ensure t
  :config

  ;; bindings

  (tommyx-bind-keys
   `(:case
     :keymaps (swiper-map)
     (:bindings

      "M-s" swiper-query-replace
      "<tab>" ivy-posframe-swiper-avy)))

  (tommyx-bind-keys
   `(:case
     :keymaps (swiper-all-map)
     (:bindings

      "M-s" swiper-all-query-replace))))

(use-package which-key :ensure t
  :config
  (which-key-mode 1)
  (setq which-key-popup-type 'side-window)
  (setq which-key-sort-order 'which-key-prefix-then-key-order-reverse)
  (setq which-key-idle-delay 0.5)
  (setq which-key-idle-secondary-delay 0.1)
  ;; (setq which-key-allow-evil-operators t)
  (setq which-key-show-operator-state-maps nil)
  (setq which-key-binding-filter-function
        (lambda (cell prefix)
          (cond
           ((string-match "move to window 1" (cdr cell)) '("[0-9]" . "move to window [0-9]"))
           ((string-match "move to window [0-9]" (cdr cell)) nil)
           (cell)))))

(use-package spacemacs-theme :ensure t :defer t)

(use-package doom-themes :ensure t :defer t)

(use-package ace-window :ensure t)

(use-package general :ensure t
  :config
  (general-evil-setup)
  (general-auto-unbind-keys))

(use-package beacon :ensure t
  :config
  ;; (beacon-mode 1)
  ;; ;; disable in insert mode
  ;; (add-hook 'evil-insert-state-entry-hook (lambda () (beacon-mode -1)))
  ;; (add-hook 'evil-insert-state-exit-hook (lambda () (beacon-mode 1)))
  (setq beacon-blink-when-focused nil) ; may cause problem
  (setq beacon-blink-when-buffer-changes t)
  (setq beacon-blink-when-window-changes t)
  (setq beacon-blink-when-window-scrolls t)
  (setq beacon-blink-duration 0.15)
  (setq beacon-blink-delay 0.15)
  (setq beacon-size 15)
  (setq beacon-color "#2499ff"))

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

;; TODO: loading our own version
(add-to-list 'load-path (expand-file-name
                         "packages/Highlight-Indentation-for-Emacs"
                         tommyx-config-path))
(enable-auto-compilation 'highlight-indentation)
(require 'highlight-indentation)
(setq highlight-indentation-blank-lines t)
(add-hook 'prog-mode-hook 'highlight-indentation-mode)
(add-hook 'text-mode-hook 'highlight-indentation-mode)

(require 'origami)
(add-to-list 'origami-parser-alist
             '(python-mode . origami-indent-parser))
;; (global-origami-mode 1)

(require 'crosshairs)
;; (use-package origami :ensure t)

(use-package format-all :ensure t)

(use-package volatile-highlights :ensure t
  :config
  ;; (vhl/define-extension 'evil
  ;;             'evil-normal-state)
  ;; (with-eval-after-load 'evil
  ;;     (vhl/install-extension 'evil)
  ;;     (vhl/load-extension 'evil))
  (vhl/define-extension 'undo-tree
                        'undo-tree-move
                        'undo-tree-yank)
  (with-eval-after-load 'undo-tree
    (vhl/install-extension 'undo-tree)
    (vhl/load-extension 'undo-tree))
  (volatile-highlights-mode 1))

(use-package evil-goggles :ensure t
  :config
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
  (evil-goggles-mode 1))

(use-package flycheck :ensure t
  :config
  (setq-default flycheck-idle-change-delay 3)
  (setq-default flycheck-check-syntax-automatically '(idle-change save mode-enabled))
  (global-flycheck-mode 1))

(use-package flyspell-lazy :ensure t
  :config
  (flyspell-lazy-mode 1)
  (setq flyspell-lazy-idle-seconds 2.5)
  (setq flyspell-lazy-window-idle-seconds 5))

(use-package rainbow-delimiters :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'text-mode-hook #'rainbow-delimiters-mode))

(use-package vdiff :ensure t
  :config

  ;; bindings
  (tommyx-bind-keys
   `(:case
     (:bindings

      diff-prefix
      (:bindings

       "b" (:def
            vdiff-buffers
            :which-key "Diff Buffers")

       "q" (:def
            vdiff-quit
            :which-key "Diff Quit")))

     :keymaps vdiff-mode-map
     :states (motion normal visual)
     (:bindings

      ;; TODO: deal with folding
      "<tab>" vdiff-toggle-fold
      "TAB" vdiff-toggle-fold))))

(use-package avy :ensure t
  :config
  ;; avy
  (defun avy-handler-tommyx (char)
    (let (dispatch)
      (cond ((setq dispatch (assoc char avy-dispatch-alist))
             (setq avy-action (cdr dispatch))
             (throw 'done 'restart))
            ;; Let f and tab exit avy
            ((memq char '(?f ?\t 27 ?\C-g))
             ;; exit silently
             (throw 'done 'exit))
            ((eq char ??)
             (avy-show-dispatch-help)
             (throw 'done 'restart))
            ((mouse-event-p char)
             (signal 'user-error (list "Mouse event not handled" char)))
            (t
             (message "No such candidate: %s, hit `C-g' to quit."
                      (if (characterp char) (string char) char))))))
  (setq avy-handler-function 'avy-handler-tommyx)
  (setq avy-keys '(?w ?e ?r ?u ?i ?o ?p ?a ?s ?d ?g ?h ?j ?k ?l ?v ?n))
  (setq avy-all-windows nil)
  (setq avy-goto-word-0-regexp "\\(\\<\\sw\\|\n\\)"))

(use-package smartparens :ensure t
  :config
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
  (defun remove-parens-overlay (&rest _) (sp-remove-active-pair-overlay))
  (add-hook 'evil-insert-state-exit-hook #'remove-parens-overlay)

  ;; don't show in mode display
  :diminish smartparens-mode)

(use-package lsp-mode :ensure t)

(use-package lsp-ui :ensure t :after lsp-mode
  :config
  ;; (add-hook 'lsp-mode-hook 'lsp-ui-mode) ; TODO disabled for performance reasons
  )

(use-package company :ensure t
  :config
  ;; TODO: should we remove the existing backends?
  (make-variable-buffer-local 'company-backends)
  (add-hook 'after-init-hook 'global-company-mode)
  (company-tng-configure-default)
  ;; (company-quickhelp-mode)

  (defface company-preview-active-face
    '((t :inherit company-preview))
    "Face used for company preview when nothing is selected."
    :group 'appearance)

  ;; Patch `company-preview-frontend'.
  (defun company-preview-frontend (command)
    "`company-mode' frontend showing the selection as if it had been inserted."
    (pcase command
      (`pre-command (company-preview-hide))
      (`post-command
       ;; TODO: should we make this run-at-time 0?
       (let* ((completion
               (nth company-selection company-candidates))
              (completion-length (length completion))
              (prefix-length (length company-prefix)))
         (when (>= completion-length prefix-length)
           (company-preview-show-at-point
            (point) completion)
           (let ((ov company-preview-overlay))
             (when (and ov company-selection-changed)
               (overlay-put ov 'after-string nil)
               (overlay-put ov 'display nil)
               (move-overlay ov (max (point-min) (- (point) prefix-length))
                             (point))
               (overlay-put
                ov (if (> prefix-length 0)
                       'display
                     'after-string)
                (propertize
                 (substring-no-properties
                  completion)
                 'face 'company-preview-active-face)))))))
      (`hide (company-preview-hide))))

  ;; Patch tng front-end to not show overlay.
  (defun company-tng-frontend (command)
    "When the user changes the selection at least once, this
frontend will display the candidate in the buffer as if it's
already there and any key outside of `company-active-map' will
confirm the selection and finish the completion."
    (cl-case command
      (show
       (advice-add 'company-select-next :before-until 'company-tng--allow-unselected)
       (advice-add 'company-fill-propertize :filter-args 'company-tng--adjust-tooltip-highlight))
      ;; (update)
      (hide
       (advice-remove 'company-select-next 'company-tng--allow-unselected)
       (advice-remove 'company-fill-propertize 'company-tng--adjust-tooltip-highlight))
      (pre-command
       (when (and company-selection-changed
                  (not (company--company-command-p (this-command-keys))))
         (company--unread-this-command-keys)
         (setq this-command 'company-complete-selection)
         (advice-add 'company-call-backend :before-until 'company-tng--supress-post-completion)))))
  
  (defvar company-echo-metadata-frontend-bypass nil)

  ;; Patch echo-metadata frontend to allow bypassing.
  (defun company-echo-metadata-frontend (command)
    "`company-mode' frontend showing the documentation in the echo area."
    (pcase command
      (`post-command
       (if company-echo-metadata-frontend-bypass
           (setq company-echo-metadata-frontend-bypass nil)
         (when company-selection-changed
           (company-echo-show-when-idle 'company-fetch-metadata))))
      (`hide (company-echo-hide))))

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
        ;;  (concat "debug: "
        ;;          (prin1-to-string company-selection-changed) " "
        ;;          (prin1-to-string return) " "
        ;;          (prin1-to-string (and return (not (numberp return)))) " "
        ;;          (prin1-to-string args)))

        (and return (not (numberp return))))))
  (advice-add #'company--company-command-p :around #'my-company--company-command-p)
  ;; make evil-normal-state abort completion. note that this works only if 'not is the
  ;; first element in company-continue-commands.
  (setq company-continue-commands (-snoc company-continue-commands 'evil-normal-state))

  (eval-after-load 'company
    '(progn
       ;; (global-set-key (kbd "M-1") (lambda (interactive) (company-complete-number 1)))
       ;; (global-set-key (kbd "M-2") (lambda (interactive) (company-complete-number 2)))
       ;; (global-set-key (kbd "M-3") (lambda (interactive) (company-complete-number 3)))
       ;; (global-set-key (kbd "M-4") (lambda (interactive) (company-complete-number 4)))
       ;; (global-set-key (kbd "M-5") (lambda (interactive) (company-complete-number 5)))
       ;; (global-set-key (kbd "M-6") (lambda (interactive) (company-complete-number 6)))
       ;; (global-set-key (kbd "M-7") (lambda (interactive) (company-complete-number 7)))
       ;; (global-set-key (kbd "M-8") (lambda (interactive) (company-complete-number 8)))
       ;; (global-set-key (kbd "M-9") (lambda (interactive) (company-complete-number 9)))
       ;; (global-set-key (kbd "M-0") (lambda (interactive) (company-complete-number 0)))
       ;; (define-key company-active-map (kbd "M-j") 'company-complete-common-or-cycle)
       ;; (define-key company-active-map (kbd "M-k") 'company-select-previous)
       ;; (define-key company-active-map (kbd "M-l") 'company-complete-selection)
       (define-key company-active-map (kbd "C-h") nil)
       (define-key company-active-map (kbd "C-z") 'company-show-doc-buffer)
                                        ; C-z when company open will show help for that symbol in another window.
       (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
       (define-key company-active-map (kbd "<S-tab>") 'company-select-previous)))
  (make-variable-buffer-local 'company-frontends)
  (setq-default company-frontends
                '(company-tng-frontend
                  company-pseudo-tooltip-frontend
                  company-preview-frontend
                  company-echo-metadata-frontend))
  (setq company-idle-delay 0)
  (setq company-tooltip-align-annotations t)
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
  (setq company-flx-limit 256))

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
;;  :hook (company-mode-hook . company-box-mode))

(use-package company-quickhelp :ensure t)

(use-package company-flx :ensure t)

;; (use-package company-lsp :ensure t :after lsp-mode
;;  :config
;;   (setq-default company-backends
;;                 (cons #'company-lsp company-backends)))

(use-package company-ycmd :ensure t
  :config)

;; (require 'company-tabnine)

(use-package company-tabnine :ensure t :after company
  :config
  (setq-default company-backends
                (cons #'company-tabnine company-backends))

  ;; (setq company-idle-delay 0.05)
  ;; (setq company-tabnine-no-continue t)

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
  (advice-add #'company-tabnine :around #'my-company-tabnine))

(use-package yasnippet :ensure t
  :config
  (add-hook 'after-init-hook 'yas-global-mode)
  (add-to-list 'yas-snippet-dirs
               (expand-file-name "snippets" tommyx-config-path))
  (setq company-continue-commands (-snoc company-continue-commands 'yas-insert-snippet)) ; make company break completion

  ;; bindings

  (tommyx-bind-keys
   `(:case
     :keymaps (yas-keymap yas-minor-mode-map)
     (:bindings

      "<tab>" nil
      "<S-tab>" nil)

     :keymaps yas-minor-mode-map
     :states (insert)
     (:bindings

      snippet-expand (menu-item "" yas-expand-from-trigger-key
                                :filter yas--maybe-expand-key-filter)))))

(use-package ycmd :ensure t
  :config
  (setq ycmd-global-config (expand-file-name "third_party/ycmd/.ycm_extra_conf.py"
                                             tommyx-config-path))
  (unless (boundp 'ycmd-server-python-command)
    (setq ycmd-server-python-command "python"))
  (setq ycmd-server-command
        `(,ycmd-server-python-command
          "-u"
          ,(expand-file-name "third_party/ycmd/ycmd/"
                             tommyx-config-path)))
  ;; TODO group these into respective language mode.
  ;; TODO disabled
  ;; (add-hook 'ycmd-mode-hook 'company-ycmd-setup) ; already manually added
  (add-hook 'ycmd-mode-hook 'flycheck-ycmd-setup)
  (add-hook 'ycmd-mode-hook (lambda () (interactive) (when (ycmd-major-mode-to-file-types major-mode) (ycmd-eldoc-setup))))
  ;; attempt to improve performance
  (setq company-ycmd-request-sync-timeout 0)
  (add-to-list 'ycmd-file-type-map '(java-mode "java")) ; file type detection
  )

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
;;  :config
;;  (require 'ecb)
;;  (setq ecb-layout-name "right1")
;; )

;; (use-package sublimity :ensure t
;;  :config
;;  (require 'sublimity-scroll
;;  (require 'sublimity-map)
;;  (require 'sublimity-attractive)
;; )

;; (use-package minimap :ensure t
;;  :config
;; )

(use-package winum :ensure t
  :config
  (winum-mode)
  )

;; (use-package symon :ensure t
;;  :config
;;  (symon-mode)
;; )

(use-package which-func :ensure t
  :config
  (which-function-mode 1))

(use-package workgroups :ensure t)

;; (use-package persp-mode :ensure t
;;  :config
;;  (persp-mode)
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
;;   git-gutter:update-interval 5
;;   git-gutter:modified-sign ""
;;   git-gutter:added-sign ""
;;   git-gutter:deleted-sign ""
;;    git-gutter:window-width nil)

;;   (global-git-gutter-mode +1))

(use-package yascroll :ensure t
  :config
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
       (yascroll:safe-show-scroll-bar)))))

(use-package color-identifiers-mode :ensure t
  :config
  ;; color-identifiers-mode
  (defface color-identifiers-avoid-face-1
    '((t :foreground "#d53b9c34cd15"))
    "")
  (defface color-identifiers-avoid-face-2
    '((t :foreground "#d53a9c359c35"))
    "")
  (defface color-identifiers-avoid-face-3
    '((t :foreground "#ac809c34d53a"))
    "")
  (setq color-identifiers-avoid-faces
        '(color-identifiers-avoid-face-1
          color-identifiers-avoid-face-2
          color-identifiers-avoid-face-3
          font-lock-warning-face
          error))
  (setq color-identifiers-coloring-method 'sequential)
  (setq color-identifiers:max-color-saturation 0.45)
  (setq color-identifiers:min-color-saturation 0.2)
  (setq color-identifiers:timer (run-with-idle-timer 3 t 'color-identifiers:refresh))
  (add-hook 'prog-mode-hook (lambda () (color-identifiers-mode 1)))
  (global-color-identifiers-mode 1)

  ;; extra modes support
  (defun setup-color-identifiers-parser (style mode)
    (cond
     ((eq style 'c)
      (color-identifiers:set-declaration-scan-fn
       mode 'color-identifiers:cc-mode-get-declarations)
      (add-to-list
       'color-identifiers:modes-alist
       `(,mode . (""
                  "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
                  (nil font-lock-variable-name-face)))))
     ((eq style 'js)
      (add-to-list
       'color-identifiers:modes-alist
       `(,mode . ("[^.][[:space:]]*"
                  "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
                  (nil font-lock-variable-name-face))))))))

(use-package auto-highlight-symbol :ensure t
  :config
  (setq ahs-idle-interval 0.3)
  (global-auto-highlight-symbol-mode 1)
  ;; (add-hook 'prog-mode-hook (auto-highlight-symbol-mode 1))
  ;; (add-hook 'html-mode-hook (auto-highlight-symbol-mode 1))
  ;; (add-hook 'nxml-mode-hook (auto-highlight-symbol-mode 1))
  (setq ahs-case-fold-search nil)

  ;; patch to not clear highlight when moving
  (defun ahs-highlight-advice (func &rest args)
    (let ((symbol (car args))
          (beg (cadr args))
          (end (caddr args)))
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
                  ahs-need-fontify nil)
            t)))))
  (advice-add #'ahs-highlight :around #'ahs-highlight-advice)

  ;; patch to fix avy bug
  (defun ahs-idle-function ()
    "Idle function. Called by `ahs-idle-timer'."
    (when (and auto-highlight-symbol-mode)
      (when ahs-highlighted
        (ahs-unhighlight))
      (let ((hl (ahs-highlight-p)))
        (when hl
          (ahs-highlight (nth 0 hl)
                         (nth 1 hl)
                         (nth 2 hl)))))))

(use-package neotree :ensure t
  :config

  (setq neo-buffer-name "*Files*")
  (setq neo-theme (if (display-graphic-p) 'icons 'nerd))
  ;; (setq neo-theme 'nerd)
  (setq neo-show-hidden-files t)
  (add-hook
   'neotree-mode-hook
   (lambda ()
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
  (setq neo-auto-indent-point nil)

  ;; bindings

  (tommyx-bind-keys
   `(:case
     :keymaps neotree-mode-map
     :states (motion normal)
     (:bindings

      ;; "h" (neotree-make-executor :dir-fn neo-open-dir)
      ;; "l" (neotree-make-executor :dir-fn neo-open-dir)
      "R" neotree-refresh
      "r" neotree-refresh
      "u" neotree-select-up-node
      "U" neotree-select-down-node
      "i" neotree-change-root
      "a" neotree-create-node ; add
      "m" neotree-rename-node ; move
      "d" neotree-delete-node ; delete
      "c" neotree-copy-node ; copy
      "o" neotree-enter
      "<return>" neotree-enter))))

(use-package magit :ensure t
  :config
  (setq magit-display-buffer-function
        #'magit-display-buffer-fullframe-status-v1))

(use-package page-break-lines :ensure t)

(use-package dashboard :ensure t :after page-break-lines
  :config
  (dashboard-setup-startup-hook)
  (add-hook 'dashboard-mode-hook (lambda () (hl-line-mode 1) (setq-local use-line-nav t)))
  (setq dashboard-items '((recents  . 5)
                          (projects . 5)
                          (bookmarks . 5)))
  ;; custom logo and message
  (setq dashboard-banner-length 250)
  (setq dashboard-banner-logo-title
        (concat "Emacs " emacs-version " (" system-configuration ")"))
  (setq dashboard-startup-banner (expand-file-name "logo.png"
                                                   tommyx-config-path)))

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

(use-package highlight-numbers :ensure t
  :config
  ;; TODO: move these into respective modes
  (add-hook 'prog-mode-hook #'highlight-numbers-mode)
  (add-hook 'json-mode-hook (lambda ()
                              (highlight-numbers-mode -1)))
  ;; (add-hook 'text-mode-hook #'highlight-numbers-mode)
  )

(use-package highlight-operators :ensure t
  :config)

(use-package hl-todo :ensure t
  :config
  (global-hl-todo-mode)) ; TODO

(use-package emmet-mode :ensure t
  :config
  (setq emmet-move-cursor-after-expanding t)
  (setq emmet-move-cursor-between-quotes t)
  (setq emmet-indentation 2)

  ;; bindings

  (tommyx-bind-keys
   `(:case
     :keymaps emmet-mode-keymap
     (:bindings

      "C-j" nil)

     :keymaps emmet-mode-keymap
     :states (insert)
     (:bindings

      snippet-next-field      yas-next-field
      snippet-previous-field  yas-prev-field
      snippet-expand          yas-insert-snippet
      template-next-field     emmet-next-edit-point
      template-previous-field emmet-prev-edit-point
      template-expand         (:case
                               emmet-expand-line
                               :states (visual)
                               emmet-wrap-with-markup)))))

(use-package imenu-list :ensure t :after neotree
  :config
  (setq imenu-list-position 'right)
  (setq imenu-list-size 30)
  (setq imenu-list-idle-update-delay 1)
  (setq imenu-list-buffer-name "*Outline*")
  (add-hook
   'imenu-list-major-mode-hook
   (lambda ()
     (make-local-variable 'face-remapping-alist)
     (add-to-list 'face-remapping-alist '(default sidebar-background))
     (yascroll-bar-mode -1)
     (setq-local left-fringe-width 0)
     (setq-local right-fringe-width 0)
     (hl-line-mode 1)
     (setq tab-width 2)
     (whitespace-mode 1)
     (setq-local use-line-nav t)))
  (add-hook
   'after-init-hook
   (lambda ()
     (setq imenu-list-mode-line-format mode-line-format)
     (imenu-list-get-buffer-create)
     (imenu-list-start-timer)
     (imenu-list-update nil t)
     (neotree-show)
     ;; (display-buffer-in-side-window (get-buffer imenu-list-buffer-name) '((side . left)))
     ))
  ;; patch to change appearance
  (defun imenu-list--depth-string (depth)
    "Return a prefix string representing an entry's DEPTH."
    (let ((indents (cl-loop for i from 1 to depth collect "\t")))
      (mapconcat #'identity indents "")))
  (defun imenu-list--get-icon-face (depth)
    "Get face for icon.
DEPTH is the depth of the entry in the list."
    (cl-case depth
      (0 'org-level-5)
      (1 'org-level-6)
      (2 'org-level-7)
      (3 'org-level-8)
      (t 'org-level-8)))
  (defun imenu-list--insert-entry (entry depth)
    "Insert a line for ENTRY with DEPTH."
    (if (imenu--subalist-p entry)
        (progn
          (insert (imenu-list--depth-string depth))
          (insert (propertize "+ " 'font-lock-face (imenu-list--get-icon-face depth)))
          (insert-button (format "%s" (car entry))
                         'face (imenu-list--get-face depth t)
                         'help-echo (format "Toggle: %s"
                                            (car entry))
                         'follow-link t
                         'action ;; #'imenu-list--action-goto-entry
                         #'imenu-list--action-toggle-hs
                         )
          (insert "\n"))
      (insert (imenu-list--depth-string depth))
      (insert (propertize "● " 'font-lock-face (imenu-list--get-icon-face depth)))
      (insert-button (format "%s" (car entry))
                     'face (imenu-list--get-face depth nil)
                     'help-echo (format "Go to: %s"
                                        (car entry))
                     'follow-link t
                     'action #'imenu-list--action-goto-entry)
      (insert "\n")))

  ;; bindings

  (tommyx-bind-keys
   `(:case
     :keymaps imenu-list-major-mode-map

     (:bindings

      "o" imenu-list-goto-entry
      "TAB" imenu-list-display-entry
      "<tab>" imenu-list-display-entry))))

;; (use-package window-purpose :ensure t :after neotree imenu-list
;;  :config
;;  (purpose-mode)
;; )

(enable-auto-compilation 'companion)
(require 'companion)
;; TODO: The following fix bug with companion's separator
(add-hook 'after-init-hook
          (lambda ()
            (companion-compile)
            (companion-open)))

;; (require 'smart-completer)


;;; mode specific configs

(tommyx-config-layer "Shaderlab"
  (require 'shaderlab-mode)
  (push 'shaderlab-mode ahs-modes))

(tommyx-config-layer "LaTeX"
  (use-package auctex :defer t :ensure t
    :config
    (dolist (hook '(latex-mode-hook TeX-mode-hook))
      (add-hook
       hook
       (lambda ()
         (setq-local tab-width 2)
         (setq-local indent-tabs-mode default-indent-tabs-mode)
         (setq-local evil-shift-width tab-width))))

    ;; bindings
    (tommyx-bind-keys
     `(:case
       :keymaps latex-mode-map
       :states (motion normal visual)
       (:bindings

        mode-specific-prefix
        (:bindings

         "p" (:def
              preview-buffer
              :which-key "Preview Buffer")

         "P" (:def
              preview-clearout-buffer
              :which-key "Clear Preview Buffer")))))))

(tommyx-config-layer "Kivy"
  (use-package kivy-mode :ensure t))

(tommyx-config-layer "Protobuf"
  (use-package protobuf-mode :ensure t
    :config
    (add-hook
     'protobuf-mode-hook
     (lambda ()
       (setq-local tab-width 2)
       (setq-local evil-shift-width tab-width)
       (setq-local highlight-indentation-offset 4)))
    (push 'protobuf-mode ahs-modes)
    (add-hook 'protobuf-mode-hook 'highlight-indentation-mode)))

(tommyx-config-layer "C/C++"
  (use-package cc-mode :ensure t
    :config
    (setq-default c-basic-offset 4)
    (setq c-default-style
          '((java-mode . "java")
            (awk-mode . "awk")
            (other . "linux")))

    (add-hook
     'c++-mode-hook
     (lambda ()
       (ycmd-mode 1)
       (setq-local company-backends
                   (append '(company-tabnine company-ycmd)
                           company-backends))))

    ;; bindings

    (tommyx-bind-keys
     `(:case
       :keymaps c-mode-map
       :states (motion normal)
       (:bindings

        jump-to-definition ycmd-goto)

       :keymaps c++-mode-map
       :states (motion normal)
       (:bindings

        jump-to-definition ycmd-goto)))))

(tommyx-config-layer "Java"
  (require 'cc-mode)

  (add-hook
   'java-mode-hook
   (lambda ()
     (setq-local tab-width 2)
     (setq-local evil-shift-width tab-width)
     (setq-local highlight-indentation-offset 4)))

  (tommyx-bind-keys
   `(:case
     :keymaps java-mode-map
     :states (motion normal)
     (:bindings

      jump-to-definition ycmd-goto))))

(tommyx-config-layer "R"
  (use-package ess :ensure t
    :config
    (add-hook 'ess-r-mode-hook
              (lambda ()
                (setq-local company-backends
                            (let ((b #'company-tabnine))
                              (cons b (remove b company-backends))))))))

(tommyx-config-layer "C#"
  (use-package csharp-mode :ensure t
    :config
    (setup-color-identifiers-parser 'c 'csharp-mode)
    (add-hook
     'csharp-mode-hook
     (lambda ()
       (ycmd-mode 1)
       (setq-local company-backends
                   (append '(company-tabnine company-ycmd)
                           company-backends))))

    ;; bindings

    (tommyx-bind-keys
     `(:case
       :keymaps csharp-mode-map
       :states (motion normal)
       (:bindings

        jump-to-definition ycmd-goto)))))

(tommyx-config-layer "Markdown"
  (use-package markdown-mode :ensure t)
  (use-package markdown-mode+ :ensure t))

(tommyx-config-layer "Racket"
  (use-package racket-mode :ensure t
    :config
    (add-hook 'racket-mode-hook (lambda () (modify-syntax-entry ?- "w")))
    (push 'racket-mode ahs-modes)))

(tommyx-config-layer "Haskell"
  (use-package haskell-mode :ensure t
    :config
    ;; flycheck has bug
    (add-hook 'haskell-mode-hook (lambda () (flycheck-mode -1)))
    (add-hook
     'haskell-mode-hook
     (lambda ()
       (setq-local tab-width 4)
       (setq-local evil-shift-width tab-width)
       (setq-local haskell-indentation-starter-offset tab-width)
       (setq-local haskell-indentation-left-offset tab-width)
       (setq-local haskell-indentation-layout-offset tab-width)))
    (push 'haskell-mode ahs-modes))

  (use-package haskell-snippets :ensure t))

(tommyx-config-layer "Rust"
  (use-package rust-mode :ensure t))

(tommyx-config-layer "CSV"
  (use-package csv-mode :ensure t
    :config
    (setq csv-align-style 'auto)
    (setq csv-invisibility-default nil)
    (add-hook 'csv-mode-hook
              (lambda ()
                (toggle-truncate-lines 1)
                (call-interactively 'csv-align-fields)))

    ;; bindings
    (tommyx-bind-keys
     `(:case
       :keymaps csv-mode-map
       :states (motion normal)
       (:bindings

        goto-greater-element-left csv-backward-field
        goto-greater-element-right csv-forward-field

        mode-specific-prefix
        (:bindings

         "a" (csv-align-fields
              :which-key "Align Fields"))

        shortcuts-prefix
        (:bindings

         "a" (csv-align-fields
              :which-key "Align Fields")))))))

(tommyx-config-layer "SQL"
  (use-package sql :ensure t
    :config
    (add-hook 'sql-mode-hook (lambda () (modify-syntax-entry ?_ "w")))
    (push 'sql-mode ahs-modes)))

(tommyx-config-layer "Python"
  (setq-default python-indent 4)
  (add-hook
   'python-mode-hook
   (lambda ()
     (setq-local tab-width 4)
     (setq-local indent-tabs-mode default-indent-tabs-mode)
     (setq-local python-indent-offset tab-width)
     (setq-local highlight-indentation-offset tab-width)
     (setq-local python-indent tab-width)
     (setq-local evil-shift-width tab-width)
     (setq-local yas-indent-line 'auto))
   t)

  (tommyx-bind-keys
   `(:case
     (:bindings

      project-prefix
      (:bindings

       "C-p" run-python))

     :keymaps python-mode-map
     :states (motion normal)
     (:bindings

      jump-to-definition elpy-goto-definition
      eval-element-or-region
      (:case
       :states (motion normal)
       (:def
        python-shell-send-defun
        :which-key "Eval Defun In Python")
       :states (visual)
       (:def
        python-shell-send-region
        :which-key "Eval Region In Python"))

      eval-buffer
      (:def
       python-shell-send-buffer
       :which-key "Eval Buffer In Python"))))

  (use-package elpy :ensure t :defer t
    :init
    (elpy-enable)

    :config
    (add-hook 'python-mode-hook (lambda () (flycheck-mode -1)))
    (setq elpy-rpc-timeout 2.5)
    (defun setup-elpy-mode ()
      (interactive)
      (ycmd-mode -1)
      (setq-local company-idle-delay 0)
      (setq-local company-backends
                  (let ((b #'company-tabnine))
                    (cons b (remove b company-backends))))
      (add-hook 'before-save-hook #'elpy-format-code nil 'local))

    (add-hook 'elpy-mode-hook #'setup-elpy-mode)))

(tommyx-config-layer "TypeScript"
  (use-package tide :ensure t
    :config
    (push 'typescript-mode ahs-modes)
    (setup-color-identifiers-parser 'js 'typescript-mode)
    (defun setup-tide-mode ()
      (interactive)
      (tide-setup)
      (setq-local flycheck-check-syntax-automatically '(save mode-enabled))
      (tide-hl-identifier-mode +1)
      (ycmd-mode -1)
      ;; indentation
      (setq-local tab-width 2)
      (setq-local typescript-indent-level 2)
      (setq-local evil-shift-width tab-width)
      (setq-local highlight-indentation-offset 4)
      (setq-local js-indent-level 2)
      (setq-local company-backends
                  (let ((b #'company-tide))
                    (cons b (remove b company-backends))))
      (setq-local company-backends
                  (let ((b #'company-tabnine))
                    (cons b (remove b company-backends)))))
    ;; formats the buffer before saving
    ;; TODO: we don't want this for now
    ;; (add-hook 'before-save-hook 'tide-format-before-save)
    (add-hook 'typescript-mode-hook #'setup-tide-mode)

    ;; bindings

    (tommyx-bind-keys
     `(:case
       :keymaps tide-mode-map
       :states (motion normal)
       (:bindings

        jump-to-definition tide-jump-to-definition)))))

(tommyx-config-layer "GLSL"
  (use-package glsl-mode :ensure t
    :config
    ;; (add-to-list 'auto-mode-alist '("\\.vs\\'" . glsl-mode))
    ;; (add-to-list 'auto-mode-alist '("\\.fs\\'" . glsl-mode))
    (setup-color-identifiers-parser 'c 'glsl-mode)
    (push 'glsl-mode ahs-modes)))

(tommyx-config-layer "JSON"
  (use-package json-mode :ensure t
    :config
    (setq json-reformat:indent-width 2)
    (add-hook
     'json-mode-hook
     (lambda ()
       (setq-local tab-width 2)
       (setq-local evil-shift-width tab-width)
       (setq-local highlight-indentation-offset 4)
       (setq-local js-indent-level 2)))))

;; (use-package vue-mode :ensure t
;;     :config
;;     (setq mmm-submode-decoration-level 0)
;; )

(tommyx-config-layer "SGML"
  (use-package sgml-mode :ensure t
    :config
    (add-hook 'sgml-mode-hook (lambda () (toggle-word-wrap 1)))
    (add-hook 'sgml-mode-hook 'emmet-mode)))

(tommyx-config-layer "HTML"
  (use-package web-mode :ensure t
    :config

    (add-hook 'web-mode-hook 'emmet-mode)

    (dolist (hook '(html-mode-hook web-mode-hook))
      (add-hook
       hook
       (lambda ()
         (modify-syntax-entry ?- "w")
         (modify-syntax-entry ?_ "w"))))

    (add-hook
     'web-mode-hook
     (lambda ()
       (setq-local tab-width 2)
       (setq-local evil-shift-width tab-width)
       (setq-local highlight-indentation-offset 4)))

    ;; bindings

    (tommyx-bind-keys
     `(:case
       :keymaps web-mode-map
       (:bindings

        "C-h" nil
        "C-l" nil)

       :keymaps web-mode-map
       :states (motion normal visual)
       (:bindings

        jump-to-matching web-mode-navigate
        goto-greater-element-up web-mode-element-previous
        goto-greater-element-down web-mode-element-next

        mode-specific-prefix
        (:bindings

         "r" web-mode-element-rename))))

    (push 'web-mode ahs-modes)

    (eval-after-load 'flycheck
      '(flycheck-add-mode 'html-tidy 'web-mode))

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
    ;; use for vue files
    (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))))

(tommyx-config-layer "CSS"
  (require 'web-mode)

  (use-package counsel-css :ensure t
    :config
    (add-hook 'css-mode-hook 'counsel-css-imenu-setup))

  (add-hook 'css-mode-hook 'emmet-mode)

  (dolist (hook '(css-mode-hook))
    (add-hook
     hook
     (lambda ()
       (modify-syntax-entry ?- "w")
       (modify-syntax-entry ?_ "w"))))

  (add-hook
   'css-mode-hook
   (lambda ()
     (setq-local tab-width 2)
     (setq-local evil-shift-width tab-width)
     (setq-local highlight-indentation-offset 4)
     (setq-local web-mode-css-indent-offset 2)
     (setq-local css-indent-offset 2)))

  (push 'css-mode ahs-modes)
  (push 'scss-mode ahs-modes))

(tommyx-config-layer "JavaScript"
  (require 'tide)
  (use-package js2-mode :ensure t
    :config
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (add-hook 'js2-mode-hook #'setup-tide-mode)
    (setq js2-strict-missing-semi-warning nil)
    (push 'js2-mode ahs-modes)))

;; (use-package lsp-python :ensure t :after lsp-mode
;;  :config
;;  (add-hook 'python-mode-hook #'lsp-python-enable)
;; )

;; (use-package lsp-javascript-typescript :ensure t :after lsp-mode
;;  :config
;;  (add-hook 'js-mode-hook #'lsp-javascript-typescript-enable)
;;  (add-hook 'typescript-mode-hook #'lsp-javascript-typescript-enable) ;; for typescript support
;;  (add-hook 'js2-mode-hook #'lsp-javascript-typescript-enable) ;; for js2-mode support
;;  (add-hook 'js3-mode-hook #'lsp-javascript-typescript-enable) ;; for js3-mode support
;;  (add-hook 'rjsx-mode #'lsp-javascript-typescript-enable) ;; for rjsx-mode support
;; )

;; (use-package js2-refactor :ensure t)


;;; key bindings util / helper functions and motion

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

(defun select-paste-region ()
  (interactive)
  (evil-goto-mark ?\[)
  (evil-visual-char)
  (evil-goto-mark ?\]))

(defun delete-line-content ()
  (interactive)
  (if (save-excursion
        (beginning-of-line)
        (looking-at "[ \t]*$"))
      (beginning-of-line)
    (evil-first-non-blank))
  (call-interactively 'evil-delete-line))

(defun smart-open-line-above ()
  (interactive)
  ;; (move-beginning-of-line nil)
  ;; (save-excursion
  ;;   (newline))
  ;; TODO: if we do not want to indent, comment this
  (save-excursion
    (newline)
    (unless (eolp)
      (indent-according-to-mode)))
  (indent-according-to-mode))

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

(defun execute-buffer-as-sh ()
  (interactive)
  (let (compile-command)
    (compile (buffer-string))))

(defun execute-region-as-sh ()
  (interactive)
  (let (compile-command)
    (compile (buffer-substring (region-beginning) (region-end)))))

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
            :history 'counsel-emms-play-history
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
  (setq company-echo-metadata-frontend-bypass t)
  (cond
   (company-selection-changed
    (company-complete-selection))
   (company-candidates
    (company-select-next)
    (company-complete-selection))
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
  (interactive) (company-complete-number 10))

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

(evil-define-command evil-ex-search-word-forward-flash () :repeat nil
  (evil-ex-search-word-forward 1)
  (flash-cursor))

(evil-define-command evil-ex-search-word-backward-flash () :repeat nil
  (evil-ex-search-word-backward 1)
  (flash-cursor))

(evil-define-command evil-visualstar/begin-search-forward-flash () :repeat nil
  (call-interactively 'evil-visualstar/begin-search-forward)
  (flash-cursor))

(evil-define-command evil-visualstar/begin-search-backward-flash () :repeat nil
  (call-interactively 'evil-visualstar/begin-search-backward)
  (flash-cursor))

(setq-default use-line-nav nil)
(evil-define-motion adaptive-avy () :type exclusive :repeat nil :jump t
  (if use-line-nav (evil-avy-goto-line) (evil-avy-goto-word-0 nil)))

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

(evil-define-motion swiper-movement () :type exclusive :repeat nil :jump t
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


;;; hydra

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


;;; key bindings

(tommyx-bind-keys
 `(:case
   (:bindings

    ;; use esc (same as "C-[") for escape
    "C-[" (:def
           keyboard-escape-quit
           :key-name escape)
    "<escape>" keyboard-escape-quit

    "q" (:def
         :ignore
         :key-name quit)
    "M-l" (:def
           :ignore
           :key-name select-action)
    "M-j" (:def
           :ignore
           :key-name next-item)
    "M-k" (:def
           :ignore
           :key-name previous-item)
    "M-J" (:def
           :ignore
           :key-name next-multiple-item)
    "M-K" (:def
           :ignore
           :key-name previous-multiple-item)
    "C-j" (:def
           :ignore
           :key-name next-history-item)
    "C-k" (:def
           :ignore
           :key-name previous-history-item)
    "u" (:def
         :ignore
         :key-name go-back)
    "U" (:def
         :ignore
         :key-name go-forward)
    "r" (:def
         :ignore
         :key-name refresh)
    "o" (:def
         :ignore
         :key-name open-item)
    "<return>" (:def
                :ignore
                :key-name select-item))

   :keymaps override
   (:bindings
    ;; profiler
    "C-M-p" ,(lambda () (interactive) (profiler-start 'cpu+mem))
    "C-M-S-p" ,(lambda () (interactive)
                 (profiler-report) (profiler-stop) (profiler-reset))

    ;; help
    "C-M-h" ,help-map
    "C-M-h C-M-h" counsel-apropos
    "C-M-x" execute-extended-command)

   :states (motion normal visual insert)
   (:bindings

    "C-z" nil)

   :states (motion normal visual)
   (:bindings

    ;; origami mode (disabled due to performance)
    ;; "C-g" origami-close-node-recursively
    ;; "C-j" origami-forward-fold
    ;; "C-k" origami-previous-fold
    ;; "C-;" origami-recursively-toggle-node
    ;; "Z" origami-close-all-nodes
    ;; "X" ,(lambda () (interactive)
    ;;       (origami-open-all-nodes (current-buffer))
    ;;       (origami-mode -1)
    ;;       (origami-mode 1))
    ;; "zx" ,(lambda () (interactive)
    ;;        (origami-show-only-node (current-buffer) (point))
    ;;        (origami-open-node-recursively (current-buffer) (point)))
    ;; "zu" origami-undo
    ;; "zU" origami-redo

    ;; "C-]" (:def
    ;;        evil-jump-to-tag
    ;;        :key-name jump-to-definition)

    "t" (:def
         evil-jump-item
         :key-name jump-to-matching)

    "p" ,(lambda () (interactive)
           (call-interactively 'evil-paste-after)
           (evil-goto-mark ?\]))

    ;; C-p paste then select region (for easy replace)
    "C-p" ,(lambda () (interactive)
             (call-interactively 'evil-paste-after)
             (select-paste-region))

    ;; map dw cw etc.
    "d" (:case
         :states (motion normal)
         ,(general-key-dispatch 'evil-delete
            :timeout tommyx-key-chord-timeout
            "w" (general-simulate-key ('evil-delete "aw"))
            "W" (general-simulate-key ('evil-delete "aW"))
            ")" (general-simulate-key ('evil-delete "i)"))
            "]" (general-simulate-key ('evil-delete "i]"))
            "}" (general-simulate-key ('evil-delete "i}"))
            ">" (general-simulate-key ('evil-delete "i>"))
            "'" (general-simulate-key ('evil-delete "i'"))
            "\"" (general-simulate-key ('evil-delete "i\""))
            "t" (general-simulate-key ('evil-delete "it"))
            "n" (general-simulate-key ('evil-delete "gn")))
         :states (visual)
         evil-delete)

    "c" (:case
         :states (motion normal)
         ,(general-key-dispatch 'evil-change
            :timeout tommyx-key-chord-timeout
            "w" (general-simulate-key ('evil-change "iw"))
            "W" (general-simulate-key ('evil-change "iW"))
            ")" (general-simulate-key ('evil-change "i)"))
            "]" (general-simulate-key ('evil-change "i]"))
            "}" (general-simulate-key ('evil-change "i}"))
            ">" (general-simulate-key ('evil-change "i>"))
            "'" (general-simulate-key ('evil-change "i'"))
            "\"" (general-simulate-key ('evil-change "i\""))
            "t" (general-simulate-key ('evil-change "it"))
            "n" (general-simulate-key ('evil-change "gn")))
         :states (visual)
         evil-change)

    "y" (:case
         :states (motion normal)
         ,(general-key-dispatch 'evil-yank
            :timeout tommyx-key-chord-timeout
            "w" (general-simulate-key ('evil-yank "iw"))
            "W" (general-simulate-key ('evil-yank "iW"))
            ")" (general-simulate-key ('evil-yank "i)"))
            "]" (general-simulate-key ('evil-yank "i]"))
            "}" (general-simulate-key ('evil-yank "i}"))
            ">" (general-simulate-key ('evil-yank "i>"))
            "'" (general-simulate-key ('evil-yank "i'"))
            "\"" (general-simulate-key ('evil-yank "i\""))
            "t" (general-simulate-key ('evil-yank "it")))
         :states (visual)
         ,(lambda () (interactive)
            (call-interactively 'evil-yank)
            (evil-goto-mark ?>)))

    "x" (:case
         :states (motion normal)
         ,(general-key-dispatch 'evil-exchange
            :timeout tommyx-key-chord-timeout
            "w" (general-simulate-key ('evil-exchange "iw"))
            "W" (general-simulate-key ('evil-exchange "iW"))
            ")" (general-simulate-key ('evil-exchange "i)"))
            "]" (general-simulate-key ('evil-exchange "i]"))
            "}" (general-simulate-key ('evil-exchange "i}"))
            ">" (general-simulate-key ('evil-exchange "i>"))
            "'" (general-simulate-key ('evil-exchange "i'"))
            "\"" (general-simulate-key ('evil-exchange "i\""))
            "t" (general-simulate-key ('evil-exchange "it"))
            "n" (general-simulate-key ('evil-exchange "gn")))
         :states (visual)
         ;; copying in visual mode goes to the end of the region
         ,(lambda () (interactive)
            (call-interactively 'evil-exchange)
            (evil-goto-mark ?>)))

    ;; inserting newline
    "M-o" ,(lambda () (interactive)
             (save-excursion (evil-insert-newline-below)))
    "C-o" ,(lambda () (interactive)
             (save-excursion (evil-insert-newline-above)))
    "C-M-o" ,(lambda () (interactive)
               (save-excursion (evil-insert-newline-above))
               (save-excursion (evil-insert-newline-below)))

    ;; indentation
    "{" ,(lambda () (interactive) (evil-shift-left-line 1))
    "}" ,(lambda () (interactive) (evil-shift-right-line 1))

    ;; scrolling
    "M-j" evil-scroll-down
    "M-k" evil-scroll-up

    ;; use M-S-j/k to go to top bottom
    "M-J" evil-goto-line
    "M-K" evil-goto-first-line

    "q" (:def
         evil-execute-macro
         :which-key "Execute Macro")

    "u" (:case
         :states (normal)
         undo)
    "U" (:case
         :states (normal)
         redo)

    "s" evil-surround-edit
    "S" evil-Surround-edit

    ;; search cursor word
    "F" ,(lambda () (interactive)
           (evil-ex-search-word-forward)
           (evil-ex-search-previous))
    "gF" ,(lambda () (interactive)
            (evil-ex-search-unbounded-word-forward)
            (evil-ex-search-next))
    "M-n" evil-ex-search-word-forward-flash
    "M-N" evil-ex-search-word-backward-flash

    ;; avy
    "f" adaptive-avy

    ;; basic movement
    "k" (:def
         evil-previous-visual-line
         :key-name goto-element-up)
    "j" (:def
         evil-next-visual-line
         :key-name goto-element-down)
    "h" (:def
         evil-backward-word-begin
         :key-name goto-element-left)
    "l" (:def
         evil-forward-word-end
         :key-name goto-element-right)

    ;; faster movement
    "K" (:def
         evil-backward-paragraph
         :key-name goto-greater-element-up)
    "J" (:def
         evil-forward-paragraph
         :key-name goto-greater-element-down)
    "H" (:def
         evil-backward-WORD-begin
         :key-name goto-greater-element-left)
    "L" (:def
         evil-forward-WORD-end
         :key-name goto-greater-element-right)
    "G" evil-first-non-blank
    ":" evil-end-of-line

    ;; more comfortable word movement
    "w" evil-backward-char
    "e" evil-forward-char
    "W" fast-move-left
    "E" fast-move-right
    "b" evil-forward-word-begin
    "B" evil-forward-WORD-begin

    ;; jumping through jump list
    "m" evil-jump-backward
    "M" evil-jump-forward
    "M-m" ,(lambda () (interactive)
             (evil-set-jump))

    "C-h" (:def
           outline-up-heading-custom
           :key-name goto-parent-semantic-element)
    "C-l" (:def
           outline-next-heading
           :key-name goto-child-semantic-element)
    "C-k" (:def
           outline-backward-heading-same-level-custom
           :key-name goto-previous-semantic-element)
    "C-j" (:def
           outline-forward-same-level
           :key-name goto-next-semantic-element)
    "C-S-h" outline-promote
    "C-S-l" outline-demote
    "C-S-k" outline-move-subtree-up
    "C-S-j" outline-move-subtree-down

    "Z" (:def
         :ignore
         :key-name fold-focus)
    "X" (:def
         :ignore
         :key-name fold-expand-all)

    ;; flycheck error
    "[f" flycheck-previous-error
    "]f" flycheck-next-error

    ;; navigate by sexp
    "(" evil-sp-backward-sexp
    ")" evil-sp-forward-sexp

    ;; text scaling
    "C--" (:def
           text-scale-decrease
           :which-key "Decrease Text Scale")
    "C-=" (:def
           text-scale-increase
           :which-key "Increase Text Scale")

    ;; change number literals
    "=" evil-numbers/inc-at-pt
    "-" evil-numbers/dec-at-pt

    ;; macro
    "Q" (:def
         evil-record-macro
         :which-key "Record Macro")

    ";" evil-ex

    "n" evil-ex-search-next-flash
    "N" evil-ex-search-previous-flash

    ","
    (:case
     :states (motion normal visual)
     (:bindings
      :which-key "Shortcuts"
      :key-name shortcuts-prefix

      ;; join lines
      "j" evil-join

      ;; repeat last ex command
      "." "@:"

      "z" evil-comfortable-recenter

      "Q" (:def
           ,(lambda () (interactive) (evil-record-macro ?q))
           :which-key "fast record macro")

      "w" evil-write-all

      ;; quick window navigation
      "0" ,(lambda () (interactive)
             (winum-select-window-0)
             (delayed-mode-line-update))
      "1" ,(lambda () (interactive)
             (winum-select-window-1)
             (delayed-mode-line-update))
      "2" ,(lambda () (interactive)
             (winum-select-window-2)
             (delayed-mode-line-update))
      "3" ,(lambda () (interactive)
             (winum-select-window-3)
             (delayed-mode-line-update))
      "4" ,(lambda () (interactive)
             (winum-select-window-4)
             (delayed-mode-line-update))
      "5" ,(lambda () (interactive)
             (winum-select-window-5)
             (delayed-mode-line-update))
      "6" ,(lambda () (interactive)
             (winum-select-window-6)
             (delayed-mode-line-update))
      "7" ,(lambda () (interactive)
             (winum-select-window-7)
             (delayed-mode-line-update))
      "8" ,(lambda () (interactive)
             (winum-select-window-8)
             (delayed-mode-line-update))
      "9" ,(lambda () (interactive)
             (winum-select-window-9)
             (delayed-mode-line-update))

      ;; break line
      "h" newline

      ;; use ivy to select kill ring
      "p" counsel-yank-pop

      "SPC" (:case
             :states (motion normal)
             ;; disable highlight and blink
             evil-noh-blink
             :states (visual)
             ;; easy quit visual mode
             ,(lambda () (interactive)
                (evil-exit-visual-state)
                (beacon-blink)))

      ;; jump to tag
      "n" (:def
           evil-jump-to-tag
           :key-name jump-to-definition)
      ;; "N" ,(lambda () (interactive) (neotree-find))

      ;; substitution
      "s" (:case
           :states (motion normal)
           ,(lambda () (interactive) (evil-ex "s/"))
           :states (visual)
           ,(lambda () (interactive) (evil-ex "'<,'>s/")))

      "S" ,(lambda () (interactive) (evil-ex "%s/"))

      "v" evil-visual-restore
      "V" select-paste-region
      "d" delete-line-content
      "f" flyspell-auto-correct-word
      "F" ,(lambda () (interactive)
             (flyspell-lazy-check-visible)
             (flyspell-auto-correct-previous-word (point)))

      "<tab>" (:case
               :states (motion normal)
               indent-buffer
               :states (visual)
               indent-region)

      "r" (:case
           :states (motion normal)
           (:def
            ,(lambda () (interactive)
               (evil-ex-search-word-forward)
               (evil-ex-search-previous)
               (evil-ex "%s//"))
            :which-key "Replace Element")
           :states (visual)
           (:def
            ,(lambda () (interactive)
               (call-interactively 'evil-visualstar/begin-search-forward)
               (evil-ex-search-previous)
               (evil-ex "%s//"))
            :which-key "Replace Element"))

      ;; macro
      "q" (:case
           :states (motion normal)
           (:def
            ,(lambda () (interactive)
               (evil-execute-macro 1 (evil-get-register ?q t)))
            :which-key "fast execute macro")
           :states (visual)
           (:def
            ,(lambda () (interactive) (evil-ex "'<,'>norm @q"))
            :which-key "fast execute macro"))

      ;; nerd commenter
      "c"
      (:bindings
       :which-key "Comments"

       "SPC" evilnc-comment-or-uncomment-lines
       "SPC" evilnc-comment-or-uncomment-lines
       "y" evilnc-copy-and-comment-lines
       "y" evilnc-copy-and-comment-lines)

      ;; extended shortcut
      ","
      (:bindings
       :which-key "Extended Shortcuts"
       :key-name extended-shortcuts-prefix

       ;; manually update heavy tasks
       "r" (:def
            update-heavy-tasks
            :which-key "update heavy tasks")

       ;; narrowing
       "n" (:case
            :states (motion normal)
            narrow-to-defun
            :states (visual)
            narrow-to-region)

       "N" widen

       "s" sort-lines))

     :states (visual)
     (:bindings
      :which-key "Shortcuts"

      ;; capitalize region
      "C" capitalize-region))

    "SPC"
    (:bindings
     :which-key "Global Leader"
     :key-name global-leader

     "f" (:def
          swiper-movement
          :which-key "Search")
     "C-f" (:def
            swiper-all
            :which-key "Search In All Buffers")
     "F" (:def
          ,(lambda () (interactive)
             (swiper (selection-or-word-at-point)))
          :which-key "Search Selection")
     "C-S-f" (:def
              ,(lambda () (interactive)
                 (swiper-all (selection-or-word-at-point)))
              :which-key "Search Selection All Buf")
     "n" (:def
          ivy-switch-buffer
          :which-key "Switch Buffer")
     "0" (:def
          ,(progn
             (eval-after-load 'which-key
               '(progn
                  ;; create fake key to represent move to window keys
                  ;; hide other keys
                  (setq which-key-replacement-alist
                        (append
                         '((("SPC [1-9]") . t)
                           (("SPC 0") . ("SPC [0-9]" . "move to window [0-9]")))
                         which-key-replacement-alist))))
             (lambda () (interactive)
               (winum-select-window-0-or-10)
               (delayed-mode-line-update)))
          :which-key "Move To Window 0")
     "1"  (:def
           ,(lambda () (interactive)
              (winum-select-window-1)
              (delayed-mode-line-update))
           :which-key "Move To Window 1")
     "2"  (:def
           ,(lambda () (interactive)
              (winum-select-window-2)
              (delayed-mode-line-update))
           :which-key "Move To Window 2")
     "3"  (:def
           ,(lambda () (interactive)
              (winum-select-window-3)
              (delayed-mode-line-update))
           :which-key "Move To Window 3")
     "4"  (:def
           ,(lambda () (interactive)
              (winum-select-window-4)
              (delayed-mode-line-update))
           :which-key "Move To Window 4")
     "5"  (:def
           ,(lambda () (interactive)
              (winum-select-window-5)
              (delayed-mode-line-update))
           :which-key "Move To Window 5")
     "6"  (:def
           ,(lambda () (interactive)
              (winum-select-window-6)
              (delayed-mode-line-update))
           :which-key "Move To Window 6")
     "7"  (:def
           ,(lambda () (interactive)
              (winum-select-window-7)
              (delayed-mode-line-update))
           :which-key "Move To Window 7")
     "8"  (:def
           ,(lambda () (interactive)
              (winum-select-window-8)
              (delayed-mode-line-update))
           :which-key "Move To Window 8")
     "9"  (:def
           ,(lambda () (interactive)
              (winum-select-window-9)
              (delayed-mode-line-update))
           :which-key "Move To Window 9")
     "TAB" (:def
            ,(lambda () (interactive)
               (switch-to-buffer (other-buffer))
               (delayed-mode-line-update))
            :which-key "Switch To Other Buffer")
     "q" (:def
          kill-this-buffer
          :which-key "Kill Current Buffer")
     "x" (:def
          counsel-M-x
          :which-key "Counsel M-x")
     "g" (:def
          google-this-search
          :which-key "Google")
     ";" (:def
          eval-expression
          :which-key "Eval Lisp")

     "w"
     (:bindings
      :which-key "Window"

      "h" (:def
           ,(lambda () (interactive)
              (evil-window-left 1)
              (delayed-mode-line-update))
           :which-key "Move To Window Left")
      "j" (:def
           ,(lambda () (interactive)
              (evil-window-down 1)
              (delayed-mode-line-update))
           :which-key "Move To Window Down")
      "k" (:def
           ,(lambda () (interactive)
              (evil-window-up 1)
              (delayed-mode-line-update))
           :which-key "Move To Window Up")
      "l" (:def
           ,(lambda () (interactive)
              (evil-window-right 1)
              (delayed-mode-line-update))
           :which-key "Move To Window Right")
      "d" (:def
           ,(lambda () (interactive)
              (evil-window-split)
              (delayed-mode-line-update))
           :which-key "Split Window Horizontally")
      "v" (:def
           ,(lambda () (interactive)
              (evil-window-vsplit)
              (delayed-mode-line-update))
           :which-key "Split Window Vertically")
      "m" (:def
           ace-swap-window
           :which-key "Swap Window")
      "f" (:def
           delete-other-windows
           :which-key "Delete Other Windows")
      "q" (:def
           ,(lambda () (interactive)
              (evil-quit)
              (delayed-mode-line-update))
           :which-key "Close Window")
      "u" (:def
           winner-undo
           :which-key "Undo Window Config")
      "U" (:def
           winner-redo
           :which-key "Redo Window Config")
      "p" (:def
           peek-region-in-split
           :which-key "Peek Region In Split")
      "t" (,(lambda () (interactive)
              (evil-window-set-height 12))
           :which-key "Make Into Terminal Height")
      "s"
      (:bindings
       :which-key "Size"

       "h" (,(lambda () (interactive)
               (evil-window-set-height
                (read-number "Enter window height: ")))
            :which-key "Set Height")

       "w" (,(lambda () (interactive)
               (evil-window-set-width
                (read-number "Enter window width: ")))
            :which-key "Set Width")))

     "H"
     (:bindings
      :which-key "Helm"
      :key-name helm-prefix)

     "d"
     (:bindings
      :which-key "Diff"
      :key-name diff-prefix)

     "h"
     (:def
      ,help-map
      :which-key "Help")

     "i"
     (:bindings
      :which-key "Navigation"
      :key-name navigation-prefix

      "i" (:def
           ivy-resume
           :which-key "Ivy Resume")
      ;; "ip" (:def
      ;;       counsel-projectile
      ;;  :which-key "Counsel Projectile")
      "p" (:def
           counsel-projectile-find-file
           :which-key "Project Files")
      "<tab>" (:def
               projectile-find-other-file
               :which-key "Other File")
      "TAB" (:def
             projectile-find-other-file
             :which-key "Other File")
      "r" (:def
           counsel-recentf
           :which-key "Recent Files")
      "C-r" (:def
             recentf-cleanup
             :which-key "Recent Files Clearn-up")
      ;; "g" (:def
      ;;     counsel-projectile-grep
      ;;  :which-key "Project Search")
      "f" (:def
           counsel-find-file
           :which-key "Files")
      "F" (:def
           find-file-at-point
           :which-key "File at Point")
      "s" (:def
           counsel-semantic-or-imenu
           :key-name find-semantic-item
           :which-key "Semantic Item")
      "S" (:def
           ,(lambda () (interactive) (call-interactively 'imenu))
           :which-key "Semantic Item Tree")
      "C-d" (:def
             counsel-rg
             :which-key "Search In Directory")
      "C-S-d" (:def
               ,(lambda () (interactive)
                  (counsel-rg (selection-or-word-at-point t)))
               :which-key "Search Cursor In Directory")
      "d" (:def
           counsel-projectile-rg
           :which-key "Search In Project")
      "D" (:def
           ,(lambda () (interactive)
              (let ((counsel-projectile-rg-initial-input
                     (selection-or-word-at-point t)))
                (counsel-projectile-rg)))
           :which-key "Search Cursor In Project"))

     "o"
     (:bindings
      :which-key "Org"
      :key-name org-prefix)

     "j"
     (:bindings
      :which-key "Mode Specific"
      :key-name mode-specific-prefix

      "e" (:def
           :ignore
           :key-name eval-element-or-region)

      "E" (:def
           :ignore
           :key-name eval-buffer))

     "a"
     (:bindings
      :which-key "Appearance"

      "F" (:def
           ,(lambda () (interactive) (toggle-frame-fullscreen))
           :which-key "Toggle Full-screen")

      "t"
      (:bindings
       :which-key "Theme"

       "l" (:def
            ,(lambda () (interactive) (change-theme light-theme))
            :which-key "Light Theme")
       "d" (:def
            ,(lambda () (interactive) (change-theme dark-theme))
            :which-key "Dark Theme"))

      "f"
      (:bindings
       :which-key "Font"

       "s" (:def
            set-to-small-font
            :which-key "Small Font")
       "b" (:def
            set-to-big-font
            :which-key "Big Font")
       "z" (:def
            hydra-zoom/body
            :which-key "Zoom Buffer Font")
       "r" (:def
            toggle-readable-buffer-font
            :which-key "Toggle Readable Buffer Font")))

     "p"
     (:bindings
      :which-key "Project + Workspace"
      :key-name project-prefix)

     "c"
     (:bindings
      :which-key "Companion"

      "d" (:def
           companion-notif-dismiss
           :which-key "Dismiss Notification")
      "q" (:def
           companion-show-last-qod
           :which-key "Quote Of The Day")
      "y" (:def
           companion-copy-qod
           :which-key "Copy Quote Of The Day")
      "Q" (:def
           companion-fetch-qod
           :which-key "Fetch Quote Of The Day"))

     "s"
     (:bindings
      :which-key "Side Bar"

      ;; "f" (:def
      ;;      ,(lambda () (interactive)
      ;;         (display-buffer-in-side-window
      ;;          (get-buffer neo-buffer-name)
      ;;          '((side . left))))
      ;;      :which-key "Files")
      "f" (:def
           neotree-show
           :which-key "Files")
      "F" (:def
           neotree-find
           :which-key "Locate Current File")
      "o" (:def
           ,(lambda () (interactive)
              (display-buffer-in-side-window
               (get-buffer imenu-list-buffer-name)
               '((side . left))))
           :which-key "Outline"))

     "t"
     (:bindings
      :which-key "Templates"

      "n" (:def
           yas-new-snippet
           :which-key "New")
      "e" (:def
           yas-visit-snippet-file
           :which-key "Edit")
      "r" (:def
           yas-reload-all
           :which-key "Reload")
      "i" (:def
           insert-file
           :which-key "Insert File Content"))

     "e"
     (:bindings
      :which-key "Files"

      "q" (:def
           save-buffers-kill-terminal
           :which-key "Quit Emacs")
      "Q" (:def
           save-buffers-kill-emacs
           :which-key "Quit Emacs Process")
      "s" (:def
           save-buffer
           :which-key "Save")
      "S" (:def
           write-file
           :which-key "Save As")
      "r" (:def
           rename-buffer
           :which-key "Rename Buffer")
      "w" (:def
           evil-write-all
           :which-key "Save All Buffers"))

     "b"
     (:bindings
      :which-key "Battery"

      "c" (:def
           ,(lambda () (interactive) (setq company-idle-delay 0))
           :which-key "Instant Completion")
      "C" (:def
           ,(lambda () (interactive) (setq company-idle-delay 0.2))
           :which-key "Delayed Completion"))

     "m"
     (:bindings
      :which-key "Music"

      "l"
      (:bindings
       :which-key "Playlist"
       "l" (:def
            emms-add-playlist
            :which-key "Add Playlist")
       "a" (:def
            emms-add-file
            :which-key "Add File")
       "d" (:def
            emms-add-directory
            :which-key "Add Directory")
       "D" (:def
            emms-add-directory-tree
            :which-key "Add Directory Recursively")
       "s" (:def
            emms-playlist-save
            :which-key "Save Playlist")
       "g" (:def
            ,(lambda () (interactive)
               (emms-playlist-mode-go)
               (emms-playlist-mode-center-current))
            :which-key "Go To Playlist")
       "s" (:def
            ,(lambda () (interactive)
               (emms-sort)
               (message "Playlist sorted."))
            :which-key "Sort Playlist")
       "S" (:def
            ,(lambda () (interactive)
               (emms-shuffle)
               (message "Playlist shuffled."))
            :which-key "Shuffle Playlist")
       "u" (:def
            emms-uniq
            :which-key "Remove Playlist Duplicates")
       "c" (:def
            emms-playlist-clear
            :which-key "Clear Playlist"))

      "f" (:def
           counsel-emms-play
           :which-key "Play Music")
      "n" (:def
           hydra-emms-control/body
           :which-key "Music Control")
      "p" (:def
           emms-pause
           :which-key "Pause Music")
      "P" (:def
           emms-stop
           :which-key "Stop Music")
      "r" (:def
           emms-random
           :which-key "Play Random Music")
      "t" (:def
           emms-toggle-repeat-track
           :which-key "Toggle Repeat Track"))

     "v"
     (:bindings
      :which-key "Version Control"

      "s" (:def
           magit-status
           :which-key "Magit Status"))))

   :states (visual)
   (:bindings

    ;; Y goes to the start of the region (default)
    "Y" evil-yank

    "X" evil-exchange

    ;; do not re-copy when pasting in visual mode
    "p" ,(lambda () (interactive)
           (call-interactively 'evil-visual-paste)
           (pop-kill-ring))

    ;; indentation
    "{" "<gv"
    "}" ">gv"

    ;; visual selection should not go over the last char
    ":" ,(lambda () (interactive) (evil-end-of-line))

    ;; allow repeat in visual mode
    "." ,(kbd ";norm . RET")
    "q" (:def
         ,(lambda () (interactive) (evil-ex "'<,'>norm @"))
         :which-key "Execute Macro")

    "s" evil-surround-region

    ;; search selection
    "F" ,(lambda () (interactive)
           (call-interactively 'evil-visualstar/begin-search-forward)
           (evil-ex-search-previous))

    "M-n" evil-visualstar/begin-search-forward-flash
    "M-N" evil-visualstar/begin-search-backward-flash)

   :states (insert)
   (:bindings

    "j" ,(progn
           (setq
            insert-mode-j-mapping-func
            (general-key-dispatch
                ;; fallback
                (lambda () (interactive)
                  (let ((my-company--company-command-p-override t))
                    (call-with-command-hooks
                     (lambda ()
                       (interactive)
                       (self-insert-or-send-raw "j")))))
              :timeout tommyx-key-chord-timeout

              "j" (lambda () (interactive)
                    (call-with-command-hooks
                     (lambda () (interactive)
                       (self-insert-or-send-raw "j")) "jj"))
              "t" (lambda () (interactive)
                    (call-with-command-hooks
                     'insert-todo "jt"))
              "f" (lambda () (interactive)
                    (call-with-command-hooks
                     'yas-expand-from-trigger-key "jf"))
              ;; jk quit insert mode
              "k" (lambda () (interactive)
                    (call-with-command-hooks
                     'evil-normal-state "jk"))
              ;; jh delete word
              "h" (lambda () (interactive)
                    (call-with-command-hooks
                     'evil-delete-backward-word "jh"))
              ;; jg move to start of line
              "g" (lambda () (interactive)
                    (call-with-command-hooks
                     'evil-first-non-blank "jg"))
              ;; jl move to end of line
              "l" (lambda () (interactive)
                    (call-with-command-hooks
                     'move-end-of-line "jl"))
              ;; jp complete
              ;; "p" 'company-smart-complete
              "p" (lambda () (interactive)
                    (call-with-command-hooks
                     'company-smart-complete "jp"))
              ;; j[ skip TabNine
              "[" (lambda () (interactive)
                    (call-with-command-hooks
                     'company-tabnine-call-other-backends "j["))
              "0" (lambda () (interactive)
                    (call-with-command-hooks
                     'company-complete-number-0 "j0"))
              "1" (lambda () (interactive)
                    (call-with-command-hooks
                     'company-complete-number-1 "j1"))
              "2" (lambda () (interactive)
                    (call-with-command-hooks
                     'company-complete-number-2 "j2"))
              "3" (lambda () (interactive)
                    (call-with-command-hooks
                     'company-complete-number-3 "j3"))
              "4" (lambda () (interactive)
                    (call-with-command-hooks
                     'company-complete-number-4 "j4"))
              "5" (lambda () (interactive)
                    (call-with-command-hooks
                     'company-complete-number-5 "j5"))
              "6" (lambda () (interactive)
                    (call-with-command-hooks
                     'company-complete-number-6 "j6"))
              "7" (lambda () (interactive)
                    (call-with-command-hooks
                     'company-complete-number-7 "j7"))
              "8" (lambda () (interactive)
                    (call-with-command-hooks
                     'company-complete-number-8 "j8"))
              "9" (lambda () (interactive)
                    (call-with-command-hooks
                     'company-complete-number-9 "j9"))
              ;; j[ context complete (TODO)
              ;; "[" 'evil-complete-next
              ;; j[ insert snippet
              ;; "[" (lambda () (interactive)
              ;;       (call-with-command-hooks 'yas-insert-snippet))
              ;; jv to paste from default register
              "v" (lambda () (interactive)
                    (call-with-command-hooks
                     'paste-from-default-register "jv"))))

           ;; Since company-tng-frontend only complete selection when
           ;; pressing any key that isn't a company-mode command
           ;; (checked with `my-company--company-command-p` function),
           ;; and we want `general-key-dispatch` to have \"j\" as a
           ;; company-mode command (so do not complete) but not to
           ;; have "jp" as one (so do completion).
           ;; Note: need to make sure company-continue-commands allow insert-mode-j-mapping (such as having 'not at first)
           ;; setting :repeat to nil because we don't want the "j" part to be repeatable, only the actual commands invoked afterwards.
           (evil-define-command insert-mode-j-mapping () :repeat nil
             (interactive)
             (call-interactively insert-mode-j-mapping-func))
           (define-key company-active-map "j" 'insert-mode-j-mapping)
           (put 'insert-mode-j-mapping 'company-keep t)
           'insert-mode-j-mapping)

    "J" ,(progn
           (setq
            insert-mode-J-mapping-func
            (general-key-dispatch
                (lambda () (interactive)
                  (let ((my-company--company-command-p-override t))
                    (call-with-command-hooks
                     (lambda ()
                       (interactive)
                       (self-insert-or-send-raw "J")))))
              :timeout tommyx-key-chord-timeout

              "J" (lambda () (interactive)
                    (call-with-command-hooks
                     (lambda () (interactive)
                       (self-insert-or-send-raw "J")) "JJ"))
              ;; JV to use counsel yank-pop
              "V" (lambda () (interactive)
                    (call-with-command-hooks
                     'counsel-yank-pop "JV"))))

           (evil-define-command insert-mode-J-mapping () :repeat nil
             (interactive)
             (call-interactively insert-mode-J-mapping-func))
           (define-key company-active-map "J" 'insert-mode-J-mapping)
           (put 'insert-mode-J-mapping 'company-keep t)
           'insert-mode-J-mapping)

    ;; spell correction
    "C-SPC" ,(lambda () (interactive)
               (save-excursion
                 (flyspell-lazy-check-pending)
                 (flyspell-auto-correct-previous-word (point))))
    "<C-space>" ,(lambda () (interactive)
                   (save-excursion
                     (flyspell-lazy-check-pending)
                     (flyspell-auto-correct-previous-word (point))))

    ;; insert space and move left
    "S-SPC" ,(lambda () (interactive)
               (save-excursion (insert " ")))
    "<S-space>" ,(lambda () (interactive)
                   (save-excursion (insert " ")))

    ;; "M-G" evil-first-non-blank
    "C-J" next-line
    "C-K" previous-line
    "C-H" left-word
    "C-L" right-word
    ;; "M-:" end-of-line
    ;; use M-j/k/l to do completion
    ;; "M-j" company-complete-common-or-cycle
    ;; "M-k" company-select-previous
    ;; "M-l" company-complete-selection

    "<M-return>" outline-insert-heading

    "<S-return>" smart-open-line-above
    "<tab>" tab-to-tab-stop

    ;; snippets
    "M-j" (:def
           yas-next-field
           :key-name snippet-next-field)
    "M-k" (:def
           yas-prev-field
           :key-name snippet-previous-field)
    "M-l" (:def
           yas-insert-snippet
           :key-name snippet-expand)

    "M-J" (:def
           :ignore
           :key-name template-next-field)
    "M-K" (:def
           :ignore
           :key-name template-previous-field)
    "M-L" (:def
           :ignore
           :key-name template-expand))))

(tommyx-bind-keys
 `(:case
   :keymaps (special-mode-map)
   :states (motion normal visual)
   (:bindings

    "h" nil)))

(tommyx-bind-keys
 `(:case
   :keymaps (lisp-interaction-mode-map emacs-lisp-mode-map)
   :states (motion normal visual)
   (:bindings

    mode-specific-prefix
    (:bindings

     "e" (:case
          :states (motion normal)
          (:def
           eval-defun
           :which-key "Eval Defun")
          :states (visual)
          (:def
           ,(lambda () (interactive)
              (message "Evaluating region.")
              (call-interactively 'eval-region))
           :which-key "Eval Region"))

     "E" (:def
          eval-buffer
          :which-key "Eval Buffer")))))

(tommyx-bind-keys
 `(:case
   :keymaps sh-mode-map
   :states (motion normal visual)
   (:bindings

    mode-specific-prefix
    (:bindings

     "e" (:case
          :states (visual)
          (:def
           execute-region-as-sh
           :which-key "Execute Region"))

     "E" (:def
          execute-buffer-as-sh
          :which-key "Execute Buffer")))))

;; (general-define-key
;;  :keymaps 'eshell-mode-map
;;  :states '(motion normal)

;;  "C-j" 'eshell-previous-prompt
;;  "C-k" 'eshell-next-prompt)

;; (general-define-key
;;  :keymaps 'eshell-mode-map
;;  :states '(insert)

;;  "M-j" 'eshell-previous-matching-input-from-input
;;  "M-k" 'eshell-next-matching-input-from-input)

(tommyx-bind-keys
 `(:case
   :keymaps term-raw-map
   :states (insert)
   (:bindings

    "j" ,(general-key-dispatch
             (lambda () (interactive)
               (self-insert-or-send-raw "j"))
           :timeout tommyx-key-chord-timeout

           "j" (lambda () (interactive)
                 (self-insert-or-send-raw "j"))
           "k" 'evil-normal-state
           "v" 'term-paste))))

(tommyx-bind-keys
 `(:case
   :keymaps (minibuffer-local-shell-command-map)
   (:bindings

    previous-item ,(lambda () (interactive)
                     (previous-line-or-history-element)
                     (end-of-line))
    next-item ,(lambda () (interactive)
                 (next-line-or-history-element)
                 (end-of-line))
    select-action exit-minibuffer)))

(tommyx-bind-keys
 `(:case
   :keymaps (minibuffer-local-map evil-ex-completion-map)
   (:bindings

    previous-item ,(lambda () (interactive)
                     (previous-line-or-history-element)
                     (end-of-line))
    next-item ,(lambda () (interactive)
                 (next-line-or-history-element)
                 (end-of-line))
    previous-history-item ,(lambda () (interactive)
                             (call-interactively
                              'previous-complete-history-element)
                             (end-of-line))
    next-history-item ,(lambda () (interactive)
                         (call-interactively
                          'next-complete-history-element)
                         (end-of-line))
    select-action exit-minibuffer)))

(tommyx-bind-keys
 `(:case
   :keymaps help-mode-map
   (:bindings

    go-back (:case
             :states (motion normal)
             help-go-back)
    go-forward (:case
                :states (motion normal)
                help-go-forward))))

(tommyx-bind-keys
 `(:case
   :keymaps compilation-mode-map
   (:bindings

    "h" nil
    "SPC" nil)
   :keymaps compilation-mode-map
   :states (motion normal visual)
   (:bindings

    goto-greater-element-up compilation-previous-error
    goto-greater-element-down compilation-next-error)))


;;; general settings after package load

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


(provide 'tommyx-main)

;;; tommyx-main.el ends here
