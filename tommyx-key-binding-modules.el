(require 'tommyx-packages)
(require 'tommyx-extensions)
(require 'general)
(require 'evil)
(require 'hydra)
(require 'tommyx-config-framework)
(require 'tommyx-key-binding-framework)

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

(defhydra hydra-org-nav ()
  "org heading navigation"
  ("h" evil-org-up-heading "parent heading")
  ("l" evil-outline-next-heading "next heading")
  ("k" evil-org-backward-heading-same-level "previous heading same level")
  ("j" evil-org-forward-heading-same-level "next heading same level"))

(defun tommyx-bind-keys ()
  ($bind-keys
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
      "C-M-h SPC" counsel-apropos
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

      "R" (:case
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

        "e" (:def
             sp-rewrap-sexp
             :which-key "Re-wrap parens")

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
                 (:def
                  indent-buffer
                  :key-name format-buffer)
                 :states (visual)
                 (:def
                  indent-region
                  :key-name format-region))

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

        ;; manually update heavy tasks
        "r" (:def
             update-heavy-tasks
             :which-key "update heavy tasks")

        ;; extended shortcut
        ","
        (:bindings
         :which-key "Extended Shortcuts"
         :key-name extended-shortcuts-prefix

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
       "N" (:def
            ivy-switch-buffer-other-window
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
               ,(lambda () (interactive)
                  (counsel-rg "\\b"))
               :which-key "Search In Directory")
        "C-S-d" (:def
                 ,(lambda () (interactive)
                    (counsel-rg (selection-or-word-at-point t)))
                 :which-key "Search Cursor In Directory")
        "d" (:def
             ,(lambda () (interactive)
                (let ((counsel-projectile-rg-initial-input
                       "\\b"))
                  (counsel-projectile-rg)))
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
             neotree-find
             :which-key "Locate Current File")
        "F" (:def
             neotree-show
             :which-key "Files")
        "p" (:def
             neotree-projectile-action
             :which-key "Locate Project")
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
        "l" (:def
             yas-describe-tables
             :which-key "List")
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
         "f" (:def
              emms-set-active-playlist
              :which-key "Set Active Playlist")
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
         "p" (:def
              emms-state-push
              :which-key "Push Playlist State")
         "P" (:def
              emms-state-pop
              :which-key "Pop Playlist State")
         "s" (:def
              emms-playlist-save
              :which-key "Save Playlist")
         "g" (:def
              emms
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

        "q" (:def
             counsel-emms-enqueue
             :which-key "Enqueue Music")
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
                ;; j' expand snippet
                "'" (lambda () (interactive)
                      (call-with-command-hooks
                       'emmet-expand-line "j'"))
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
                       'company-smart-complete "jp" t))
                ;; j[ skip TabNine
                "[" (lambda () (interactive)
                      (call-with-command-hooks
                       'company-tabnine-call-other-backends "j["))
                "0" (lambda () (interactive)
                      (call-with-command-hooks
                       'company-complete-number-0 "j0" t))
                "1" (lambda () (interactive)
                      (call-with-command-hooks
                       'company-complete-number-1 "j1" t))
                "2" (lambda () (interactive)
                      (call-with-command-hooks
                       'company-complete-number-2 "j2" t))
                "3" (lambda () (interactive)
                      (call-with-command-hooks
                       'company-complete-number-3 "j3" t))
                "4" (lambda () (interactive)
                      (call-with-command-hooks
                       'company-complete-number-4 "j4" t))
                "5" (lambda () (interactive)
                      (call-with-command-hooks
                       'company-complete-number-5 "j5" t))
                "6" (lambda () (interactive)
                      (call-with-command-hooks
                       'company-complete-number-6 "j6" t))
                "7" (lambda () (interactive)
                      (call-with-command-hooks
                       'company-complete-number-7 "j7" t))
                "8" (lambda () (interactive)
                      (call-with-command-hooks
                       'company-complete-number-8 "j8" t))
                "9" (lambda () (interactive)
                      (call-with-command-hooks
                       'company-complete-number-9 "j9" t))
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
                       'counsel-yank-pop "JV"))
                ;; JH to delete line
                "H" (lambda () (interactive)
                      (call-with-command-hooks
                       'evil-delete-backward-line "JH"))))

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

  ($bind-keys
   `(:case
     :keymaps (special-mode-map)
     :states (motion normal visual)
     (:bindings

      "h" nil)))

  ($bind-keys
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

  ($bind-keys
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

  ($bind-keys
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

  ($bind-keys
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

  ($bind-keys
   `(:case
     :keymaps (minibuffer-local-map evil-ex-completion-map)
     (:bindings

      "j" ,(general-key-dispatch 'self-insert-command
             :timeout tommyx-key-chord-timeout
             "j" 'self-insert-command
             ;; "l" 'ivy-done
             "k" 'minibuffer-keyboard-quit
             "v" 'yank
             "h" 'ivy-backward-kill-word
             "p" 'completion-at-point)
      "J" ,(general-key-dispatch 'self-insert-command
             :timeout tommyx-key-chord-timeout
             "J" 'self-insert-command
             "H" 'delete-minibuffer-contents)

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

  ($bind-keys
   `(:case
     :keymaps help-mode-map
     (:bindings

      go-back (:case
               :states (motion normal)
               help-go-back)
      go-forward (:case
                  :states (motion normal)
                  help-go-forward))))

  ($bind-keys
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

  ($bind-keys
   `(:case
     :keymaps helm-map
     (:bindings

      next-item helm-next-line
      previous-item helm-previous-line)))

  ($bind-keys
   `(:case
     :keymaps helm-find-files-map
     (:bindings

      "C-h" helm-find-files-up-one-level)))

  ($bind-keys
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
           :which-key "Helm Swoop"))))

  ($bind-keys
   `(:case
     :keymaps (swiper-map ivy-minibuffer-map ivy-switch-buffer-map counsel-imenu-map)
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
      "<tab>" $ivy-avy

      "j" ,(general-key-dispatch 'self-insert-command
             :timeout tommyx-key-chord-timeout
             "j" 'self-insert-command
             ;; "l" 'ivy-done
             "k" 'minibuffer-keyboard-quit
             "v" 'yank
             "h" 'ivy-backward-kill-word
             "p" 'ivy-partial)
      "J" ,(general-key-dispatch 'self-insert-command
             :timeout tommyx-key-chord-timeout
             "J" 'self-insert-command
             "H" 'delete-minibuffer-contents))

     :keymaps (ivy-occur-grep-mode-map ivy-occur-mode-map)
     (:bindings

      global-leader (:case
                     :states nil
                     nil)

      "<tab>" ivy-occur-press
      open-item (:case
                 :states (motion normal)
                 ivy-occur-press-and-switch)
      "<S-return>" ivy-occur-dispatch)))

  ($bind-keys
   `(:case
     :keymaps (counsel-find-file-map)
     (:bindings

      ;; Note: use / to enter directory, not ENTER.
      ;; If we want to use ENTER, uncomment below.
      ;; "RET" ivy-alt-done
      ;; "<return>" ivy-alt-done
      ;; "M-l" ivy-alt-done
      "S-RET" ivy-immediate-done ; use exact input, not candidate
      "<S-return>" ivy-immediate-done)))

  ($bind-keys
   `(:case
     :keymaps (swiper-map)
     (:bindings

      "M-s" swiper-query-replace
      "<tab>" $ivy-avy)))

  ($bind-keys
   `(:case
     :keymaps (swiper-all-map)
     (:bindings

      "M-s" swiper-all-query-replace)))

  ($bind-keys
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
      "TAB" vdiff-toggle-fold)))

  ($bind-keys
   `(:case
     :keymaps (yas-keymap yas-minor-mode-map)
     (:bindings

      "<tab>" nil
      "<S-tab>" nil)

     :keymaps yas-minor-mode-map
     :states (insert)
     (:bindings

      snippet-expand (menu-item "" yas-expand
                                :filter yas-maybe-expand-abbrev-key-filter))))

  ($bind-keys
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
      goto-parent-semantic-element neotree-select-up-node
      goto-child-semantic-element neotree-select-down-node
      goto-previous-semantic-element neotree-select-previous-sibling-node
      goto-next-semantic-element neotree-select-next-sibling-node
      "<return>" neotree-enter)))

  ($bind-keys
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
                               emmet-wrap-with-markup))))

  ($bind-keys
   `(:case
     :keymaps imenu-list-major-mode-map

     (:bindings

      "o" imenu-list-goto-entry
      "TAB" imenu-list-display-entry
      "<tab>" imenu-list-display-entry)))

  ($bind-keys
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
            :which-key "Clear Preview Buffer")))))

  ($bind-keys
   `(:case
     :keymaps c-mode-map
     :states (motion normal)
     (:bindings

      jump-to-definition ycmd-goto)

     :keymaps c++-mode-map
     :states (motion normal)
     (:bindings

      jump-to-definition ycmd-goto)))

  ($bind-keys
   `(:case
     :keymaps java-mode-map
     :states (motion normal)
     (:bindings

      jump-to-definition ycmd-goto)))

  ($bind-keys
   `(:case
     :keymaps csharp-mode-map
     :states (motion normal)
     (:bindings

      jump-to-definition ycmd-goto)))

  ($bind-keys
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
            :which-key "Align Fields")))))

  ($bind-keys
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

  ($bind-keys
   `(:case
     :keymaps tide-mode-map
     :states (motion normal)
     (:bindings

      jump-to-definition tide-jump-to-definition)))

  ($bind-keys
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

       "r" web-mode-element-rename
       "w" web-mode-element-wrap))))

  ($bind-keys
   `(:case
     :keymaps company-active-map
     (:bindings

      "RET" nil
      "<return>" nil
      "TAB" company-select-next
      "<tab>" company-select-next
      "S-TAB" company-select-previous
      "<backtab>" company-select-previous
      "<S-tab>" company-select-previous)))

  ($bind-keys
   `(:case
     (:bindings

      org-prefix
      (:bindings

       "h" (:def
            helm-org-rifle-org-directory
            :which-key "Helm Org Rifle")
       "f" (:def
            counsel-find-org-files
            :which-key "Org Goto File")
       "d" (:def
            ,(lambda () (interactive)
               (let ((counsel-org-rg-initial-input "\\b"))
                 (counsel-org-rg)))
            :which-key "Org Search")
       "D" (:def
            ,(lambda () (interactive)
               (let ((counsel-org-rg-initial-input
                      (selection-or-word-at-point t)))
                 (counsel-org-rg)))
            :which-key "Org Search Cursor")
       "F" (:def
            counsel-org-goto-all ; go to heading of opened org files
            :which-key "Org Goto Opened Heading")
       "r" (:def
            ,(lambda () (interactive) (org-refile '(4)))
            :which-key "Org Goto Refile Targets")
       "c" (:def
            counsel-org-capture
            :which-key "Org Capture")
       "C" (:def
            org-capture
            :which-key "Org Capture")
       "a" (:def
            org-agenda
            :which-key "Org Agenda")
       "i" (:def
            ,(lambda () (interactive) (org-clock-in '(4))) ; (lambda () (interactive) (org-pomodoro '(4)))
            :which-key "Org Clock In Recent")
       "I" (:def
            org-clock-goto
            :which-key "Org Goto Clock")
       "o" (:def
            org-clock-out
            :which-key "Org Clock Out")
       "O" (:def
            org-clock-cancel
            :which-key "Org Clock Cancel")
       "M-l" (:def
              org-insert-last-stored-link
              :which-key "Org Insert Last Link")
       "l" (:def
            org-insert-link-global
            :which-key "Insert Org Link")
       "L" (:def
            org-store-link
            :which-key "Store Org Link")
       "A"
       (:bindings
        :which-key "Auto Capture"

        "p" (:def
             org-auto-capture-process-subtree
             :which-key "Process Subtree")
        "d" (:def
             org-auto-capture-delete-marked-subtree
             :which-key "Delete Marked In Subtree"))))

     :keymaps org-mode-map
     :states (motion normal visual)
     (:bindings

      "<tab>" (:case
               :states (motion normal visual insert)
               org-cycle)

      ;; using evil-collection with org mode:
      ;;
      ;; notes:
      ;; can use zc, zo, zO etc.
      ;; many of these are dot-repeatable.
      ;;
      ;; TAB: toggle show or hide, navigate table
      ;; C-TAB: cycle visibility
      ;;
      ;; C-c C-t: make into todo / cycle todo states
      ;; C-c C-s: add / change scheduled start
      ;; C-c C-d: add / change deadline
      ;; C-c C-w: refile (move to)
      ;; C-c ,: add / change priority
      ;; C-c .: enter / modify timestamp (date only)
      ;; C-u C-c .: enter / modify timestamp (with time)
      ;; C-c C-.: enter / modify inactive (no agenda) timestamp (date only)
      ;; C-c C-.: enter / modify inactive (no agenda) timestamp (date only)
      ;;
      ;; C-c C-d: refile (move subtree to)
      ;;
      ;; C-c C-c:
      ;; refresh item under cursor
      ;; toggle state of checkbox
      ;; edit tag of item
      ;;
      ;; C-c [: add current file to agenda file list (DO NOT USE)
      ;;
      ;; C-c c: initiate org capture. can be used everywhere.

      ;; remove bindings
      "M-h" nil
      "M-j" nil
      "M-k" nil
      "M-l" nil

      "C-c C-." org-time-stamp-inactive ; with C-u as previx also add time.

      "_" org-shiftdown ; change date like speed-dating
      "+" org-shiftup

      "C-_" org-shiftleft
      "C-+" org-shiftright

      "C-S-h" org-shiftmetaleft ; promote/outdent
      "C-S-j" org-metadown ; move down
      "C-S-k" org-metaup ; move up
      "C-S-l" org-shiftmetaright ; demote/indent

      "<M-return>" org-insert-heading-respect-content
      "<M-S-return>" org-insert-todo-heading-respect-content
      "<C-return>" org-insert-subheading
      "<C-S-return>" org-insert-todo-subheading

      "C-g" ,(lambda () (interactive) (outline-hide-subtree))
      ;; (kbd "C-j") org-next-visible-heading
      ;; (kbd "C-k") org-previous-visible-heading
      ;; ;; (kbd "C-l") ,(lambda () (interactive) (outline-show-entry) (outline-show-children))
      "C-;" org-cycle

      fold-expand-all org-show-all
      fold-focus org-focus
      ;; Just use shift-tab itself, or C-u tab
      ;; "Z" org-shifttab ; cycle global visibility

      goto-parent-semantic-element evil-org-up-heading
      goto-child-semantic-element evil-outline-next-heading
      goto-previous-semantic-element evil-org-backward-heading-same-level
      goto-next-semantic-element evil-org-forward-heading-same-level

      find-semantic-item
      (:def
       counsel-org-goto
       :which-key "Org Goto Headings")

      mode-specific-prefix
      (:case
       (:bindings

        "h" (:def
             org-toggle-heading
             :which-key "Org Toggle Heading/Text")
        "-" (:def
             org-toggle-item
             :which-key "Org Toggle Item Type")
        "[" (:def
             org-toggle-checkbox
             :which-key "Org Toggle Checkbox")
        "]" (:def
             ,(lambda () (interactive) (org-toggle-checkbox '(4)))
             :which-key "Org Toggle Checkbox Presence")
        "f" (:def
             counsel-org-goto
             :which-key "Org Goto")
        ";" (:def
             org-sort
             :which-key "Org Sort")
        ":" (:def
             ,(lambda () (interactive)
                (org-sort-list nil
                               ?f
                               #'org-sort-checklist-getkey-func
                               #'org-sort-checklist-compare-func))
             :which-key "Org Sort Checklist"))

       :states (motion normal)
       (:bindings

        "r" (:def
             org-refile
             :which-key "Org Refile")
        "c" (:def
             org-copy
             :which-key "Org Copy")
        "t" (:def
             org-todo
             :which-key "Org Todo")
        "T" (:def
             org-shiftleft
             :which-key "Org Todo Back")
        "x" (:def
             org-ctrl-c-ctrl-c
             :which-key "Org Update")
        "s" (:def
             org-schedule
             :which-key "Org Schedule")
        "e" (:def
             org-set-effort
             :which-key "Org Set Effort")
        "e" (:def
             org-set-effort
             :which-key "Org Set Effort")
        "E" (:def
             org-inc-effort
             :which-key "Org Increase Effort")
        "C-s" (:def
               ,(lambda () (interactive) (org-todo "TODAY"))
               :which-key "Org Set Today")
        "C-S-s" (:def
                 ,(lambda () (interactive) (org-todo "TODO"))
                 :which-key "Org Remove Today")
        "S" (:def
             ,(lambda () (interactive) (org-schedule nil "+1d"))
             :which-key "Org Schedule To Tomorrow")
        "d" (:def
             org-deadline
             :which-key "Org Deadline")
        "D" (:def
             ,(lambda () (interactive) (org-deadline nil "+1w"))
             :which-key "Org Deadline To 1w")
        "p" (:def
             org-priority
             :which-key "Org Priority")
        "P" (:def
             org-set-property
             :which-key "Org Set Property")
        "q" (:def
             org-set-tags-command
             :which-key "Org Set Tags")
        "i" (:def
             org-clock-in ; org-pomodoro
             :which-key "Org Clock In")
        "%" (:def
             ,(lambda () (interactive)
                (insert "[%]")
                (org-ctrl-c-ctrl-c)
                (insert " "))
             :which-key "Org Insert % Cookie")
        "/" (:def
             ,(lambda () (interactive)
                (insert "[/]")
                (org-ctrl-c-ctrl-c)
                (insert " "))
             :which-key "Org Insert / Cookie")
        "l" (:def
             org-insert-link ; also allow editing link
             :which-key "Org Insert Link")
        "M-l" (:def
               org-insert-last-stored-link
               :which-key "Org Insert Last Link")
        "L" (:def
             org-store-link
             :which-key "Org Store Link")
        "o" (:def
             org-open-at-point
             :which-key "Org Open Link")
        "C-l" (:def
               org-toggle-link-display
               :which-key "Org Toggle Link Display")

        "v"
        (:bindings
         :which-key "Org View And Export"

         "l" (:def
              org-show-all-latex-fragments
              :which-key "Preview Latex Fragment")
         "i" (:def
              org-redisplay-inline-images
              :which-key "Display Inline Images")
         "I" (:def
              org-remove-inline-images
              :which-key "Remove Inline Images")
         "L" (:def
              ,(lambda () (interactive)
                 (org-remove-latex-fragment-image-overlays))
              :which-key "Remove Latex Fragment")
         "c" (:def
              org-clock-display
              :which-key "Clock Display")
         "C" (:def
              org-clock-remove-overlays
              :which-key "Remove Clock Display")
         "d" (:def
              org-update-all-dblocks
              :which-key "Update Dynamic Blocks"))))

      shortcuts-prefix
      (:bindings

       ;; insert date
       "t" org-time-stamp
       ;; insert inactive date
       "T" org-time-stamp-inactive
       ;; show latex fragments
       "l" ,(lambda () (interactive)
              (org-show-all-latex-fragments)
              (org-redisplay-inline-images))
       ;; remove latex fragments
       "L" ,(lambda () (interactive)
              (org-remove-latex-fragment-image-overlays)
              (org-remove-inline-images)))

      extended-shortcuts-prefix
      (:bindings

       ;; narrow
       "n" org-narrow-to-subtree
       ;; insert date and time
       "t" ,(lambda () (interactive)
              (org-time-stamp '(4)))
       ;; insert inactive date and time
       "T" ,(lambda () (interactive)
              (org-time-stamp-inactive '(4)))
       ;; toggle latex fragments
       "l" org-toggle-latex-fragment))

     :keymaps org-mode-map
     :states (insert)
     (:bindings

      ;; using evil-collection with org mode:
      ;;
      ;; M-RET: create heading at same level
      ;; M-S-RET: create TODO heading at same level
      ;; C-RET: create heading at same level below current one (most useful)
      ;; C-S-RET: create TODO heading at same level below current one
      ;;
      ;; TAB and S-TAB: go through table fields
      ;; RET: table next row

      "C-c C-." org-time-stamp-inactive ; with C-u as previx also add time.
      "M-RET" org-meta-return
      "M-S-RET" org-insert-todo-heading
      "C-RET" org-insert-subheading
      "C-S-RET" org-insert-todo-subheading
      "<M-return>" org-meta-return
      "<M-S-return>" org-insert-todo-heading
      "<C-return>" org-insert-subheading
      "<C-S-return>" org-insert-todo-subheading)

     :keymaps org-mode-map
     :states (visual)
     (:bindings

      ;; promote/outdent
      "C-S-h" ,(lambda () (interactive)
                 (org-metaleft)
                 (evil-visual-restore))
      ;; demote/indent
      "C-S-l" ,(lambda () (interactive)
                 (org-metaright)
                 (evil-visual-restore)))

     :keymaps org-agenda-mode-map
     :states (motion normal visual)
     (:bindings

      ;; C-c C-t: make into todo / cycle todo states
      ;; C-c C-s: add / change scheduled start
      ;; C-c C-d: add / change deadline
      ;; C-c ,: add / change priority
      ;;
      ;; .: go to today.
      ;;
      ;; TAB: goto entry.
      ;;
      ;; r: refresh
      ;; q: quit

      fold-focus ,(lambda () (interactive)
                    (when org-agenda-entry-text-mode
                      (org-agenda-entry-text-mode)))
      fold-expand-all ,(lambda () (interactive)
                         (when (not org-agenda-entry-text-mode)
                           (org-agenda-entry-text-mode)))

      "_" org-agenda-do-date-earlier
      "+" org-agenda-do-date-later

      "C-h" org-agenda-earlier
      "C-l" org-agenda-later

      "r" org-agenda-redo
      "u" ,(lambda () (interactive)
             (message "Temporarily disabled undo.")) ; org-agenda-undo
      "U" org-agenda-redo
      "q" org-agenda-quit
      "j" org-agenda-next-line
      "k" org-agenda-previous-line
      goto-next-semantic-element org-agenda-next-date-line
      goto-previous-semantic-element org-agenda-previous-date-line
      "C-c v" org-agenda-view-mode-dispatch

      mode-specific-prefix
      (:bindings

       "r" (:def
            org-agenda-refile
            :which-key "Agenda Refile")
       "f" (:def
            org-agenda-goto-date
            :which-key "Agenda Goto Date")
       "t" (:def
            org-agenda-todo
            :which-key "Agenda Todo")
       "s" (:def
            org-agenda-schedule
            :which-key "Agenda Schedule")
       "e" (:def
            org-agenda-set-effort
            :which-key "Agenda Set Effort")
       "C-s" (:def
              ,(lambda () (interactive)
                 (org-agenda-todo "TODAY"))
              :which-key "Agenda Set Today")
       "C-S-s" (:def
                ,(lambda () (interactive)
                   (org-agenda-todo "TODO"))
                :which-key "Agenda Remove Today")
       "S" (:def
            ,(lambda () (interactive) (org-agenda-schedule nil "+1d"))
            :which-key "Agenda Schedule To Tomorrow")
       "d" (:def
            org-agenda-deadline
            :which-key "Agenda Deadline")
       "D" (:def
            ,(lambda () (interactive) (org-agenda-deadline nil "+1w"))
            :which-key "Agenda Deadline To 1w")
       "p" (:def
            org-agenda-priority
            :which-key "Agenda Priority")
       "P" (:def
            org-agenda-set-property
            :which-key "Agenda Set Property")
       "q" (:def
            org-agenda-set-tags
            :which-key "Agenda Set Tags")
       "i" (:def
            org-agenda-clock-in
            :which-key "Clock In")
       "x"
       (:bindings
        :which-key "Org-life"

        "v" (:def
             ivy-org-life-agenda-show-view
             :which-key "Org-life Show View")
        "m" (:def
             org-life-agenda-show-main
             :which-key "Org-life Main")
        "l" (:def
             org-life-agenda-show-task-list
             :which-key "Org-life Task List"))))

     :keymaps org-capture-mode-map
     :states (motion normal visual)
     (:bindings

      mode-specific-prefix
      (:bindings

       "r" (:def
            org-capture-refile
            :which-key "Capture Refile")
       "j" (:def
            org-capture-kill
            :which-key "Capture Discard")
       "k" (:def
            org-capture-finalize
            :which-key "Capture Save")
       "K" (:def
            ,(lambda () (interactive) (org-capture-finalize '(4)))
            :which-key "Capture Save")))))

  ($bind-keys
   `(:case
     (:bindings

      global-leader
      (:bindings

       "k"
       (:bindings
        :which-key "Catalyst"

        "s" org-catalyst-status
        "r" org-catalyst-recompute-history)))))

  ($bind-keys
   `(:case
     (:bindings

      global-leader
      (:bindings

       "l"
       (:bindings
        :which-key "Layout"

        "s" (:def
             easy-layout-switch
             :which-key "Switch Layout")

        "r" (:def
             easy-layout-refresh
             :which-key "Refresh Active Layout"))))))

  ($bind-keys
   `(:case
     :keymaps (json-mode-map)
     (:bindings
      format-region json-pretty-print
      format-buffer json-pretty-print-buffer))))

($define-module tommyx-key-bindings
  '(:on-init
    (tommyx-bind-keys)))


(provide 'tommyx-key-binding-modules)

;;; tommyx-key-binding-modules.el ends here
