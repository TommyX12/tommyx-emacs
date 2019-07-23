;; TODO: dependencies here

(require 'tommyx-config-framework)

(defun $ivy-posframe-patch ()
  (defun ivy-posframe--display (str &optional poshandler full-width)
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
         :override-parameters ivy-posframe-parameters)))))

(defun $company-preview-patch ()
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
                 'face '$company-preview-active-face)))))))
      (`hide (company-preview-hide)))))

(defun $company-tng-frontend-patch ()
  (defface $company-preview-active-face
    '((t :inherit company-preview))
    "Face used for company preview when nothing is selected."
    :group 'appearence)

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
         (advice-add 'company-call-backend :before-until 'company-tng--supress-post-completion))))))

(defun $company-echo-metadata-frontend-patch ()
  (defvar $company-echo-metadata-frontend-bypass nil)
  (defun company-echo-metadata-frontend (command)
    "`company-mode' frontend showing the documentation in the echo area."
    (pcase command
      (`post-command
       (if $company-echo-metadata-frontend-bypass
           (setq company-echo-metadata-frontend-bypass nil)
         (when company-selection-changed
           (company-echo-show-when-idle 'company-fetch-metadata))))
      (`hide (company-echo-hide)))))

(defun $company-general-compatibility-patch ()
  (setq $my-company--company-command-p-override nil)
  (defun $my-company--company-command-p (func &rest args)
    "Patch company-mode to treat key sequences like \"jp\" not a company-mode command.

Since company-tng-frontend only complete selection when pressing any key that isn't
a company-mode command (checked with this function), and we want general-key-dispatch
to have \"j\" as a company-mode command (so do not complete) but not to have
\"jp\" as one (so do completion)."
    (if $my-company--company-command-p-override
        nil ; treat all command as breaking company completion
      (let ((return (apply func args)))

        ;; (message
        ;;  (concat "debug: "
        ;;          (prin1-to-string company-selection-changed) " "
        ;;          (prin1-to-string return) " "
        ;;          (prin1-to-string (and return (not (numberp return)))) " "
        ;;          (prin1-to-string args)))

        (and return (not (numberp return))))))
  (advice-add #'company--company-command-p :around #'$my-company--company-command-p)
  ;; make evil-normal-state abort completion. note that this works only if 'not is the
  ;; first element in company-continue-commands.
  (setq company-continue-commands (-snoc company-continue-commands 'evil-normal-state)))

(defun $company-posframe-patch ()
  (defun company-posframe-show ()
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

  ;; integration with desktop package if installed
  (when (require 'desktop nil 'noerror)
    (push '(company-posframe-mode . nil)
          desktop-minor-mode-table)))

(defun $company-tabnine-patch ()
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

(defun $company-yasnippet-compatibility-patch ()
  ;; make company break completion
  (setq company-continue-commands (-snoc company-continue-commands 'yas-insert-snippet)))

(defun $ahs-persistent-highlight-patch ()
  (defun $ahs-highlight-advice (func &rest args)
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
  (advice-add #'ahs-highlight :around #'$ahs-highlight-advice))

(defun $ahs-bug-patch ()
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

(defun $imenu-list-appearence-patch ()
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
      (insert "\n"))))

(provide 'tommyx-patches)

;;; tommyx-patches.el ends here

