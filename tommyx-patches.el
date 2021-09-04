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
         :internal-border-color (face-attribute 'ivy-posframe-border :background nil t)
         :override-parameters ivy-posframe-parameters)))))

(defun $compile-always-comint ()
  (defun $compilation-start-advice (func &rest args)
    (when (cdr args)
      (setcar (cdr args) (or (cadr args) t)))
    (apply func args))
  (advice-add #'compilation-start :around #'$compilation-start-advice))

(defun $ivy-format-function-patch ()
  (defun ivy-format-function-default (cands)
    "Transform CANDS into a string for minibuffer."
    (ivy--format-function-generic
     (lambda (str)
       (concat "> " (ivy--add-face (concat str "\n") 'ivy-current-match)))
     (lambda (str)
       (concat "  " str "\n"))
     cands
     "")))

(defun $ivy-actions-patch ()
  (ivy-set-actions
   t
   `(("i" ,(lambda (x) (insert (if (stringp x) x (car x)))) "insert")
     ("w" ,(lambda (x) (kill-new (if (stringp x) x (car x)))) "copy")
     ;; TODO: the following does not work on swiper-all. only swiper
     ("y" ,(lambda (x)
             (goto-char swiper--opoint)
             (kill-new
              (let ((s (if (stringp x) x (car x))))
                (with-temp-buffer
                  (insert s)
                  (goto-char (point-min))
                  (skip-chars-forward " \t\n")
                  (skip-chars-forward "0123456789")
                  (skip-chars-forward " \t\n")
                  (buffer-substring (point) (point-max))))))
      "copy swiper")
     ("p" ,(lambda (x)
             (goto-char swiper--opoint)
             (insert
              (let ((s (if (stringp x) x (car x))))
                (with-temp-buffer
                  (insert s)
                  (goto-char (point-min))
                  (skip-chars-forward " \t\n")
                  (skip-chars-forward "0123456789")
                  (skip-chars-forward " \t\n")
                  (buffer-substring (point) (point-max))))))
      "insert swiper"))))

(defun $company-preview-patch ()
  (defface company-preview-active-face
    '((t :inherit company-preview))
    "Face used for company preview when nothing is selected."
    :group 'appearance)

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
      (`hide (company-preview-hide)))))

(defun $company-tng-frontend-no-preview-patch ()
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
  (defvar $company-bypass-tng nil)
  (defun $my-company--company-command-p (func &rest args)
    "Patch company-mode to treat key sequences like \"jp\" not a company-mode command.

Since company-tng-frontend only complete selection when pressing any key that isn't
a company-mode command (checked with this function), and we want general-key-dispatch
to have \"j\" as a company-mode command (so do not complete) but not to have
\"jp\" as one (so do completion)."
    (if $my-company--company-command-p-override
        nil ; treat all command as breaking company completion
      (if $company-bypass-tng
          t ; treat all command as not breaking company completion
        (let ((return (apply func args)))

          ;; (message
          ;;  (concat "debug: "
          ;;          (prin1-to-string company-selection-changed) " "
          ;;          (prin1-to-string return) " "
          ;;          (prin1-to-string (and return (not (numberp return)))) " "
          ;;          (prin1-to-string args)))

          (and return (not (numberp return)))))))
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

(defun $imenu-list-appearance-patch ()
  (defun imenu-list--depth-string (depth)
    "Return a prefix string representing an entry's DEPTH."
    (let ((indents (cl-loop for i from 1 to depth collect "\t")))
      (mapconcat #'identity indents "")))
  (defun imenu-list--get-icon-face (depth)
    "Get face for icon.
DEPTH is the depth of the entry in the list."
    (cl-case depth
      (0 'imenu-list-entry-face-0)
      (1 'imenu-list-entry-face-1)
      (2 'imenu-list-entry-face-2)
      (3 'imenu-list-entry-face-3)
      (t 'imenu-list-entry-face-3)))
  (defun imenu-list--insert-entry (entry depth)
    "Insert a line for ENTRY with DEPTH."
    (if (imenu--subalist-p entry)
        (progn
          (insert (imenu-list--depth-string depth))
          (insert (propertize "- " 'font-lock-face (imenu-list--get-icon-face depth)))
          (insert-button (format "%s" (car entry))
                         'face (if (<= depth 0)
                                   'imenu-list-entry-subalist-face-0
                                 'imenu-list-entry-subalist-face-1)
                         'help-echo (format "Toggle: %s"
                                            (car entry))
                         'follow-link t
                         'action ;; #'imenu-list--action-goto-entry
                         #'imenu-list--action-toggle-hs)
          
          (insert "\n"))
      (insert (imenu-list--depth-string depth))
      (insert (propertize "● " 'font-lock-face (imenu-list--get-icon-face depth)))
      (insert-button (format "%s" (car entry))
                     'face 'imenu-list-entry-face
                     'help-echo (format "Go to: %s"
                                        (car entry))
                     'follow-link t
                     'action #'imenu-list--action-goto-entry)
      (insert "\n"))))

(defun $imenu-list-mode-line-patch ()
  (remove-hook 'imenu-list-major-mode-hook #'imenu-list--set-mode-line))

(defun $all-the-icons-dir-patch ()
  (defun all-the-icons-icon-for-dir (dir &optional chevron padding)
    "Format an icon for DIR with CHEVRON similar to tree based directories.

If PADDING is provided, it will prepend and separate the chevron
and directory with PADDING.

Produces different symbols by inspecting DIR to distinguish
symlinks and git repositories which do not depend on the
directory contents"
    (let* ((matcher (all-the-icons-match-to-alist (file-name-base (directory-file-name dir)) all-the-icons-dir-icon-alist))
           (path (expand-file-name dir))
           ;; (chevron (if chevron (all-the-icons-octicon (format "chevron-%s" chevron) :height 0.8 :v-adjust -0.1) ""))
           (chevron (propertize
                     (pcase chevron
                       ("down" "-")
                       ("right" "+")
                       (_ ""))
                     'face '(:weight bold)))
           (padding (or padding "\t"))
           (icon (cond
                  ((file-symlink-p path)
                   (all-the-icons-octicon "file-symlink-directory" :height 1.0))
                  ((all-the-icons-dir-is-submodule path)
                   (all-the-icons-octicon "file-submodule" :height 1.0))
                  ((file-exists-p (format "%s/.git" path))
                   (format "%s" (all-the-icons-octicon "repo" :height 1.1)))
                  (t (apply (car matcher) (cdr matcher))))))
      (format "%s%s%s%s%s" padding chevron padding icon padding))))

(defun $org-mode-angular-brackets-patch ()
  (add-hook 'org-mode-hook
            (lambda ()
              (modify-syntax-entry ?< ".")
              (modify-syntax-entry ?> "."))))

(defun $org-mode-heading-coloring-patch ()
  (defface org-heading-text-level-1 '((t :inherit org-level-1))
    "Face used for level 1 headline text."
    :group 'org-faces)

  (defface org-heading-text-level-2 '((t :inherit org-level-2))
    "Face used for level 2 headline text."
    :group 'org-faces)

  (defface org-heading-text-level-3 '((t :inherit org-level-3))
    "Face used for level 3 headline text."
    :group 'org-faces)

  (defface org-heading-text-level-4 '((t :inherit org-level-4))
    "Face used for level 4 headline text."
    :group 'org-faces)

  (defface org-heading-text-level-5 '((t :inherit org-level-5))
    "Face used for level 5 headline text."
    :group 'org-faces)

  (defface org-heading-text-level-6 '((t :inherit org-level-6))
    "Face used for level 6 headline text."
    :group 'org-faces)

  (defface org-heading-text-level-7 '((t :inherit org-level-7))
    "Face used for level 7 headline text."
    :group 'org-faces)

  (defface org-heading-text-level-8 '((t :inherit org-level-8))
    "Face used for level 8 headline text."
    :group 'org-faces)

  (defconst org-level-heading-text-faces
    '(org-heading-text-level-1
      org-heading-text-level-2
      org-heading-text-level-3
      org-heading-text-level-4
      org-heading-text-level-5
      org-heading-text-level-6
      org-heading-text-level-7
      org-heading-text-level-8))

  (defconst org-n-level-heading-text-faces
    (length org-level-heading-text-faces))

  (defun org-get-level-face (n)
    "Get the right face for match N in font-lock matching of headlines."
    (setq org-l (- (match-end 2) (match-beginning 1) 1))
    (when org-odd-levels-only (setq org-l (1+ (/ org-l 2))))
    (if org-cycle-level-faces
        (setq org-f (nth (% (1- org-l) org-n-level-faces) org-level-faces))
      (setq org-f (nth (1- (min org-l org-n-level-faces)) org-level-faces)))
    (cond
     ((eq n 1) (if org-hide-leading-stars 'org-hide org-f))
     ((eq n 2) org-f)
     (t (nth (1- (min org-l org-n-level-heading-text-faces))
             org-level-heading-text-faces)))))

(defun color-identifiers:regenerate-colors ()
  "Generate perceptually distinct colors with the same luminance in HSL space.
Colors are output to `color-identifiers:colors'."
  (interactive)
  (let* ((luminance (or color-identifiers:color-luminance
                        (max 0.35 (min 0.8 (color-identifiers:attribute-luminance :foreground)))))
         (min-saturation (float color-identifiers:min-color-saturation))
         (saturation-range (- (float color-identifiers:max-color-saturation) min-saturation))
         (bgcolor (color-identifiers:attribute-lab :background))
         (avoidlist (mapcar 'color-identifiers:foreground-lab color-identifiers-avoid-faces))
         (candidates '())
         (chosens '())
         (n 8)
         (n-1 (float (1- n))))
    ;; Populate candidates with evenly spaced HSL colors with fixed luminance,
    ;; converted to LAB
    (dotimes (h n)
      (dotimes (s n)
        (add-to-list
         'candidates
         (apply 'color-srgb-to-lab
                (color-hsl-to-rgb (/ h (float n))
                                  (+ min-saturation (* (/ s n-1) saturation-range))
                                  luminance)))))
    (let ((choose-candidate (lambda (candidate)
                              (delq candidate candidates)
                              (push candidate chosens))))
      (while (and candidates (< (length chosens) color-identifiers:num-colors))
        (let* (;; For each remaining candidate, find the distance to the closest chosen
               ;; color
               (min-dists (-map (lambda (candidate)
                                  (cons candidate
                                        (-min (-map (lambda (chosen)
                                                      (color-cie-de2000 candidate chosen))
                                                    (cons bgcolor (append chosens avoidlist))))))
                                candidates))
               ;; Take the candidate with the highest min distance
               (best (-max-by (lambda (x y) (> (cdr x) (cdr y))) min-dists)))
          (funcall choose-candidate (car best))))
      (setq color-identifiers:colors
            (-map (lambda (lab)
                    (let* ((srgb (apply 'color-lab-to-srgb lab))
                           (rgb (mapcar 'color-clamp srgb)))
                      (apply 'color-rgb-to-hex rgb)))
                  chosens)))))

(defun $org-fontify-whole-heading-line-patch ()
  (defun org--set-faces-extend (faces extend-p)
    (when (fboundp 'set-face-extend)
      (mapc (lambda (f) (set-face-extend f extend-p)) faces)))
  (org--set-faces-extend org-level-heading-text-faces org-fontify-whole-heading-line)
  (org--set-faces-extend org-level-faces org-fontify-whole-heading-line))

(provide 'tommyx-patches)

;;; tommyx-patches.el ends here
