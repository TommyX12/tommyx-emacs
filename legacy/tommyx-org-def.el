;; scanning all org files

(defvar all-org-directory-files nil
  "List of all org files in org-directory.
Update with 'update-all-org-directory-files'.")

(defun find-org-files
    (&optional recurse dirs file-excludes dir-excludes)
  "Fill the variable `all-org-directory-files'.
Optional parameters:
  RECURSE        If t, scan the directory recusively.
  DIRS           A list of directories to scan for *.org files.
  FILE-EXCLUDES  Regular expression.  If a filename matches this regular
expression,
                 do not add it to `all-org-directory-files'.
  DIR-EXCLUDES   Regular expression.  If a directory name matches this
regular expression,
                 do not add it to `all-org-directory-files'."
  (let ((targets (or dirs (list org-directory)))
        (fex (or file-excludes  "^[#\\.].*$"))
        (dex (or dir-excludes  "^[#\\.].*$\\|archive\\|images"))
        path)
    (dolist (dir targets)
      (if (file-directory-p dir)
          (let ((all (directory-files dir nil "^[^#\\.].*$")))
            (dolist (f all)
              (setq path
                    (concat (file-name-as-directory dir) f))
              (cond
               ((file-directory-p path)
                (if (and recurse (not (string-match dex f)))
                    (find-org-files t (list path) fex dex)))
               ((and (string-match "^[^#\\.].*\\.org$" f)
                     (not (string-match fex f)))
                (setq all-org-directory-files
                      (append (list path) all-org-directory-files))))))
        (message "Not a directory: %s" path)))))

(defun update-all-org-directory-files ()
  "Add files org-refile targets recursively."
  (interactive)
  (setq all-org-directory-files nil)
  (find-org-files t))

;; key binding helpers

(defun org-sort-checklist-compare-func (a b)
  (let ((a (if (string= a "-")
               " "
             a))
        (b (if (string= b "-")
               " "
             b)))
    (string-lessp a b)))

(defun org-sort-checklist-getkey-func ()
  (or (let* ((str (match-string 1))
             (len (length str)))
        (and (stringp str)
             (substring str
                        (- len 2)
                        (- len 1))))
			""))

(defun org-show-all-latex-fragments ()
  (interactive)
  (org-remove-latex-fragment-image-overlays)
  (org-toggle-latex-fragment '(16)))

(defun org-focus ()
  "Fold all subtrees except ones relevant to the current."
  (interactive)
  (org-overview)
  (org-reveal '(4))
  (org-show-entry)
  (org-show-children)
  (recenter nil))

;; automatic capturing

(defconst org-auto-capture-delete-flag "# (deleted) ")
(defconst org-auto-capture-delete-flag-regexp "# (deleted) ")
(defconst org-auto-capture-heading-regexp "\\*+ ")

;; TODO: make these a defcustom
(defvar org-auto-capture-targets nil)

(defun org-auto-capture-immediately-finalize
    (capture-key content)
  "Capture content and immediately finalize."
  (org-capture-string content capture-key)
  (org-capture-finalize)
  '())
(defun org-auto-capture-get-target-map (auto-capture-targets)
  (let ((result (ht-create)))
    (dolist (config auto-capture-targets)
      (let ((capture-key (car config))
            (patterns (cadr config))
            (action (caddr config)))
        (dolist (pattern patterns)
          (when (ht-contains? result pattern)
            (error "Error: pattern \"%s\" already exists" pattern))
          (ht-set! result pattern config))))
    result))
(defun org-auto-capture-ignore
    (capture-key content)
  "Do nothing and move on to the next entry."
  '(:no-mark-delete t))
(defun org-auto-capture-mark-delete ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (insert org-auto-capture-delete-flag)))
(defun org-auto-capture-parse-line (line)
  ;; TODO
  (unless (string-match-p "^[ \t]*$" line)
    (string-match "^\\([^:]+\\):\\(.+\\)" line)
    (let ((pattern (match-string 1 line))
          (content (match-string 2 line)))
      (when (and pattern content)
        `(:pattern ,(string-trim (downcase pattern)) :content ,(string-trim content))))))
(defun org-auto-capture-delete-marked-subtree ()
  "Delete all lines marked to be deleted from current line to the end of current subtree."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (error "Buffer not in org mode"))
  (save-excursion
    (let ((last-point -1))
      (while (progn
               (beginning-of-line)
               (and (not (looking-at org-auto-capture-heading-regexp))
                    (not (= (point) last-point))))
        (if (looking-at org-auto-capture-delete-flag-regexp)
            (delete-region (point-at-bol) (1+ (point-at-eol)))
          (setq last-point (point))
          (next-logical-line))))))
(defun org-auto-capture-process-subtree ()
  "Perform auto capture from current line to the end of current subtree."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (error "Buffer not in org mode"))
  (let ((target-map
         (org-auto-capture-get-target-map org-auto-capture-targets))
        (loop-running t))
    (while loop-running
      (beginning-of-line)
      (cond
       ((looking-at org-auto-capture-delete-flag-regexp)
        (if (eobp)
            (setq loop-running nil)
          (next-logical-line)))
       ((looking-at org-auto-capture-heading-regexp)
        (setq loop-running nil))
       (t
        (let* ((parsed-line (org-auto-capture-parse-line
                             (buffer-substring-no-properties
                              (point-at-bol)
                              (point-at-eol))))
               (pattern (plist-get parsed-line :pattern))
               (content (plist-get parsed-line :content)))
          (cond
           ((null parsed-line)
            (if (eobp)
                (setq loop-running nil)
              (next-logical-line)))
           ((not (ht-contains? target-map (downcase pattern)))
            (message "Error: pattern \"%s\" does not exist" pattern)
            (next-logical-line))
           (t
            (let* ((config (ht-get target-map (downcase pattern)))
                   (capture-key (car config))
                   (action (caddr config))
                   (action-return (funcall action capture-key content)))
              (if (plist-get action-return :stop)
                  (setq loop-running nil)
                (unless (plist-get action-return :no-mark-delete)
                  (org-auto-capture-mark-delete))
                (next-logical-line))))))
        (redisplay))))))


(provide 'tommyx-org-def)

;;; tommyx-org-def.el ends here
