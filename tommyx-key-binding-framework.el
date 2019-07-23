;;; imports

(require 'cl-lib)
(require 'ht)
(require 'general)


;;; functions

(defvar $-key-context-ht (ht-create))
(defvar $-deferred-key-defs (ht-create))
(defvar $-definer 'general-define-key)
;; (defvar $-definer '$-debug-definer)

(defun $-debug-definer (&rest args)
  (message "definer called: %s" (prin1-to-string args)))

(defun $-get-key-context (key-name)
  (ht-get $-key-context-ht key-name))

(defun $-store-key-context (key-name key states)
  (ht-set! $-key-context-ht
           key-name
           `(:key ,key :states ,states)))

(defun $-store-deferred-key-def (key-name args definition)
  (ht-set! $-deferred-key-defs
           key-name
           (cons `(:args ,args :definition ,definition)
                 (ht-get $-deferred-key-defs key-name))))

(defun $-bind-deferred-key-def (key-name)
  (dolist (deferred-def (ht-get $-deferred-key-defs key-name))
    ($-bind-definition
     nil
     (plist-get deferred-def :args)
     `(:bindings
       ,key-name ,(plist-get deferred-def :definition))))
  (ht-set! $-deferred-key-defs key-name nil))

(defun $-bind-definition (prefix args definition)
  (cond

   ((and (listp definition)
         (eq (car definition) :case)) ; adding args
    (setq definition (cdr definition))
    (let ((new-args (copy-sequence args)))
      (while definition
        (if (keywordp (car definition))
            (progn
              (setq new-args
                    (plist-put new-args
                               (car definition)
                               (cadr definition)))
              (setq definition (cddr definition)))
          ($-bind-definition prefix new-args (car definition))
          (setq new-args (copy-sequence args))
          (setq definition (cdr definition))))))

   ((and (listp definition)
         (eq (car definition) :bindings)) ; nested binding
    (setq definition (cdr definition))

    ;; TODO extract this out to function
    (let ((key-names (plist-get definition :key-name)))
      (when key-names
        (unless (listp key-names)
          (setq key-names (list key-names)))
        (dolist (key-name key-names)
          ($-store-key-context key-name
                               prefix
                               (plist-get args :states))
          ($-bind-deferred-key-def key-name))))

    (let ((which-key (plist-get definition :which-key)))
      (when which-key
        (apply $-definer
               (append args
                       `(,prefix (:ignore t :which-key ,which-key))))))

    (while definition
      (let ((key (car definition))
            (key-definition (cadr definition))
            (new-args (copy-sequence args)))

        (unless (keywordp key)
          (when (symbolp key)
            (progn
              (when prefix
                (error "Cannot bind to named key (%s) in nested binding"
                       (prin1-to-string key)))
              (let ((key-context
                     ($-get-key-context key)))
                (if (not key-context)
                    ($-store-deferred-key-def key args key-definition)
                  (setq new-args
                        (plist-put new-args :states
                                   (plist-get key-context :states)))
                  (setq key (plist-get key-context :key))))))

          ;; note: if we stored deferred definition, `key' will stay a symbol.
          (unless (symbolp key)
            (when prefix
              (setq key (concat prefix " " key)))
            ($-bind-definition key new-args key-definition)))
        (setq definition (cddr definition)))))

   ((and (listp definition)
         (eq (car definition) :def)) ; extended definition
    (unless prefix
      (error "No key given for action %s"
             (prin1-to-string definition)))

    ;; TODO extract this out to function
    (let ((key-names (plist-get definition :key-name)))
      (when key-names
        (unless (listp key-names)
          (setq key-names (list key-names)))
        (dolist (key-name key-names)
          ($-store-key-context key-name
                               prefix
                               (plist-get args :states))
          ($-bind-deferred-key-def key-name))))

    (unless (eq (cadr definition) :ignore)
      (apply $-definer (append args `(,prefix ,definition)))))

   (t ; single action
    (unless prefix
      (error "No key given for action %s"
             (prin1-to-string definition)))
    (apply $-definer (append args `(,prefix ,definition))))))

(defun $bind-keys (definition)
  "Bind keys."
  ($-bind-definition nil nil definition))


(provide 'tommyx-key-binding-framework)

;;; tommyx-key-binding-framework.el ends here

