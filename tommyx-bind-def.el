;;; imports

(require 'cl-lib)
(require 'ht)
(require 'general)


;;; functions

(defvar tommyx--key-context-ht (ht-create))
(defvar tommyx--deferred-key-defs (ht-create))
(defvar tommyx--definer 'general-define-key)
;; (defvar tommyx--definer 'tommyx--debug-definer)

(defun tommyx--debug-definer (&rest args)
  (message "definer called: %s" (prin1-to-string args)))

(defun tommyx--get-key-context (key-name)
  (ht-get tommyx--key-context-ht key-name))

(defun tommyx--store-key-context (key-name key states)
  (ht-set! tommyx--key-context-ht
           key-name
           `(:key ,key :states ,states)))

(defun tommyx--store-deferred-key-def (key-name args definition)
  (ht-set! tommyx--deferred-key-defs
           key-name
           (cons `(:args ,args :definition ,definition)
                 (ht-get tommyx--deferred-key-defs key-name))))

(defun tommyx--bind-deferred-key-def (key-name)
  (dolist (deferred-def (ht-get tommyx--deferred-key-defs key-name))
    (tommyx--bind-definition
     nil
     (plist-get deferred-def :args)
     `(:bindings
       ,key-name ,(plist-get deferred-def :definition))))
  (ht-set! tommyx--deferred-key-defs key-name nil))

(defun tommyx--bind-definition (prefix args definition)
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
          (tommyx--bind-definition prefix new-args (car definition))
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
          (tommyx--store-key-context key-name
                                     prefix
                                     (plist-get args :states))
          (tommyx--bind-deferred-key-def key-name))))

    (let ((which-key (plist-get definition :which-key)))
      (when which-key
        (apply tommyx--definer
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
                     (tommyx--get-key-context key)))
                (if (not key-context)
                    (tommyx--store-deferred-key-def key args key-definition)
                  (setq new-args
                        (plist-put new-args :states
                                   (plist-get key-context :states)))
                  (setq key (plist-get key-context :key))))))

          ;; note: if we stored deferred definition, `key' will stay a symbol.
          (unless (symbolp key)
            (when prefix
              (setq key (concat prefix " " key)))
            (tommyx--bind-definition key new-args key-definition)))
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
          (tommyx--store-key-context key-name
                                     prefix
                                     (plist-get args :states))
          (tommyx--bind-deferred-key-def key-name))))

    (unless (eq (cadr definition) :ignore)
      (apply tommyx--definer (append args `(,prefix ,definition)))))

   (t ; single action
    (unless prefix
      (error "No key given for action %s"
             (prin1-to-string definition)))
    (apply tommyx--definer (append args `(,prefix ,definition))))))

(defun tommyx-bind-keys (definition)
  "Bind keys."
  (tommyx--bind-definition nil nil definition))


(provide 'tommyx-bind-def)

;;; tommyx-bind-def.el ends here
