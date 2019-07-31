;;; tommyx-config-framework.el --- summary -*- lexical-binding: t -*-

;; Author: TommyX
;; Maintainer: TommyX
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:

(require 'cl-lib)

(defvar $config-macros (make-hash-table :test 'equal))
(defvar $modules (make-hash-table :test 'equal))

(defun $is-quoted (symbol)
  "TODO"
  (and (consp symbol)
       (eq (car symbol) 'quote)))

(defun $warning (module context msg &rest args)
  (display-warning
   'tommyx-config-framework
   (format "(Module: %s, Context: %s) %s"
           (prin1-to-string module)
           (prin1-to-string context)
           (apply #'format msg args)))
  nil)

(defmacro $define-settings-macro (name inputs &rest body)
  (declare (indent 2) (doc-string 3))
  `(puthash (symbol-name (quote ,name))
            (cl-function
             (lambda (,@inputs)
               ,@body))
            $config-macros))

(defmacro $define-module (name &rest components)
  "TODO"
  (declare (indent 1) (doc-string 2))
  `(let ((c (list ,@components)))
     (when (stringp (car c))
       (setq c (cdr c)))
     (puthash (symbol-name (quote ,name)) c $modules)))

(defun $make-setter (local components &optional context module)
  "TODO"
  (let ((setter
         `(lambda ()
            ,@(mapcar
               (lambda (component)
                 (let ((key (car component))
                       (value (cdr component)))
                   (if (keywordp (car value))
                       (cond
                        ((eq (car value) :append-front)
                         `(,(if local 'setq-local 'setq-default)
                           ,(eval key)
                           (append (list ,@(cdr value))
                                   ,(eval key))))
                        ((eq (car value) :delete)
                         `(progn
                            ,@(mapcar
                               (lambda (val)
                                 `(,(if local 'setq-local 'setq-default)
                                   ,(eval key)
                                   (remove ,val ,(eval key))))
                               (reverse (cdr value)))))
                        ((eq (car value) :ensure-front)
                         `(progn
                            ,@(mapcar
                               (lambda (val)
                                 `(,(if local 'setq-local 'setq-default)
                                   ,(eval key)
                                   (cons ,val (remove ,val ,(eval key)))))
                               (reverse (cdr value)))))
                        ((eq (car value) :default)
                         (if (cddr value)
                             ($warning
                              module context
                              "Multiple values provided for setting %s"
                              (prin1-to-string key))
                           `(unless (boundp ,key)
                              (,(if local 'setq-local 'setq-default)
                               ,(eval key)
                               ,(cadr value)))))
                        ((keywordp (car value))
                         ($warning module context
                                   "Unrecognized modifier %s"
                                   (prin1-to-string (car value)))))
                     (if (cdr value)
                         ($warning
                          module context
                          "Multiple values provided for setting %s"
                          (prin1-to-string key))
                       `(,(if local 'setq-local 'setq-default)
                         ,(eval key)
                         ,(car value))))))
               components))))
    (eval setter)))

(defun $make-function-runner (components)
  (let ((function-runner
         `(lambda ()
            ,@components)))
    (eval function-runner)))

(defun $install-chunk (components &optional context module)
  "TODO"
  (when components
    (condition-case err
        (let* ((type (plist-get context :type))
               (modes (plist-get context :mode-local))
               (mode-hooks
                (and modes
                     (mapcar (lambda (m)
                               (intern (concat (symbol-name m) "-hook")))
                             modes))))
          (pcase type
            (:settings
             (let ((setter ($make-setter mode-hooks components context module)))
               (if mode-hooks
                   (dolist (mode-hook mode-hooks) (add-hook mode-hook setter t))
                 (funcall setter))))
            ((or :minor-modes :on-init :patches)
             (if (and modes (eq type :patches))
                 ($warning module context
                           ":patches not supported in mode-local (%s) config"
                           (prin1-to-string modes))
               (let ((function-runner ($make-function-runner components)))
                 (if mode-hooks
                     (dolist (mode-hook mode-hooks)
                       (add-hook mode-hook function-runner t))
                   (funcall function-runner)))))
            (:after-init
             (if mode-hooks
                 ($warning module context
                           ":after-init not supported in mode-local (%s) config"
                           (prin1-to-string modes))
               (let ((function-runner ($make-function-runner components)))
                 (add-hook 'after-init-hook function-runner t))))
            (:on-idle
             (if mode-hooks
                 ($warning module context
                           ":on-idle not supported in mode-local (%s) config"
                           (prin1-to-string modes))
               (dolist (component components)
                 (if (and (numberp (car component))
                          (consp (cdr component)))
                     (run-with-idle-timer (car component) t
                                          ($make-function-runner
                                           (cdr component)))
                   ($warning module context
                             "Unrecognized idle timer definition: %s"
                             (prin1-to-string component))))))
            (:on-interval
             (if mode-hooks
                 ($warning module context
                           ":on-interval not supported in mode-local (%s) config"
                           (prin1-to-string modes))
               (dolist (component components)
                 (if (and (numberp (car component))
                          (consp (cdr component)))
                     (run-at-time 0 (car component)
                                  ($make-function-runner
                                   (cdr component)))
                   ($warning module context
                             "Unrecognized interval timer definition: %s"
                             (prin1-to-string component))))))
            (:on-focus-out
             (let ((function-runner ($make-function-runner components)))
               (if mode-hooks
                   (dolist (mode-hook mode-hooks)
                     (add-hook mode-hook
                               (lambda ()
                                 (add-hook 'focus-out-hook function-runner t t))))
                 (add-hook 'focus-out-hook function-runner t))))
            (:on-before-save
             (let ((function-runner ($make-function-runner components)))
               (if mode-hooks
                   (dolist (mode-hook mode-hooks)
                     (add-hook mode-hook
                               (lambda ()
                                 (add-hook 'before-save-hook function-runner t t))))
                 (add-hook 'before-save-hook function-runner t))))
            (_ ($warning module context
                         "Unrecognized keyword: %s %s"
                         (prin1-to-string type)
                         (prin1-to-string components)))))
      (error ($warning module context
                       "Error: %s" (error-message-string err))))))

(defun $install-component (component &optional context module)
  "TODO"
  (let ((cur-context (car component))
        (raw-elements (cdr component))
        (requirements-met t)
        elements)

    (cond
     ((consp cur-context)
      (let ((context-key (car cur-context))
            (context-value (cdr cur-context)))
        (cond
         ((eq context-key :when)
          (setq requirements-met
                (eval (car context-value))))
         ((eq context-key :require)
          (dolist (dep context-value)
            (unless (featurep dep)
              ($warning module context
                        "Feature %s not found"
                        (symbol-name dep))
              (setq requirements-met nil))))
         ((eq context-key :macro)
          (let ((macro-func (gethash (symbol-name (car context-value))
                                     $config-macros)))
            (if macro-func
                (setq raw-elements (apply macro-func raw-elements))
              ($warning module context
                        "Macro %s not found"
                        (prin1-to-string (car context-value))))))
         (t
          (setq context (append (list context-key context-value)
                                context))))))
     (t
      (setq context (append (list :type cur-context) context))))

    (when requirements-met
      (dolist (element raw-elements)
        (if (or (and (not ($is-quoted (car element)))
                     (consp (car element)))
                (keywordp (car element)))
            (progn
              ($install-chunk (nreverse elements) context module)
              (setq elements nil)
              ($install-component element context module))
          (push element elements)))
      ($install-chunk (nreverse elements) context module)
      (setq elements nil))))

(defun $install-modules (modules)
  "TODO"
  (dolist (module modules)
    (if (listp module)
        ($install-modules module)
      (let ((components (gethash (symbol-name module) $modules 'not-found)))
        (message "Installing module %s" (prin1-to-string module))
        (if (eq components 'not-found)
            ($warning module nil
                      "No definition found for module %s" (symbol-name module))
          (dolist (component components)
            ($install-component component nil module))
          (message "Done"))))))

(provide 'tommyx-config-framework)

;;; tommyx-config-framework.el ends here
