;; TODO: refactor status line, project, org, to here.

;; emergency key setting for debug
(global-set-key (kbd "C-M-x") 'execute-extended-command)

;;; startup appearance


;;; main

(require 'tommyx-config-framework)
(require 'tommyx-packages)
(require 'tommyx-main-modules)
(require 'tommyx-key-binding-modules)

(defconst tommyx-main-modules-list
  (list 'tommyx-appearance
        'tommyx-main
        'tommyx-default-major-modes
        'tommyx-log-modes
        'tommyx-emacs-lisp-mode
        'tommyx-sh-mode
        'tommyx-sgml-mode
        'tommyx-emacs-internal-modes
        'tommyx-emms-playlist-mode
        'tommyx-neotree-mode
        'tommyx-imenu-list-mode
        'tommyx-dashboard-mode
        'tommyx-shaderlab-mode
        'tommyx-latex-mode
        'tommyx-protobuf-mode
        'tommyx-java-mode
        'tommyx-c++-mode
        'tommyx-r-mode
        'tommyx-csharp-mode
        'tommyx-glsl-mode
        'tommyx-json-mode
        'tommyx-html-mode
        'tommyx-css-mode
        'tommyx-racket-mode
        'tommyx-haskell-mode
        'tommyx-csv-mode
        'tommyx-sql-mode
        'tommyx-python-mode
        'tommyx-javascript-mode
        'tommyx-typescript-mode
        'tommyx-term-mode
        'tommyx-compilation-mode
        'tommyx-help-mode
        'tommyx-message-mode
        'tommyx-org-mode))

(defconst tommyx-key-binding-modules-list
  (list 'tommyx-key-bindings))

;;; extra things not yet refactored

(require 'tommyx-status-lines)
(require 'tommyx-project)


(provide 'tommyx)

;;; tommyx.el ends here
