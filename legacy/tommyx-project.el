;; requires
(require 'projectile)
(require 'tommyx-bind-def)

;; general config
(setq compile-command "")

;; configs
(setq projectile-enable-caching t)
(projectile-mode)
;; neotree integration
(setq projectile-switch-project-action 'neotree-projectile-action)
(setq projectile-globally-ignored-directories (append '("node_modules" "dist" "bin" "build") projectile-globally-ignored-directories))

;; TODO: temporary bug fix
(setq projectile-project-compilation-cmd "")

;; helper functions
(defun project/project-relative-name ()
	"Return the project-relative file name of 'buffer-file-name'."
	(ignore-errors (file-relative-name
									buffer-file-name (projectile-project-root))))

;; project definitions
(defun project-cp-run ()
  "Return a String representing the compile command to run for the given context."
  (cond
   ((eq major-mode 'c++-mode)
    (concat (if (eq system-type 'windows-nt)
								"cl /EHsc /W4 /out:a.exe \""
							"g++ -Wall -std=c++11 -o a.exe \"")
						(project/project-relative-name)
						"\""))
   ((eq major-mode 'python-mode)
    (concat "python \""
						(project/project-relative-name)
						"\""))
   ))
(projectile-register-project-type 'cp '("cp.txt")
	                                :compile #'project-cp-run
	                                :run #'project-cp-run)

;; key bindings
(tommyx-bind-keys
 `(:case
   :states (motion normal visual)
   
	 (:bindings

    project-prefix
    (:bindings

	   "p" (:def
          counsel-projectile-switch-project
		      :which-key "Switch Project")
	   "w" (:def
          persp-switch
		      :which-key "Switch Workspace")
	   "n" (:def
          persp-rename
		      :which-key "Rename Workspace")
	   "d" (:def
          persp-kill
		      :which-key "Delete Workspace")
	   "R" (:def
          projectile-invalidate-cache
		      :which-key "Re-index Project Files")
	   "r" (:def
          projectile-run-project
		      :which-key "Run Project")
	   "c" (:def
          compile
		      :which-key "Compile")
	   "C" (:def
          projectile-compile-project
		      :which-key "Compile Project")
     "C-c" (:def
            kill-compilation
            :which-key "Kill Compilation")
     "M-c" (:def
            ,(lambda () (interactive)
               (switch-to-buffer-other-window "*compilation*"))
            :which-key "Go To Compilation")
     "t" (:def
          ansi-term
          :which-key "Open Terminal")
     "T" (:def
          projectile-run-term
          :which-key "Open Project Terminal")
     "e" (:def
          projectile-run-eshell
          :which-key "Open Project Eshell")
     "s" (:def
          shell-command
          :which-key "Shell Command")))))


(provide 'tommyx-project)

;;; tommyx-project.el ends here
