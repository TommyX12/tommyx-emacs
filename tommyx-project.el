;; requires
(require 'projectile)

;; general config
(setq compile-command "")

;; configs
(setq projectile-enable-caching t)
(projectile-mode)
; neotree integration
(setq projectile-switch-project-action 'neotree-projectile-action)
(setq projectile-globally-ignored-directories (append '("node_modules" "dist" "bin" "build") projectile-globally-ignored-directories))

;; bug fix
;; (setq projectile-project-compilation-cmd "")

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
	:run #'project-cp-run
)

;; key bindings
(general-define-key
	:keymaps 'override
	:states '(motion normal)
	:prefix "SPC"
	:non-normal-prefix "M-SPC"

	"pp" '(counsel-projectile-switch-project
		:which-key "switch project")
	"pw" '(persp-switch
		:which-key "switch workspace")
	"pn" '(persp-rename
		:which-key "rename workspace")
	"pd" '(persp-kill
		:which-key "delete workspace")
	"pR" '(projectile-invalidate-cache
		:which-key "re-index project files")

	"pr" '(projectile-run-project
		:which-key "run project")
	"pc" '(compile
		:which-key "compile")
	"pC" '(projectile-compile-project
		:which-key "compile project")

)
