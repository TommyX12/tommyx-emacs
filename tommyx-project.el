;; requires
(require 'projectile)

;; configs
(setq projectile-enable-caching t)
(projectile-mode)
; neotree integration
(setq projectile-switch-project-action 'neotree-projectile-action)
(setq projectile-globally-ignored-directories (append '("node_modules" "dist" "bin" "build") projectile-globally-ignored-directories))

;; helper functions
(defun project/project-relative-name ()
	"Return the project-relative file name of 'buffer-file-name'."
	(ignore-errors (file-relative-name
									buffer-file-name (projectile-project-root))))

;; project definitions
(defun project/cp/run ()
  "Return a String representing the compile command to run for the given context."
  (cond
   ((eq major-mode 'c++-mode)
    (concat "g++ -Wall -std=c++11 -o a.exe " (project/project-relative-name)))
   ((eq major-mode 'python-mode)
    (concat "python " (project/project-relative-name)))
   ))
(projectile-register-project-type 'cp '("cp.txt")
	:compile 'project/cp/run
	:run 'project/cp/run
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
	"pr" '(projectile-invalidate-cache
		:which-key "re-index project files")

)
