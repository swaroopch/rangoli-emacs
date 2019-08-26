;; rangoli-projects.el --- projects -*- lexical-binding: t; -*-

;; NOTE: When using ivy/counsel, you can insert the value of
;; ‘(ivy-thing-at-point)’ by hitting "M-n" in the minibuffer.
;; https://oremacs.com/swiper/#key-bindings-that-alter-the-minibuffer-input
;;
;; NOTE: When using ivy/counsel, you can convert current completion
;; session into a buffer by hitting "C-c C-o"
;; https://oremacs.com/swiper/#saving-the-current-completion-session-to-a-buffer

;;; Projectile

(straight-use-package 'projectile)
(projectile-mode +1)
(diminish 'projectile-mode)
(rangoli/set-leader-key "p" 'projectile-command-map)
(setq projectile-switch-project-action #'projectile-commander)
(setq projectile-completion-system 'ivy)

;;; Search with ripgrep

(straight-use-package 'ripgrep)
(def-projectile-commander-method ?i
  "Run rg (ripgrep) on project."
    (call-interactively 'projectile-ripgrep))
(rangoli/set-leader-key "/" 'projectile-ripgrep "search project")
(rangoli/set-leader-key "." 'ripgrep-regexp "search directory")

;;; Treemacs

(straight-use-package 'treemacs)
(straight-use-package 'treemacs-projectile)
(straight-use-package 'treemacs-magit)

(rangoli/set-leader-key "f t" 'treemacs)
(rangoli/set-leader-key "0" 'treemacs-select-window)

;;; On startup
(run-with-idle-timer 0.1 nil 'projectile-cleanup-known-projects)

(provide 'rangoli-projects)
;; rangoli-projects.el ends here
