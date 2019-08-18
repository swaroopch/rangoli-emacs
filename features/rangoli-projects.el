;; rangoli-projects.el --- projects -*- lexical-binding: t; -*-

;; NOTE: When using counsel, you can insert the value of
;; ‘(ivy-thing-at-point)’ by hitting "M-n" in the minibuffer.

;;; Packages

(straight-use-package 'projectile)
(straight-use-package 'counsel-projectile)
(straight-use-package 'treemacs)
(straight-use-package 'treemacs-projectile)
(straight-use-package 'treemacs-magit)
(straight-use-package 'deadgrep)

;;; Projectile
(projectile-mode +1)
(diminish 'projectile-mode)
(rangoli/set-leader-key "p" 'projectile-command-map)
(setq projectile-switch-project-action #'projectile-commander)
(setq projectile-completion-system 'ivy)

(counsel-projectile-mode +1)

;;; Search with ripgrep
(rangoli/set-leader-key "/" 'counsel-projectile-rg "search")
(rangoli/set-leader-key "." 'deadgrep "ripgrep")

(defun rangoli/search-thing-at-point-in-project ()
  (interactive)
  (let ((counsel-projectile-rg-initial-input '(ivy-thing-at-point)))
    (counsel-projectile-rg)))
(rangoli/set-leader-key "*" 'rangoli/search-thing-at-point-in-project "search symbol")

;;; Treemacs
(rangoli/set-leader-key "f t" 'treemacs)
(rangoli/set-leader-key "0" 'treemacs-select-window)

;;; On startup
(run-with-idle-timer 0.1 nil 'projectile-cleanup-known-projects)

(provide 'rangoli-projects)
;; rangoli-projects.el ends here
