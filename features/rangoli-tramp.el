;; rangoli-tramp.el --- TRAMP configuration -*- lexical-binding: t; -*-

(straight-use-package 'counsel-tramp)
(straight-use-package 'vagrant-tramp)
(straight-use-package 'docker-tramp)

;; https://github.com/masasam/emacs-counsel-tramp#sample-configuration
(setq tramp-default-method "ssh")

(when (featurep 'projectile)
  ;; https://github.com/masasam/emacs-counsel-tramp#if-you-want-to-speed-up-tramp
  (add-hook 'counsel-tramp-pre-command-hook '(lambda ()
                                               (projectile-mode 0)
                                               ))
  (add-hook 'counsel-tramp-quit-hook '(lambda ()
                                        (projectile-mode 1)
                                        )))

;; Disable version control when using tramp
;; https://www.gnu.org/software/tramp/#Frequently-Asked-Questions
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

(rangoli/set-leader-key "f F" 'counsel-tramp "remote file")

(provide 'rangoli-tramp)
;; rangoli-tramp.el ends here


