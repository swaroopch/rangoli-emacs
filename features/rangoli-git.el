;; rangoli-git.el --- git -*- lexical-binding: t; -*-

;;; Packages

(straight-use-package 'magit)
(straight-use-package 'gitconfig-mode)
(straight-use-package 'forge)

;;; GPG
;; https://help.github.com/en/articles/managing-commit-signature-verification
;; https://docs.gitlab.com/ee/user/project/repository/gpg_signed_commits/

;; https://magit.vc/manual/ghub.html#How-Ghub-uses-Auth_002dSource
(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))

;;; Magit
;; https://magit.vc/manual/magit.html

(require 'magit)
(rangoli/set-leader-key "g s" 'magit-status)
(rangoli/set-leader-key "g l" 'magit-log-head)

;;; Forge (GitHub, GitLab)
;; https://magit.vc/manual/forge.html
;; https://magit.vc/manual/ghub.html#Manually-Creating-and-Storing-a-Token

(with-eval-after-load 'magit
  (require 'forge))

(provide 'rangoli-git)
;; rangoli-git.el ends here
