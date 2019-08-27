;; rangoli-git.el --- git -*- lexical-binding: t; -*-

;;; Packages

(straight-use-package 'magit)
(straight-use-package 'gitconfig-mode)
(straight-use-package 'forge)
(straight-use-package 'git-link)

;;; GPG
;; https://help.github.com/en/articles/managing-commit-signature-verification
;; https://docs.gitlab.com/ee/user/project/repository/gpg_signed_commits/

;; https://magit.vc/manual/ghub.html#How-Ghub-uses-Auth_002dSource
(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))

;;; Magit
;; https://magit.vc/manual/magit.html

(require 'magit)
(rangoli/set-leader-key "g s" 'magit-status "status")
(rangoli/set-leader-key "g h" 'magit-log-head "history")

;;; Forge (GitHub, GitLab)
;; https://magit.vc/manual/forge.html
;; https://magit.vc/manual/ghub.html#Manually-Creating-and-Storing-a-Token

(with-eval-after-load 'magit
  (require 'forge))

;;; Git Link

(require 'git-link)
(rangoli/declare-prefix "g l" "link")
(rangoli/set-leader-key "g l c" 'git-link-commit)
(rangoli/set-leader-key "g l l" 'git-link)
(rangoli/set-leader-key "g l h" 'git-link-homepage)

(provide 'rangoli-git)
;; rangoli-git.el ends here
