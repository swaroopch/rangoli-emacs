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

(setq git-link-use-commit t)

(defun rangoli/jump-git-link-commit ()
  (interactive)
  (let ((git-link-open-in-browser t))
    (call-interactively 'git-link-commit)))

(defun rangoli/jump-git-link ()
  (interactive)
  (let ((git-link-open-in-browser t))
    (call-interactively 'git-link)))

(defun rangoli/jump-git-link-homepage ()
  (interactive)
  (let ((git-link-open-in-browser t))
    (call-interactively 'git-link-homepage)))

(rangoli/declare-prefix "g l" "link")
(rangoli/set-leader-key "g l c" 'git-link-commit "copy link to commit")
(rangoli/set-leader-key "g l C" 'rangoli/jump-git-link-commit "jump to commit")
(rangoli/set-leader-key "g l l" 'git-link "copy link to line")
(rangoli/set-leader-key "g l L" 'rangoli/jump-git-link "jump to line")
(rangoli/set-leader-key "g l h" 'git-link-homepage "copy link to homepage")
(rangoli/set-leader-key "g l H" 'rangoli/jump-git-link-homepage "jump to homepage")

(provide 'rangoli-git)
;; rangoli-git.el ends here
