;; rangoli-text.el --- text related functionality -*- lexical-binding: t; -*-

;;; copy-as-format

(straight-use-package 'copy-as-format)

(rangoli/declare-prefix "t c" "copy as format")
(rangoli/set-leader-key "t c j" 'copy-as-format-jira "jira")
(rangoli/set-leader-key "t c m" 'copy-as-format-markdown "markdown")
(rangoli/set-leader-key "t c o" 'copy-as-format-org-mode "org-mode")
(rangoli/set-leader-key "t c s" 'copy-as-format-slack "slack")
(rangoli/declare-prefix "t c g" "git")
(rangoli/set-leader-key "t c g h" 'copy-as-format-github "github")
(rangoli/set-leader-key "t c g l" 'copy-as-format-gitlab "gitlab")

(provide 'rangoli-text)
;; rangoli-text.el ends here
