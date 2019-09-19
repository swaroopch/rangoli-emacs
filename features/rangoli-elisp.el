;; rangoli-elisp.el --- elisp programming functionality -*- lexical-binding: t; -*-

;;; Syntax-highlighting for `Cask' files

;; https://cask.readthedocs.io/en/latest/guide/dsl.html
(add-to-list 'auto-mode-alist '("Cask" . lisp-mode))

;;; Jump to website of a elisp package

;; Merged upstream : https://github.com/raxod502/straight.el/pull/414
(rangoli/set-leader-key "j e" 'straight-visit-package-website "open website of package")

(provide 'rangoli-elisp)
;; rangoli-elisp.el ends here
