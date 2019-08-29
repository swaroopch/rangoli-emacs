;; rangoli-elisp.el --- elisp programming functionality -*- lexical-binding: t; -*-

;;; Syntax-highlighting for `Cask' files

;; https://cask.readthedocs.io/en/latest/guide/dsl.html
(add-to-list 'auto-mode-alist '("Cask" . lisp-mode))

;;; Jump to website of a elisp package

(defun rangoli/elisp-package-website ()
  (interactive)
  (let* ((melpa-recipe (straight-get-recipe))
         (recipe (straight--convert-recipe melpa-recipe))
         (host (plist-get recipe :host))
         (repo (plist-get recipe :repo)))
    (pcase host
      ('github (browse-url (s-lex-format "https://github.com/${repo}")))
      ('gitlab (browse-url (s-lex-format "https://gitlab.com/${repo}")))
      (_ (error (s-lex-format "Not implemented yet : ${host}"))))))

(rangoli/set-leader-key "g e o" 'rangoli/elisp-package-website "open website of package")

(provide 'rangoli-elisp)
;; rangoli-elisp.el ends here
