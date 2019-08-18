;; rangoli-markdown.el --- markdown -*- lexical-binding: t; -*-

;;; Packages

;; markdown-mode - To enable editing of code blocks in indirect buffers using `C-c '`
(straight-use-package 'edit-indirect)
(straight-use-package 'markdown-mode)

;;; Markdown

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(provide 'rangoli-markdown)
;; rangoli-markdown.el ends here
