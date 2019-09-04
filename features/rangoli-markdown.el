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

(require 'markdown-mode)

;; Setup preview command
(cond
 ;; brew install cmark-gfm
 ;; sudo apt install cmark-gfm
 ((executable-find "cmark-gfm")
  (setq markdown-command "cmark-gfm"))
 ;; npm install -g marked
 ((executable-find "marked")
  (setq markdown-command "marked"))
 ;; no preview command available
 (t
  (error "Markdown preview command unavailable")))

(provide 'rangoli-markdown)
;; rangoli-markdown.el ends here
