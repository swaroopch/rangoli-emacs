;; rangoli-lsp.el --- LSP configuration -*- lexical-binding: t; -*-

;;; LSP (language server protocol)
;; https://microsoft.github.io/language-server-protocol/specification
;; https://github.com/emacs-lsp/lsp-mode
;; https://github.com/emacs-lsp/lsp-ui
;; https://github.com/tigersoldier/company-lsp
;; https://github.com/emacs-lsp/dap-mode

(straight-use-package 'lsp-mode)
(require 'lsp-mode)
(setq lsp-prefer-flymake nil
      lsp-auto-guess-root t
      lsp-restart 'ignore
      ;; for debugging, see `*lsp-log*' buffer
      lsp-log-io t
      lsp-print-performance t)
(add-hook 'prog-mode-hook #'lsp)

(straight-use-package 'lsp-ui)
(require 'lsp-ui)
(add-hook 'lsp-mode-hook #'lsp-ui-mode)
;; SOMEDAY/MAYBE [2019-06-01] Currently, this renders blank windows
;; (setq lsp-ui-doc-use-webkit t)

(straight-use-package 'company-lsp)
(require 'company-lsp)
(push 'company-lsp company-backends)

(straight-use-package 'lsp-treemacs)
(require 'lsp-treemacs)

;;; DAP

(straight-use-package 'dap-mode)
(dap-mode 1)
(dap-ui-mode 1)

(provide 'rangoli-lsp)
;; rangoli-lsp.el ends here


