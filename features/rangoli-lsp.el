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

;; Inspired by
;; https://github.com/syl20bnr/spacemacs/blob/81d08a6b35a6043070b18f555d78ab945dd37be0/layers/+tools/lsp/packages.el#L28
(defun rangoli/setup-lsp-keybindings ()
  (rangoli/declare-prefix-for-mode major-mode "=" "format")
  (rangoli/set-local-leader-key "= b" #'lsp-format-buffer)
  (rangoli/set-local-leader-key "= r" #'lsp-format-region)

  (rangoli/declare-prefix-for-mode major-mode "g" "goto")
  (rangoli/set-local-leader-key "g M" #'lsp-ui-imenu)
  (rangoli/set-local-leader-key "g d" #'xref-find-definitions)
  (rangoli/set-local-leader-key "g e" #'lsp-treemacs-errors-list)
  (rangoli/set-local-leader-key "g i" #'lsp-find-implementation)
  (rangoli/set-local-leader-key "g p" #'xref-pop-marker-stack)
  (rangoli/set-local-leader-key "g r" #'xref-find-references)
  (rangoli/set-local-leader-key "g t" #'lsp-find-type-definition)

  (rangoli/declare-prefix-for-mode major-mode "h" "help")
  (rangoli/set-local-leader-key "h h" #'lsp-describe-thing-at-point)

  (rangoli/declare-prefix-for-mode major-mode "b" "backend")
  (rangoli/set-local-leader-key "b a" #'lsp-execute-code-action)
  (rangoli/set-local-leader-key "b d" #'lsp-describe-session)
  (rangoli/set-local-leader-key "b r" #'lsp-workspace-restart)
  (rangoli/set-local-leader-key "b s" #'lsp-workspace-shutdown)

  (rangoli/declare-prefix-for-mode major-mode "r" "refactor")
  (rangoli/set-local-leader-key "r r" #'lsp-rename)

  (rangoli/declare-prefix-for-mode major-mode "T" "toggle")
  (rangoli/set-local-leader-key "T d" #'lsp-ui-doc-mode)
  (rangoli/set-local-leader-key "T l" #'lsp-lens-mode)
  (rangoli/set-local-leader-key "T s" #'lsp-ui-sideline-mode)

  (rangoli/declare-prefix-for-mode major-mode "F" "folders")
  (rangoli/set-local-leader-key "F a" #'lsp-workspace-folders-add)
  (rangoli/set-local-leader-key "F o" #'lsp-workspace-folders-open)
  (rangoli/set-local-leader-key "F r" #'lsp-workspace-folders-remove))

(add-hook 'lsp-after-open-hook #'rangoli/setup-lsp-keybindings)

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


