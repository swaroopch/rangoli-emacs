;; rangoli-python.el --- python programming functionality -*- lexical-binding: t; -*-

;;; LSP
(straight-use-package 'lsp-python-ms)
;; TODO https://github.com/microsoft/python-language-server/blob/master/TROUBLESHOOTING.md#unresolved-import-warnings
(require 'lsp-python-ms)

;;; pyenv
(require 'python)
(when (f-directory? (f-expand "~/.pyenv/shims/"))
  (add-to-list 'python-shell-exec-path (f-expand "~/.pyenv/shims/"))
  (setq flycheck-python-pycompile-executable  (f-expand "~/.pyenv/shims/python")
        flycheck-python-flake8-executable (f-expand "~/.pyenv/shims/python")
        flycheck-python-pylint-executable (f-expand "~/.pyenv/shims/python")
        flycheck-python-mypy-executable (f-expand "~/.pyenv/shims/mypy")))

;;; python3, when not using pyenv
(when (not (f-directory? (f-expand "~/.pyenv/shims/")))
  (setq python-shell-interpreter "python3"
        flycheck-python-pycompile-executable "python3"
        flycheck-python-flake8-executable "python3"
        flycheck-python-pylint-executable "python3"))

;;; DAP
(require 'dap-python)

;;; virtualenv
(straight-use-package 'pythonic)
(add-hook 'python-mode-hook
          (lambda ()
            (rangoli/declare-prefix-for-mode "v" "virtualenv")
            (rangoli/set-local-leader-key "v a" 'pythonic-activate "activate")
            (rangoli/set-local-leader-key "v d" 'pythonic-deactivate "deactivate")))

(provide 'rangoli-python)
;; rangoli-python.el ends here
