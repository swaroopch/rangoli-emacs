;; rangoli-python.el --- python programming functionality -*- lexical-binding: t; -*-

;;; Packages

(require 'python)
(require 'lsp-pyls)

;;; pyenv
(defvar rangoli/pyenv-root (f-expand "~/.pyenv/"))

(defvar rangoli/has-pyenv
  (f-directory? rangoli/pyenv-root)
  "Is pyenv installed?")

(when rangoli/has-pyenv
  (setenv "WORKON_HOME" (f-join rangoli/pyenv-root "versions"))  ;; Used by `pyvenv-workon'
  (add-to-list 'python-shell-exec-path (f-join rangoli/pyenv-root "shims"))
  (setq flycheck-python-pycompile-executable  (f-join rangoli/pyenv-root "shims/python")
        flycheck-python-flake8-executable (f-join rangoli/pyenv-root "shims/python")
        flycheck-python-pylint-executable (f-join rangoli/pyenv-root "shims/python")
        flycheck-python-mypy-executable (f-join rangoli/pyenv-root "shims/mypy")
        lsp-pyls-server-command (f-join rangoli/pyenv-root "shims/pyls")))

;;; python3, when not using pyenv
(when (not rangoli/has-pyenv)
  (setq python-shell-interpreter "python3"
        flycheck-python-pycompile-executable "python3"
        flycheck-python-flake8-executable "python3"
        flycheck-python-pylint-executable "python3"))

;;; lsp
(setq lsp-pyls-plugins-pylint-enabled nil
      lsp-pyls-plugins-pycodestyle-max-line-length 120)

;;; virtualenv
(straight-use-package 'pyvenv)
(add-hook 'python-mode-hook
          (lambda ()
            (pyvenv-mode)  ;; Loads venv as per `pyvenv-workon' in directory local variables
            (rangoli/declare-prefix-for-mode "v" "virtualenv")
            (rangoli/set-local-leader-key "v a" 'pyvenv-activate "activate")
            (rangoli/set-local-leader-key "v d" 'pyvenv-deactivate "deactivate")
            (when rangoli/has-pyenv
              (rangoli/set-local-leader-key "v p" 'pyvenv-workon "pick pyenv"))))

;;; DAP

;; https://www.reddit.com/r/emacs/comments/bxhod6/where_do_i_find_templates_for_dapmode/eq6rm65
(require 'dap-python)

(provide 'rangoli-python)
;; rangoli-python.el ends here
