;; rangoli-python.el --- python programming functionality -*- lexical-binding: t; -*-

;;; Packages

(straight-use-package 'pythonic)

;;; pyenv
(require 'python)
(require 'lsp-pyls)
(when (f-directory? (f-expand "~/.pyenv/shims/"))
  (add-to-list 'python-shell-exec-path (f-expand "~/.pyenv/shims/"))
  (setq flycheck-python-pycompile-executable  (f-expand "~/.pyenv/shims/python")
        flycheck-python-flake8-executable (f-expand "~/.pyenv/shims/python")
        flycheck-python-pylint-executable (f-expand "~/.pyenv/shims/python")
        flycheck-python-mypy-executable (f-expand "~/.pyenv/shims/mypy")
        lsp-pyls-server-command (f-expand "~/.pyenv/shims/pyls")))

;;; python3, when not using pyenv
(when (not (f-directory? (f-expand "~/.pyenv/shims/")))
  (setq python-shell-interpreter "python3"
        flycheck-python-pycompile-executable "python3"
        flycheck-python-flake8-executable "python3"
        flycheck-python-pylint-executable "python3"))

;;; lsp
(setq lsp-pyls-plugins-pylint-enabled nil
      lsp-pyls-plugins-pycodestyle-max-line-length 120)

;;; virtualenv
(add-hook 'python-mode-hook
          (lambda ()
            (rangoli/set-local-leader-key "v" 'pythonic-activate "virtualenv activate")
            (rangoli/set-local-leader-key "V" 'pythonic-deactivate "virtualenv deactivate")))

;;; TODO : LSP + vagrant using tramp
;; TODO : How to make this work automatically on opening a remote Python file?
;;
;; This currently hangs, e.g. see buffer `*lsp-log: pyls-remote:18578*'
;;
;; https://github.com/emacs-lsp/lsp-mode#tramp
;; https://github.com/emacs-lsp/lsp-mode/issues/843
(defun rangoli/lsp-vagrant-python (vagrant-box-name)
  (interactive "sVagrantBox: ")
  (pythonic-activate (s-format "/vagrant:$0:/vagrant/venv/" 'elt (list vagrant-box-name)))
  (lsp-register-client
        (make-lsp-client :new-connection (lsp-tramp-connection "python")
                         :major-modes '(python-mode)
                         :remote? t
                         :server-id 'pyls-remote)))

(add-hook 'python-mode-hook
          (lambda ()
            (rangoli/set-local-leader-key "l" 'rangoli/lsp-vagrant-python "lsp vagrant")))

;;; DAP

;; https://www.reddit.com/r/emacs/comments/bxhod6/where_do_i_find_templates_for_dapmode/eq6rm65
(require 'dap-python)

(provide 'rangoli-python)
;; rangoli-python.el ends here
