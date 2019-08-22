;; rangoli-programming.el --- general programming functionality -*- lexical-binding: t; -*-

;;; Packages

(straight-use-package 'open-junk-file)
(straight-use-package 'smartparens)
(straight-use-package 'rainbow-delimiters)
(straight-use-package 'flycheck)
(straight-use-package 'company)
(straight-use-package 'highlight-symbol)
(straight-use-package 'counsel-tramp)
(straight-use-package 'vagrant-tramp)
(straight-use-package 'docker-tramp)
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'company-lsp)
(straight-use-package 'company-prescient)
(straight-use-package 'dap-mode)
(straight-use-package 'yaml-mode)
(straight-use-package 'toml-mode)
(straight-use-package 'ssh-config-mode)
(straight-use-package 'dumb-jump)

;;; Junk file
(require 'open-junk-file)
(setq open-junk-file-format (f-join user-emacs-directory "junk/%Y/%m/%d-%H%M%S."))
(rangoli/set-leader-key "f J" 'open-junk-file "junk")

;;; smartparens
(require 'smartparens-config)
(diminish 'smartparens-mode)
(add-hook 'prog-mode-hook #'smartparens-strict-mode)

;; https://github.com/Fuco1/.emacs.d/blob/master/files/smartparens.el
(define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
(define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)

(define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
(define-key smartparens-mode-map (kbd "C-M-a") 'sp-backward-down-sexp)
(define-key smartparens-mode-map (kbd "C-S-d") 'sp-beginning-of-sexp)
(define-key smartparens-mode-map (kbd "C-S-a") 'sp-end-of-sexp)

(define-key smartparens-mode-map (kbd "C-M-e") 'sp-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp)

(define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
(define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)

(define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-<right>") 'sp-backward-barf-sexp)

(define-key smartparens-mode-map (kbd "M-D") 'sp-splice-sexp)

(define-key smartparens-mode-map (kbd "C-]") 'sp-select-next-thing-exchange)
(define-key smartparens-mode-map (kbd "C-<left_bracket>") 'sp-select-previous-thing)
(define-key smartparens-mode-map (kbd "C-M-]") 'sp-select-next-thing)

(define-key smartparens-mode-map (kbd "M-F") 'sp-forward-symbol)
(define-key smartparens-mode-map (kbd "M-B") 'sp-backward-symbol)

(define-key smartparens-mode-map (kbd "C-\"") 'sp-change-inner)
(define-key smartparens-mode-map (kbd "M-i") 'sp-change-enclosing)

;;; rainbow-delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;; flycheck
;; https://www.flycheck.org/en/latest/user/quickstart.html

(add-hook 'prog-mode-hook #'flycheck-mode)

(with-eval-after-load 'flycheck
  (setq flycheck-emacs-args '("-Q" "--batch" "-l" "~/.emacs.d/init.el"))
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;;; company
(require 'company)
(diminish 'company-mode)
(add-hook 'prog-mode-hook #'company-mode)
(company-prescient-mode)

;;; highlight-symbol

(require 'highlight-symbol)
(setq highlight-symbol-idle-delay 0.5)
(defhydra rangoli/hydra-highlight-symbol ()
  "highlight-symbol"
  ("n" highlight-symbol-next "next")
  ("p" highlight-symbol-prev "previous")
  ("q" nil "quit"))
(add-hook 'prog-mode-hook
          (lambda ()
            (highlight-symbol-mode)
            (diminish 'highlight-symbol-mode)
            (rangoli/set-local-leader-key "h" 'rangoli/hydra-highlight-symbol/body "highlight symbol")))

;;; tramp
(rangoli/set-leader-key "f F" 'counsel-tramp "remote file")

;; https://github.com/masasam/emacs-counsel-tramp#sample-configuration
(setq tramp-default-method "ssh")

;; https://github.com/masasam/emacs-counsel-tramp#if-you-want-to-speed-up-tramp
(add-hook 'counsel-tramp-pre-command-hook '(lambda ()
                                             (projectile-mode 0)
                                             ))
(add-hook 'counsel-tramp-quit-hook '(lambda ()
                                      (projectile-mode 1)
                                      ))

;; Disable version control when using tramp
;; https://www.gnu.org/software/tramp/#Frequently-Asked-Questions
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;;; LSP (language server protocol)
;; https://microsoft.github.io/language-server-protocol/specification
;; https://github.com/emacs-lsp/lsp-mode
;; https://github.com/emacs-lsp/lsp-ui
;; https://github.com/tigersoldier/company-lsp
;; https://github.com/emacs-lsp/dap-mode

(require 'lsp-mode)
(setq lsp-prefer-flymake nil
      lsp-auto-guess-root t
      lsp-restart 'ignore
      ;; for debugging, see `*lsp-log*' buffer
      lsp-log-io t
      lsp-print-performance t)
(add-hook 'prog-mode-hook #'lsp)

(require 'lsp-ui)
(add-hook 'lsp-mode-hook #'lsp-ui-mode)
;; SOMEDAY/MAYBE [2019-06-01] Currently, this renders blank windows
;; (setq lsp-ui-doc-use-webkit t)

(require 'company-lsp)
(push 'company-lsp company-backends)

;;; DAP

(dap-mode 1)
(dap-ui-mode 1)

;;; dumb-jump

(require 'dumb-jump)
(setq dumb-jump-selector 'ivy)
(setq dumb-jump-prefer-searcher 'rg)
(add-hook 'prog-mode-hook 'dumb-jump-mode)

;;; emacs lisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (rangoli/set-local-leader-key "j" 'counsel-outline "jump")))


;;; eshell
;; TODO Fix error `local-set-key: Key sequence M-n l starts with non-prefix key M-n'
;; (add-hook 'eshell-mode-hook
;;           '(lambda ()
;;              (rangoli/set-local-leader-key "l" 'eshell/clear "clear")))

(provide 'rangoli-programming)
;; rangoli-programming.el ends here
