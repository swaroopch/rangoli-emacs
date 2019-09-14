;; rangoli-programming.el --- general programming functionality -*- lexical-binding: t; -*-

;;; Language Modes

(straight-use-package 'yaml-mode)
(straight-use-package 'toml-mode)
(straight-use-package 'ssh-config-mode)

;;; Junk file

(straight-use-package 'open-junk-file)

(require 'open-junk-file)
(setq open-junk-file-format (f-join user-emacs-directory "junk/%Y/%m/%d-%H%M%S."))
(rangoli/set-leader-key "f J" 'open-junk-file "junk")

;;; smartparens

(straight-use-package 'smartparens)

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

(straight-use-package 'rainbow-delimiters)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;; flycheck
;; https://www.flycheck.org/en/latest/user/quickstart.html

(straight-use-package 'flycheck)

(add-hook 'prog-mode-hook #'flycheck-mode)

(with-eval-after-load 'flycheck
  (setq flycheck-emacs-args
        (list "-Q" "--batch" "-l" (f-join user-emacs-directory "init.el")))
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(rangoli/set-leader-key "T e" 'flycheck-list-errors "errors (flycheck)")

;;; company

(straight-use-package 'company)
(straight-use-package 'company-prescient)

(require 'company)
(diminish 'company-mode)
(add-hook 'prog-mode-hook #'company-mode)
(company-prescient-mode)

;;; dumb-jump

(straight-use-package 'dumb-jump)

(require 'dumb-jump)
(setq dumb-jump-selector 'ivy)
(setq dumb-jump-prefer-searcher 'rg)
(add-hook 'prog-mode-hook 'dumb-jump-mode)

;;; highlight todos

(straight-use-package 'hl-todo)
(add-hook 'prog-mode-hook #'hl-todo-mode)
(defhydra rangoli/hl-todo-navigate ()
  "navigate todos"
  ("n" hl-todo-next "next")
  ("p" hl-todo-previous "previous")
  ("o" hl-todo-occur "occur" :exit t)
  ("q" nil "quit"))
(rangoli/set-leader-key "j t" 'rangoli/hl-todo-navigate/body "todo")

;;; focus mode

(straight-use-package 'focus)
(rangoli/set-leader-key "T f" 'focus-mode "focus")

(provide 'rangoli-programming)
;; rangoli-programming.el ends here
