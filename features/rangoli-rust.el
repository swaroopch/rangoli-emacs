;; rangoli-rust.el --- rust language configuration -*- lexical-binding: t; -*-

;; https://github.com/rust-lang/rust-mode
(straight-use-package 'rust-mode)
(require 'rust-mode)

;; https://github.com/rust-lang/rust-mode#rustfmt
(setq rust-format-on-save t)

(add-hook 'rust-mode-hook
          (lambda ()
            ;; https://github.com/rust-lang/rust-mode#indentation
            (setq indent-tabs-mode nil)))

;; https://github.com/flycheck/flycheck-rust
(straight-use-package 'flycheck-rust)

(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; https://github.com/kwrooijen/cargo.el
(straight-use-package 'cargo)
(add-hook 'rust-mode-hook 'cargo-minor-mode)

(provide 'rangoli-rust)
;; rangoli-rust.el ends here


