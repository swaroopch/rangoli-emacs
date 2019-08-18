;; rangoli-swift.el --- swift programming functionality -*- lexical-binding: t; -*-
;;
;; Install Swift "Trunk Development (master)" snapshot from https://swift.org/download/#snapshots
;; Follow instructions at https://github.com/apple/sourcekit-lsp#building-sourcekit-lsp
;;
;; https://github.com/emacs-lsp/lsp-sourcekit

;;; Packages

(straight-use-package 'swift-mode)
(straight-use-package 'flycheck-swift)
(straight-use-package 'lsp-sourcekit)

;;; SourceKit

(defvar rangoli/sourcekit-toolchain-path
  (f-expand
   (pcase system-type
     ('darwin "/Library/Developer/Toolchains/swift-latest.xctoolchain")
     ('gnu/linux "~/.swift-toolchain"))))

(require 'lsp-sourcekit)
(setenv "SOURCEKIT_TOOLCHAIN_PATH" rangoli/sourcekit-toolchain-path)
(setq swift-mode:repl-executable (f-join rangoli/sourcekit-toolchain-path "usr/bin/swift"))
(setq lsp-sourcekit-executable (f-expand "~/code/sourcekit-lsp/.build/debug/sourcekit-lsp"))

;;; Flycheck

(require 'flycheck-swift)
(setq flycheck-swift-executable (f-join rangoli/sourcekit-toolchain-path "usr/bin/swiftc"))
(eval-after-load 'flycheck '(flycheck-swift-setup))

(provide 'rangoli-swift)
;; rangoli-swift.el ends here
