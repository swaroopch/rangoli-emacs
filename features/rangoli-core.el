;; rangoli-core.el --- core functionality -*- lexical-binding: t; -*-

;;; Startup optimizations
;; https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast
;; https://github.com/hlissner/doom-emacs/blob/develop/init.el
;; Also see `early-init.el'

(run-with-idle-timer
 3 nil (lambda () (setq gc-cons-threshold (* 20 1024 1024))))

(defvar rangoli/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'after-init-hook (lambda () (setq file-name-handler-alist rangoli/file-name-handler-alist)))

;;; Encoding
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(setq file-name-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(setq coding-system-for-write 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8))

;;; Packages

;; strings
(straight-use-package 's)
;; lists
(straight-use-package 'dash)
;; hash tables
(straight-use-package 'ht)
;; files and directories
(straight-use-package 'f)
;; date time
(straight-use-package '(ts :type git :host github :repo "alphapapa/ts.el"))
;; http requests
(straight-use-package 'request)
;; executables path
(straight-use-package 'exec-path-from-shell)

;;; Require Common Packages

(require 's)
(require 'dash)
(require 'ht)
(require 'f)
(require 'ts)
(require 'request)

;;; executables path
(exec-path-from-shell-initialize)

(provide 'rangoli-core)
;; rangoli-core.el ends here
