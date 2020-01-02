;;; Straight bootstrap
;; https://github.com/raxod502/straight.el

(setq straight-repository-branch "develop"
      straight-enable-use-package-integration nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; Features

(add-to-list 'load-path (concat user-emacs-directory "features/"))

;; You /will/ need these

(require 'rangoli-core)
(require 'rangoli-config)
(require 'rangoli-keybindings)
(require 'rangoli-ui)

;; You /may/ want these

(require 'rangoli-ui-theme)
(require 'rangoli-ui-font)
(require 'rangoli-snippet)
(require 'rangoli-org)
(require 'rangoli-markdown)
(require 'rangoli-pdf)

(require 'rangoli-timer)
(require 'rangoli-websearch)
(require 'rangoli-emoji)

(require 'rangoli-reading)
(require 'rangoli-text)
(require 'rangoli-annotate)

(require 'rangoli-git)
(require 'rangoli-projects)

(require 'rangoli-programming)
(require 'rangoli-lsp)
(require 'rangoli-docker)
(require 'rangoli-tramp)
(require 'rangoli-elisp)
(require 'rangoli-rust)
(require 'rangoli-cpp)
(require 'rangoli-python)
(require 'rangoli-swift)
(require 'rangoli-java)

;; You probably /do not/ want these

;; (require 'swa-org)
;; (require 'swa-bookmarks)
;; (require 'swa-personal)

(let ((rangoli-private-config (f-join rangoli/private-emacs-config-dir "init.el")))
  (when (f-exists? rangoli-private-config)
    (load rangoli-private-config)))
