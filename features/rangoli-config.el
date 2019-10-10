;; rangoli-config.el --- core config -*- lexical-binding: t; -*-

(defvar rangoli/home-dir
  (f-expand "~/notes/")
  "/Private/ home directory.")

;; I use a separate repository for highly-specific elisp code.
(defvar rangoli/private-emacs-config-dir
  (f-expand "~/.config/emacs-private/")
  "/Private/ emacs repository.")

(provide 'rangoli-config)
;; rangoli-config.el ends here

