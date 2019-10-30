;; rangoli-config.el --- core config -*- lexical-binding: t; -*-

(defvar rangoli/notes-dir
  (f-expand "~/notes/")
  "Home directory.")

;; I use a separate repository for highly-specific elisp code that I do not want to publish online.
(defvar rangoli/private-emacs-config-dir
  (f-expand "~/.config/emacs-private/")
  "Private emacs repository.")

(provide 'rangoli-config)
;; rangoli-config.el ends here

