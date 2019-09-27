;; rangoli-config.el --- core config -*- lexical-binding: t; -*-

(defvar rangoli/home-dir
  (f-expand "~/docs/notes/")
  "/Private/ home directory.")

(defvar rangoli/shared-home-dir
  (f-expand "~/docs/notes/")
  "/Shared/ home directory. Usually used with other software like BeOrg iPhone app.")

(defvar rangoli/inbox-dir
  (f-expand "~/Downloads/")
  "/Private/ inbox directory.")

;; I use a separate repository for highly-specific elisp code.
(defvar rangoli/private-emacs-config-dir
  (f-expand "~/.config/emacs-private/")
  "/Private/ emacs repository.")

(provide 'rangoli-config)
;; rangoli-config.el ends here

