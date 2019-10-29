;; rangoli-config.el --- core config -*- lexical-binding: t; -*-

;; I use https://tresorit.com/ to sync this folder.
(defvar rangoli/home-dir
  (f-expand "~/notes/")
  "Home directory.")

;; I use https://tresorit.com/ to sync this folder.
(defvar rangoli/work-dir
  (f-expand "~/work-notes/")
  "Work directory.")

(defvar rangoli/mobile-dir
  (-first #'f-exists?
          (list
           ;; https://www.fastmail.com/help/files/davnftp.html
           ;; macOS path
           "/Volumes/myfiles.fastmail.com/org/"))
  "Directory which is synced to mobile app https://beorg.app/")

;; I use a separate repository for highly-specific elisp code that I do not want to publish online.
(defvar rangoli/private-emacs-config-dir
  (f-expand "~/.config/emacs-private/")
  "Private emacs repository.")

(provide 'rangoli-config)
;; rangoli-config.el ends here

