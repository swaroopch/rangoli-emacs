;; rangoli-config.el --- core config -*- lexical-binding: t; -*-

;; I use https://tresorit.com/ to sync this folder, end-to-end encrypted, to the cloud.
(defvar rangoli/home-dir
  (f-expand "~/Tresors/home/")
  "/Private/ home directory where files are synced, with end-to-end encryption, to the cloud.")

;; I use https://www.dropbox.com/ to sync this folder to the cloud.
(defvar rangoli/shared-home-dir
  (f-expand "~/Dropbox/org/")
  "/Shared/ home directory where files are synced to the cloud. Usually used with other software like BeOrg iPhone app.")

;; I use https://tresorit.com/ to sync this folder, end-to-end encrypted, to the cloud.
(defvar rangoli/inbox-dir
  (f-expand "~/Tresors/inbox/")
  "/Private/ inbox directory where files are synced, with end-to-end encryption, to the cloud.")

(provide 'rangoli-config)
;; rangoli-config.el ends here

