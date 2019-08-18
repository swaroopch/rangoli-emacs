;; rangoli-pdf.el --- reading pdf inside emacs -*- lexical-binding: t; -*-

;;; Packages

(when (eq system-type 'gnu/linux)
  (straight-use-package 'pdf-tools))

;; Linux - pdf
(when (eq system-type 'gnu/linux)
  (add-hook 'after-init-hook 'pdf-loader-install))

(provide 'rangoli-pdf)
;; rangoli-pdf.el ends here
