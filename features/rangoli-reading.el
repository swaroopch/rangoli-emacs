;; rangoli-reading.el --- reading related functionality -*- lexical-binding: t; -*-

;;; Olivetti, for reading prose, centered.
;; Useful for reading long READMEs.

(straight-use-package 'olivetti)
(setq-default olivetti-body-width 100)
(rangoli/set-leader-key "t o" 'olivetti-mode)

(provide 'rangoli-reading)
;; rangoli-reading.el ends here
