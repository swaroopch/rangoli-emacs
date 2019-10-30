;; rangoli-annotate.el --- annotate functionality -*- lexical-binding: t; -*-

;;; Annotate

(straight-use-package 'annotate)
(require 'annotate)
(setq annotate-file (f-join rangoli/notes-dir "annotations"))
(rangoli/set-leader-key "a a" 'annotate-mode "annotate")

(provide 'rangoli-annotate)
;; rangoli-annotate.el ends here


