;; rangoli-docker.el --- docker related functionality -*- lexical-binding: t; -*-

;;; Dockerfile mode
(straight-use-package 'dockerfile-mode)

;;; Docker integration
(straight-use-package 'docker)
(rangoli/set-leader-key "a d" 'docker)

(provide 'rangoli-docker)
;; rangoli-docker.el ends here


