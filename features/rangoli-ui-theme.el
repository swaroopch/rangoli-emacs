;; rangoli-ui-theme.el --- theme configuration -*- lexical-binding: t; -*-

;;; Packages

(straight-use-package 'gruvbox-theme)
(straight-use-package 'kaolin-themes)
(straight-use-package 'nord-theme)

;;; Terminal

(when (not (display-graphic-p))
  ;; https://stackoverflow.com/a/13812604/4869
  (send-string-to-terminal "\033]12;green\007"))

;;; Theme-specific config

(require 'kaolin-themes)
(setq kaolin-themes-org-scale-headings nil)

;; https://www.nordtheme.com/docs/ports/emacs/configuration
(require 'nord-theme)
(setq nord-region-highlight "snowstorm")

;;; Default Theme

(defvar rangoli/default-theme-light 'kaolin-valley-light)
(defvar rangoli/default-theme-dark 'nord)

(defun rangoli/mac-appearance ()
  "Is Mac currently using light or dark appearance?"
  (when (eq system-type 'darwin)
    (if (s-contains? "Dark"
                     (shell-command-to-string "defaults read -g AppleInterfaceStyle"))
        "dark"
      "light")))

(defun rangoli/day-time? ()
  "Is now between 8am and 5pm?"
  (< 7 (ts-hour (ts-now)) 17))

(defun rangoli/theme-light-or-dark? ()
  (if-let ((mac-appearance (rangoli/mac-appearance)))
      mac-appearance
    
    (if (rangoli/day-time?)
        ;; day
        "light"
      ;; evening
      "dark")))

(defvar rangoli/theme-type nil "light or dark.")

(defun rangoli/load-theme-light ()
  (interactive)
  (setq rangoli/theme-type "light")
  (counsel-load-theme-action (symbol-name rangoli/default-theme-light))
  (when (eq system-type 'darwin)
    (modify-all-frames-parameters '((ns-transparent-titlebar . t) (ns-appearance . light)))))

(defun rangoli/load-theme-dark ()
  (interactive)
  (setq rangoli/theme-type "dark")
  (counsel-load-theme-action (symbol-name rangoli/default-theme-dark))
  (when (eq system-type 'darwin)
    (modify-all-frames-parameters '((ns-transparent-titlebar . t) (ns-appearance . dark)))))

(if (s-equals? "light" (rangoli/theme-light-or-dark?))
    (rangoli/load-theme-light)
  (rangoli/load-theme-dark))

;;; Theme switcher

(defun rangoli/theme-cycle ()
  (interactive)
  (if (s-equals? "light" rangoli/theme-type)
      (rangoli/load-theme-dark)
    (rangoli/load-theme-light)))

(rangoli/set-leader-key "T n" 'rangoli/theme-cycle "next theme")
(rangoli/set-leader-key "T s" 'counsel-load-theme "switch theme")

(provide 'rangoli-ui-theme)
;; rangoli-ui-theme.el ends here
