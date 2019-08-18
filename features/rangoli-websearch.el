;; rangoli-websearch.el --- search the web -*- lexical-binding: t; -*-

;;; Packages

(straight-use-package 'engine-mode)

;;; engine-mode

(require 'engine-mode)
(engine-mode t)

(engine/set-keymap-prefix (kbd (s-concat rangoli/leader-key " s e")))

(defengine duckduckgo
  "https://duckduckgo.com/?q=%s"
  :keybinding "d")

(defengine google-maps
  "https://www.google.com/maps?q=%s"
  :keybinding "m")

(defengine twitter
  "https://twitter.com/search?q=%s"
  :keybinding "t")

(defengine youtube
  "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
  :keybinding "y")

(provide 'rangoli-websearch)
;; rangoli-websearch.el ends here
