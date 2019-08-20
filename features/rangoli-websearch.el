;; rangoli-websearch.el --- search the web -*- lexical-binding: t; -*-

;;; Packages

(straight-use-package 'engine-mode)

;;; engine-mode

(require 'engine-mode)
(engine-mode t)

(engine/set-keymap-prefix (kbd (s-concat rangoli/leader-key " s e")))
(rangoli/declare-prefix "s e" "engine")

(defengine duckduckgo
  "https://duckduckgo.com/?q=%s"
  :keybinding "d")
(rangoli/declare-prefix "s e d" "duckduckgo")

(defengine google-maps
  "https://www.google.com/maps?q=%s"
  :keybinding "m")
(rangoli/declare-prefix "s e m" "maps (google)")

(defengine twitter
  "https://twitter.com/search?q=%s"
  :keybinding "t")
(rangoli/declare-prefix "s e t" "twitter")

(defengine youtube
  "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
  :keybinding "y")
(rangoli/declare-prefix "s e y" "youtube")

(defengine wordnik
  "https://www.wordnik.com/words/%s"
  :keybinding "w")
(rangoli/declare-prefix "s e w" "wordnik")

(provide 'rangoli-websearch)
;; rangoli-websearch.el ends here
