;; rangoli-websearch.el --- search the web -*- lexical-binding: t; -*-

;;; Packages

(straight-use-package 'engine-mode)

;;; engine-mode

(require 'engine-mode)
(engine-mode t)

(engine/set-keymap-prefix (kbd (s-concat rangoli/leader-key " j s")))
(rangoli/declare-prefix "j s" "search")

(defengine duckduckgo
  "https://duckduckgo.com/?q=%s"
  :keybinding "d")
(rangoli/declare-prefix "j s d" "duckduckgo")

(defengine google-maps
  "https://www.google.com/maps?q=%s"
  :keybinding "m")
(rangoli/declare-prefix "j s m" "maps (google)")

(defengine twitter
  "https://twitter.com/search?q=%s"
  :keybinding "t")
(rangoli/declare-prefix "j s t" "twitter")

(defengine youtube
  "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
  :keybinding "y")
(rangoli/declare-prefix "j s y" "youtube")

(defengine wordnik
  "https://www.wordnik.com/words/%s"
  :keybinding "w")
(rangoli/declare-prefix "j s w" "wordnik")

(provide 'rangoli-websearch)
;; rangoli-websearch.el ends here
