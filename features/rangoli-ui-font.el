;; rangoli-ui-font.el --- font configuration -*- lexical-binding: t; -*-

;; Need a font that supports emoji
;; Choices are:
;;
;; "Noto Mono"
;; https://www.google.com/get/noto/
;; Linux : `sudo apt install fonts-noto'
;; macOS : `brew tap homebrew/cask-fonts; brew cask install font-noto-mono'
;;
;; "IBM Plex Mono"
;; https://www.ibm.com/plex/
;; Linux : `sudo apt install fonts-ibm-plex'
;; macOS : `brew tap homebrew/cask-fonts; brew cask install font-ibm-plex'
(defvar rangoli/default-font-name "Noto Mono")
(defvar rangoli/default-font-size (pcase system-type
                                    ('darwin 16)
                                    ('gnu/linux 18)
                                    (_ 16)))
(defvar rangoli/default-font-weight "normal")

(defvar rangoli/current-font-name rangoli/default-font-name)
(defvar rangoli/current-font-size rangoli/default-font-size)
(defvar rangoli/current-font-weight rangoli/default-font-weight)

(defun rangoli/font-spec (font-name font-size font-weight)
  (s-lex-format "${font-name}:pixelsize=${font-size}:weight=${font-weight}"))

(defun rangoli/set-font (font-name font-size font-weight)
  (set-frame-font (rangoli/font-spec font-name font-size font-weight) nil t))

(defun rangoli/bigger-font-size ()
  (interactive)
  (setq rangoli/current-font-size (1+ rangoli/current-font-size))
  (rangoli/set-font rangoli/current-font-name rangoli/current-font-size rangoli/current-font-weight))

(defun rangoli/smaller-font-size ()
  (interactive)
  (setq rangoli/current-font-size (1- rangoli/current-font-size))
  (rangoli/set-font rangoli/current-font-name rangoli/current-font-size rangoli/current-font-weight))

(defun rangoli/reset-font-size ()
  (interactive)
  (setq rangoli/current-font-size rangoli/default-font-size)
  (rangoli/set-font rangoli/current-font-name rangoli/current-font-size rangoli/current-font-weight))

(defhydra rangoli/hydra-change-font-size ()
  "change font size"
  ("+" rangoli/bigger-font-size "bigger")
  ("-" rangoli/smaller-font-size "smaller")
  ("0" rangoli/reset-font-size "reset")
  ("q" nil "quit"))
(rangoli/set-leader-key "t z" 'rangoli/hydra-change-font-size/body "zoom")

(rangoli/set-font rangoli/current-font-name rangoli/current-font-size rangoli/current-font-weight)

(provide 'rangoli-ui-font)
;; rangoli-ui-font.el ends here
