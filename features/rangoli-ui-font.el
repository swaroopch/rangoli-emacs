;; rangoli-ui-font.el --- font configuration -*- lexical-binding: t; -*-

;; Need a font that supports emoji
;; https://www.google.com/get/noto/
(defvar rangoli/default-font-name "Noto Mono")
(defvar rangoli/default-font-size 15.0)

(defvar rangoli/current-font-name rangoli/default-font-name)
(defvar rangoli/current-font-size rangoli/default-font-size)

(defun rangoli/font-spec (font-name font-size)
  (s-lex-format "${font-name}-${font-size}"))

(defun rangoli/set-font (font-name font-size)
  (set-frame-font (rangoli/font-spec font-name font-size) nil t))

(defun rangoli/bigger-font-size ()
  (interactive)
  (setq rangoli/current-font-size (1+ rangoli/current-font-size))
  (rangoli/set-font rangoli/current-font-name rangoli/current-font-size))

(defun rangoli/smaller-font-size ()
  (interactive)
  (setq rangoli/current-font-size (1- rangoli/current-font-size))
  (rangoli/set-font rangoli/current-font-name rangoli/current-font-size))

(defun rangoli/reset-font-size ()
  (interactive)
  (setq rangoli/current-font-size rangoli/default-font-size)
  (rangoli/set-font rangoli/current-font-name rangoli/current-font-size))

(defhydra rangoli/hydra-change-font-size ()
  "change font size"
  ("+" rangoli/bigger-font-size "bigger")
  ("-" rangoli/smaller-font-size "smaller")
  ("0" rangoli/reset-font-size "reset")
  ("q" nil "quit"))
(rangoli/set-leader-key "w z" 'rangoli/hydra-change-font-size/body "zoom")

(rangoli/set-font rangoli/current-font-name rangoli/current-font-size)

(provide 'rangoli-ui-font)
;; rangoli-ui-font.el ends here
