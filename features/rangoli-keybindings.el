;; rangoli-keybindings.el --- key bindings functionality -*- lexical-binding: t; -*-

;;; Packages

(straight-use-package 'which-key)

;;; Leader Keys, which-key
;; https://www.masteringemacs.org/article/mastering-key-bindings-emacs
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Commands.html

;; NOTE: Choose leader keys that work in both terminal and GUI.

(defvar rangoli/leader-key "M-m")
(define-prefix-command 'rangoli/leader-key-map)
(global-set-key (kbd rangoli/leader-key) 'rangoli/leader-key-map)

(defvar rangoli/local-leader-key "M-n")

(setq which-key-echo-keystrokes 0.02
      echo-keystrokes 0.02
      which-key-idle-delay 0.4)
(which-key-mode +1)
(diminish 'which-key-mode)

(defun rangoli/declare-prefix (key name)
  "Declare prefix KEY as NAME in which-key."
  (which-key-add-key-based-replacements
    (concat rangoli/leader-key " " key) name))

(defun rangoli/declare-prefix-for-mode (mode key name)
  "In major mode MODE, declare prefix KEY as NAME in which-key."
  (which-key-add-major-mode-key-based-replacements mode
    (concat rangoli/local-leader-key " " key) name))

(defun rangoli/set-global-key (key def &optional name)
  "Add KEY mapped to DEF as global key, showing NAME in which-key."
  (global-set-key (kbd key) def)
  (when name
    (which-key-add-key-based-replacements key name)))

(defun rangoli/set-leader-key (key def &optional name)
  "Add KEY mapped to DEF under leader key, showing NAME in which-key."
  (global-set-key (kbd (concat rangoli/leader-key " " key)) def)
  (when name
    (rangoli/declare-prefix key name)))

(defun rangoli/set-local-leader-key (key def &optional name)
  "Add KEY mapped to DEF under local leader key, showing NAME in which-key."
  (local-set-key (kbd (concat rangoli/local-leader-key " " key)) def)
  (when name
    (rangoli/declare-prefix-for-mode major-mode key name)))

(rangoli/declare-prefix "a" "application")
(rangoli/declare-prefix "b" "buffer")
(rangoli/declare-prefix "c" "comment")
(rangoli/declare-prefix "d" "dired")
(rangoli/declare-prefix "f" "file")
(rangoli/declare-prefix "f e" "emacs")
(rangoli/declare-prefix "g" "git")
(rangoli/declare-prefix "i" "insert")
(rangoli/declare-prefix "o" "orgmode")
(rangoli/declare-prefix "p" "project")
(rangoli/declare-prefix "q" "quit")
(rangoli/declare-prefix "t" "date/time")
(rangoli/declare-prefix "w" "window")
(rangoli/declare-prefix "x" "syntax")
(rangoli/declare-prefix "z" "work")
(rangoli/declare-prefix "T" "theme")

(provide 'rangoli-keybindings)
;; rangoli-keybindings.el ends here
