;; rangoli-emoji.el --- display emoji -*- lexical-binding: t; -*-

;;; Packages

(straight-use-package 'emojify)

;;; Emoji

(require 'emojify)

(add-hook 'after-init-hook #'global-emojify-mode)

(defun swa/insert-unicode-emoji ()
  (interactive)
  (insert (emojify-completing-read
           "Insert Emoji: "
           (lambda (_ data)
             (equal (gethash "style" data) "unicode")))))

(rangoli/set-leader-key "i e" 'swa/insert-unicode-emoji "emoji")

(provide 'rangoli-emoji)
;; rangoli-emoji.el ends here
