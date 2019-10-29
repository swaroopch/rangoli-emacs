;; swa-bookmarks.el --- personal bookmarks -*- lexical-binding: t; -*-

;; Inspired by https://github.com/emacs-helm/helm/blob/master/helm-org.el

(defvar swa/bookmarks-files
  (list
   (f-join rangoli/work-dir "bookmarks.org")
   (f-join rangoli/home-dir "bookmarks.org")))

(defun swa/counsel-org-get-heading (&optional no-tags no-todo no-priority no-comment)
  ;; Parse link, if any, and return description inside link
  (let ((heading (org-get-heading no-tags no-todo no-priority no-comment)))
    (if (s-matches? org-link-bracket-re heading)
        (-third-item (s-match org-link-bracket-re heading))
      heading)))

(defun swa/counsel-org-open-bookmark-action (x)
  (save-excursion
    (goto-char (cdr x))
    (org-open-at-point)))

(defun swa/open-web-bookmark ()
  (interactive)
  ;; Inspired by `counsel-outline'
  (with-temp-buffer
    (org-mode)
    (-each-r swa/bookmarks-files #'insert-file-contents)
    (goto-char (point-min))
    (ivy-read "Outline: " (counsel-outline-candidates (list :outline-title #'swa/counsel-org-get-heading))
              :action #'swa/counsel-org-open-bookmark-action)))

(rangoli/set-leader-key "m" 'swa/open-web-bookmark)

(provide 'swa-bookmarks)
;; swa-bookmarks.el ends here
