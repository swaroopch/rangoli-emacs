;; swa-personal.el --- personal settings -*- lexical-binding: t; -*-

;;; Packages

;;; My details
(setq user-full-name "Swaroop Chitlur")
(setq user-mail-address "swaroop@swaroopch.com")

;;; World Clock

(setq zoneinfo-style-world-list
      '(("America/Los_Angeles" "San Francisco")
        ("America/New_York" "New York")
        ("Asia/Calcutta" "Bangalore")
        ("Asia/Singapore" "Singapore")))

;;; Startup

(setq initial-major-mode 'org-mode
      initial-scratch-message nil)

;;; Remove indentation on region

(defun swa/org-mode-remove-leading-indentation ()
  (interactive)
  (let* ((text (org-get-entry))
         (first-line (car (s-lines text)))
         (first-line-trimmed (s-trim-left first-line))
         (leading-indentation-length (- (length first-line) (length first-line-trimmed)))
         (leading-indentation-regexp (s-concat "^" (s-repeat leading-indentation-length "\s"))))
    (when (> leading-indentation-length 0)
      (save-excursion
        (org-mark-subtree)
        (goto-char (region-beginning))
        (while (re-search-forward leading-indentation-regexp (region-end) t)
          (replace-match ""))
        (deactivate-mark)))))

(rangoli/declare-prefix "k o" "org-mode")
(rangoli/set-leader-key "k o i" 'swa/org-mode-remove-leading-indentation "remove leading indentation")

(provide 'swa-personal)
;; swa-personal.el ends here
