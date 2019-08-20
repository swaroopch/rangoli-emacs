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

(defun swa/remove-indentation ()
  (interactive)
  (query-replace-regexp "^\s+" "" nil (region-beginning) (region-end)))
(rangoli/set-leader-key "b I" 'swa/remove-indentation "remove indentation")

(provide 'swa-personal)
;; swa-personal.el ends here
