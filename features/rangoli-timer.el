;; rangoli-timer.el --- general timer functionality -*- lexical-binding: t; -*-

(defun rangoli/timer-alert (timer-message)
  (alert timer-message
         :severity 'high
         :title "Timer"
         :category 'timer))

(defun rangoli/set-timer ()
  (interactive)
  (let ((timer-duration (read-string "When? (default: 5 min) " nil nil "5 min"))
        (timer-message (read-string "What? (default: 'timer!') " nil nil "You had set a timer!")))
    (run-at-time
     timer-duration
     nil
     #'rangoli/timer-alert
     timer-message)))

(rangoli/set-leader-key "a y" #'rangoli/set-timer "timer")

(provide 'rangoli-timer)
;; rangoli-timer.el ends here
