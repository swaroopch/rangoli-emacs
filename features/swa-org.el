;; swa-org.el --- personal orgmode configuration -*- lexical-binding: t; -*-

;;; Locations of orgmode files

(setq org-directory rangoli/notes-dir
      org-default-notes-file (f-join rangoli/notes-dir "inbox.org"))

;;; Overall Behavior customization

(setq
 ;; Agenda display
 ;; Align category `%c' with larger width
 org-agenda-prefix-format  '((agenda . " %i %-20:c%?-12t% s")
                             (todo . " %i %-20:c")
                             (tags . " %i %-20:c")
                             (search . " %i %-20:c"))

 org-habit-graph-column 100)

;;; Todo states

;; ! indicates insert timestamp
;; @ indicates insert note
;; / indicates entering/exiting the state
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "DELEGATED(g!)" "WAITING(w!)" "SOMEDAY/MAYBE(s)"
		  "|"
		  "DONE(d!)" "CANCELLED(c!)")))

;;; Agenda

(defun swa/org-files-in-directory (dir)
  (when (f-exists? dir)
    (f-files dir (lambda (path)
                   (and (s-matches? "\\.org$" path)
                        ;; WebDAV folder contains files with pattern `._foo.org'
                        (not (s-matches? "^\\." (f-base path))))))))

(defun rangoli/org-files ()
  (swa/org-files-in-directory rangoli/notes-dir))

(defun swa/org-files-work ()
  (list (concat rangoli/notes-dir "work.org")))

(setq org-agenda-files (rangoli/org-files))

(defun rangoli/reload-org-agenda-files ()
  (interactive)
  (setq org-agenda-files (rangoli/org-files)))

;; https://orgmode.org/manual/Block-agenda.html
;; https://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html
(setq org-agenda-custom-commands
      '(("e" "Everything"
	 ((agenda "")
          (todo "NEXT"))
         ((org-agenda-files (rangoli/org-files))))
        ("w" "Work"
	 ((agenda "")
          (todo "NEXT"))
         ((org-agenda-files (swa/org-files-work))))))

;;; Capture templates

;; https://orgmode.org/manual/Template-elements.html

(setq org-capture-templates
      (list
       (list "i"
             "Inbox"
             'entry
             (list 'file+headline org-default-notes-file
                   "Inbox")
             "* %?\n%U\n%i\n")

       (list "l"
             "Later reading"
             'entry
             (list 'file+headline org-default-notes-file
                   "To Read")
             "* %?\n")

       (list "w"
             "Work"
             'entry
             (list 'file+headline (concat rangoli/notes-dir "work-inbox.org")
                   "Work Inbox")
             "* %?\n%U\n%i\n")

       (list "m"
             "Milestone"
             'entry
             (list 'file+olp+datetree (concat rangoli/notes-dir "milestone.org"))
             "* %?\n%i\n")

       (list "d"
             "Diary"
             'entry
             (list 'file+olp+datetree (concat rangoli/notes-dir "diary.org"))
             "* %?\n%i\n")))

(defun swa/jump-work ()
  "Jump to work file."
  (interactive)
  (find-file (f-join rangoli/notes-dir "work.org")))

(rangoli/set-leader-key "o w" 'swa/jump-work "work file")

(defun swa/jump-work-inbox ()
  "Jump to work-inbox file."
  (interactive)
  (find-file (f-join rangoli/notes-dir "work-inbox.org")))

(rangoli/set-leader-key "o W" 'swa/jump-work-inbox "work inbox file")

(defun swa/jump-tasks ()
  "Jump to tasks file."
  (interactive)
  (find-file (f-join rangoli/notes-dir "tasks.org")))

(rangoli/set-leader-key "o t" 'swa/jump-tasks "tasks file")

(provide 'swa-org)
;; swa-org.el ends here
