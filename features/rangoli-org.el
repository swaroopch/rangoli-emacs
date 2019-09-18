;; rangoli-org.el --- personal orgmode configuration -*- lexical-binding: t; -*-

;;; Packages

(straight-use-package 'org-plus-contrib)
;; https://github.com/raxod502/straight.el/issues/352#issuecomment-460069774
(straight-use-package '(org :local-repo nil))
(straight-use-package 'org-pomodoro)
(straight-use-package 'toc-org)

;;; Require libraries

(require 'org)
(require 'org-attach)
(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
;; Template extension mechanism, i.e. =<q=, =<s=, etc.
;; Alternatively, use =C-c C-,= key binding.
(require 'org-tempo)
;; Load `org-md-export-as-markdown'
(require 'ox-md)

;;; Overall Behavior customization

(setq
 org-startup-indented t

 org-startup-folded nil

 org-image-actual-width 500

 org-tags-column 100

 ;; If I attach a file on Linux, sync my org directory to macOS, and then I attach a file on macOS,
 ;; it can result in a conflict because Linux generates UUIDs in both lowercase and uppercase,
 ;; whereas macOS is case-insensitive, and org-attach uses first 2 characters of the UUID to create
 ;; the directory where the attachments will be stored.
 org-id-uuid-program "uuidgen | tr '[:lower:]' '[:upper:]'"

 ;; Agenda window
 org-agenda-window-setup 'only-window

 org-agenda-show-future-repeats 'next

 ;; http://orgmode.org/worg/org-faq.html#agenda-wrong-time-of-day
 org-agenda-search-headline-for-time nil

 ;; Don't show DONE in agenda, but toggle with log mode
 org-agenda-skip-scheduled-if-done t
 org-agenda-skip-deadline-if-done t
 org-agenda-skip-deadline-prewarning-if-scheduled t
 org-agenda-log-mode-items '(closed clock state)

 ;; Agenda number of days
 org-agenda-span 1

 ;; Link to attachments
 org-link-abbrev-alist '(("att" . org-attach-expand-link))

 ;; Respect `ATTACH_DIR_INHERIT' property always
 org-attach-use-inheritance t

 ;; How to calculate clock?
 org-clock-mode-line-total 'today

 ;; Where to show clock?
 org-clock-clocked-in-display 'frame-title

 ;; Include currently running clock for report
 org-clock-report-include-clocking-task t
 )

;;; File opening
(setq org-file-apps
      '((directory . emacs)
        (auto-mode . emacs)
        ("csv" . emacs)
        ("html" . browse-url)))
(when (eq system-type 'gnu/linux)
  ;; https://askubuntu.com/a/883905/43532
  (add-to-list 'org-file-apps '(system . "setsid -w xdg-open %s"))
  ;; https://help.libreoffice.org/Common/XML_File_Formats
  (add-to-list 'org-file-apps '("odp" . "libreoffice %s"))
  (add-to-list 'org-file-apps '("odt" . "libreoffice %s"))
  (add-to-list 'org-file-apps '("ods" . "libreoffice %s"))
  ;; Microsoft Office formats
  (add-to-list 'org-file-apps '("xlsx" . "libreoffice %s"))
  (add-to-list 'org-file-apps '("xls" . "libreoffice %s"))
  (add-to-list 'org-file-apps '("docx" . "libreoffice %s"))
  (add-to-list 'org-file-apps '("doc" . "libreoffice %s")))
(when (eq system-type 'darwin)
  ;; https://help.libreoffice.org/Common/XML_File_Formats
  (add-to-list 'org-file-apps '("odp" . system))
  (add-to-list 'org-file-apps '("odt" . system))
  (add-to-list 'org-file-apps '("ods" . system))
  ;; Microsoft Office formats
  (add-to-list 'org-file-apps '("xlsx" . system))
  (add-to-list 'org-file-apps '("xls" . system))
  (add-to-list 'org-file-apps '("docx" . system))
  (add-to-list 'org-file-apps '("doc" . system)))

;;; Markup

(defun rangoli/org-bold () (interactive) (org-emphasize ?*))
(defun rangoli/org-code () (interactive) (org-emphasize ?~))
(defun rangoli/org-italic () (interactive) (org-emphasize ?/))
(defun rangoli/org-clear () (interactive) (org-emphasize ? ))
(defun rangoli/org-strikethrough () (interactive) (org-emphasize ?+))
(defun rangoli/org-underline () (interactive) (org-emphasize ?_))
(defun rangoli/org-verbatim () (interactive) (org-emphasize ?=))

;;; Log into a drawer
;; http://stackoverflow.com/a/8198018/4869
(setq org-log-into-drawer t
      org-clock-into-drawer "CLOCK"
      org-clock-idle-time 10)

;;; Effort estimation
;; http://orgmode.org/worg/doc.html#org-effort-durations
(setq org-duration-units
      `(("h" . 60)
        ;; 1 day is 4 hours
        ("d" . ,(* 60 4))
        ;; 1 week is 5 days
        ("w" . ,(* 60 4 5))
        ;; 1 month is 4 weeks
        ("m" . ,(* 60 4 5 4))
        ;; 1 year is 10 months
        ("y" . ,(* 60 4 5 4 10))))

;;; Clocking

;; Don't go idle before a pomodoro ends
(setq org-clock-idle-time 25)

;;; Refiling
;; Taken from https://github.com/vedang/emacs-config/blob/master/configuration/org-mode-config.el
(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 4)
                                 (nil :maxlevel . 4)))
      ;; Targets start with the file name - allows creating level 1 tasks
      org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm)

;;; OrgMode HTML export
;; https://orgmode.org/manual/Export-settings.html
;; https://orgmode.org/manual/HTML-Export.html
(require 'ox-html)
(setq org-html-postamble nil
      org-export-with-toc nil
      org-export-with-section-numbers nil
      org-export-with-todo-keywords nil
      org-export-with-author nil
      org-export-with-creator nil
      ;; https://developer.mozilla.org/en-US/docs/Web/CSS/font-family
      ;; https://developer.mozilla.org/en-US/docs/Web/CSS/font-size
      org-html-head-extra "
<style type=\"text/css\">
body {
  font-family: Verdana, Arial, Helvetica, sans-serif;
  font-size: small;
}
</style>
")

;;; Jump to org file, insert attachment, etc.

(defun rangoli/jump-to-org-file ()
  "Jump to one of my orgmode files."
  (interactive)
  (ivy-read "Jump to orgmode file: "
            (rangoli/org-files)
            :require-match t
            :action 'find-file))

(defun rangoli/insert-attachment-link (file-name)
  "Insert orgmode link to given file name."
  (interactive)
  (insert (s-format "[[att:$0]]" 'elt (list file-name))))

(defun rangoli/choose-and-insert-attachment-link ()
  "Choose one of the already-attached files and insert orgmode link."
  (interactive)
  (ivy-read "Choose attachment: "
            (org-attach-file-list (org-attach-dir))
            :require-match t
            :action 'rangoli/insert-attachment-link)
  (org-redisplay-inline-images))

(defun rangoli/not-dot-file? (file)
  "Predicate that determines a given file is /not/ a dot file."
  (not (s-starts-with? "." (f-filename file))))

(defun rangoli/list-files-in-commonly-used-folders ()
  "List files in commonly used folders."
  (-concat
   (f-files "~/Desktop/" 'rangoli/not-dot-file?)
   (f-files "~/Downloads/" 'rangoli/not-dot-file?)
   (f-files rangoli/inbox-dir 'rangoli/not-dot-file?)))

(defun rangoli/move-file-and-insert-attachment-link (file-name)
  "Move file and insert attachment link."
  (interactive)
  (org-attach-attach file-name nil 'mv)
  (rangoli/insert-attachment-link (f-filename file-name))
  (org-redisplay-inline-images))

(defun rangoli/choose-file-to-move-and-insert-attachment-link ()
  (interactive)
  (ivy-read "Choose file: "
            (rangoli/list-files-in-commonly-used-folders)
            :require-match t
            :action 'rangoli/move-file-and-insert-attachment-link))

(defun rangoli/rename-unique-move-file-and-insert-attachment-link (file-name)
  "Rename file to something unique, move and insert attachment link."
  (interactive)
  (let ((new-file-name (f-join (f-dirname file-name)
                               (s-concat (org-id-uuid) "." (s-downcase (f-ext file-name))))))
    (f-move file-name new-file-name)
    (rangoli/move-file-and-insert-attachment-link new-file-name)))

(defun rangoli/choose-file-to-move-rename-unique-and-insert-attachment-link ()
  (interactive)
  (ivy-read "Choose file: "
            (rangoli/list-files-in-commonly-used-folders)
            :require-match t
            :action 'rangoli/rename-unique-move-file-and-insert-attachment-link))

(defun rangoli/jump-inbox ()
  "Jump to inbox file."
  (interactive)
  (find-file org-default-notes-file))

(defun rangoli/org-insert-last-stored-link (arg)
  (interactive "p")
  (org-insert-last-stored-link arg)
    ;; org-insert-last-stored-link adds a newline ⟶ delete it
  (delete-char -1))

(defun rangoli/open-file-external ()
    (interactive)
    (org-open-at-point '(16)))

(defun rangoli/org-remove-schedule ()
  (interactive)
  ;; pass one universal argument
  (let ((current-prefix-arg '(4)))
    (call-interactively 'org-schedule)))

(defun rangoli/org-remove-deadline ()
  (interactive)
  ;; pass one universal argument
  (let ((current-prefix-arg '(4)))
    (call-interactively 'org-deadline)))

;; Already used in core-ui.el : "o a", "o c"
(rangoli/set-leader-key "o h" 'counsel-org-agenda-headlines "jump to headline")
(rangoli/set-leader-key "o j" 'rangoli/jump-to-org-file "jump to org file")
(rangoli/set-leader-key "o x" 'rangoli/jump-inbox "inbox file")
(rangoli/set-leader-key "o r" 'org-clock-goto "jump to running task")
(rangoli/set-leader-key "o R" 'rangoli/reload-org-agenda-files "reload org-agenda-files")

;;; Theme

;; https://github.com/hlissner/emacs-doom-themes/blob/e771d566b138918c7c553a9a9e96ce1bd38bc903/doom-themes-org.el#L71-L81
(setq org-hide-leading-stars t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t
      org-fontify-whole-heading-line t)

;;; Local Leader keys
(add-hook 'org-mode-hook
          (lambda ()
            (rangoli/set-local-leader-key "a" 'org-attach "attach")

            (rangoli/declare-prefix-for-mode "c" "clock")
            (rangoli/set-local-leader-key "c k" 'org-clock-cancel "cancel")
            (rangoli/set-local-leader-key "c i" 'org-clock-in "clock in")
            (rangoli/set-local-leader-key "c o" 'org-clock-out "clock out")
            (rangoli/set-local-leader-key "c p" 'org-pomodoro "pomodoro")
            (rangoli/set-local-leader-key "c r" 'org-clock-report "clock report")

            (rangoli/declare-prefix-for-mode "d" "date")
            (rangoli/set-local-leader-key "d s" 'org-schedule "schedule")
            (rangoli/set-local-leader-key "d S" 'rangoli/org-remove-schedule "remove schedule")
            (rangoli/set-local-leader-key "d d" 'org-deadline "deadline")
            (rangoli/set-local-leader-key "d D" 'rangoli/org-remove-deadline "remove deadline")

            (rangoli/declare-prefix-for-mode "i" "insert")
            (rangoli/set-local-leader-key "i a" 'rangoli/choose-file-to-move-and-insert-attachment-link "attach, insert link")
            (rangoli/set-local-leader-key "i A" 'rangoli/choose-file-to-move-rename-unique-and-insert-attachment-link "rename, attach, insert link")
            (rangoli/set-local-leader-key "i e" 'counsel-org-entity "entity")
            (rangoli/set-local-leader-key "i f" 'org-set-effort "effort")
            (rangoli/set-local-leader-key "i h" 'org-insert-heading "heading")
            (rangoli/set-local-leader-key "i n" 'org-add-note "note")
            (rangoli/set-local-leader-key "i p" 'org-set-property "property")
            (rangoli/set-local-leader-key "i t" 'counsel-org-tag "tag")

            (rangoli/set-local-leader-key "j" 'counsel-outline "jump")

            (rangoli/set-local-leader-key "l" 'org-store-link "save (l)ink")
            (rangoli/set-local-leader-key "L" 'rangoli/org-insert-last-stored-link "insert (L)ink")

            (rangoli/declare-prefix-for-mode "n" "navigate")
            (rangoli/set-local-leader-key "n c" 'org-insert-heading-after-current "create")
            (rangoli/set-local-leader-key "n u" 'outline-up-heading "up")
            (rangoli/set-local-leader-key "n n" 'org-forward-heading-same-level "next")
            (rangoli/set-local-leader-key "n p" 'org-backward-heading-same-level "previous")

            (rangoli/set-local-leader-key "o" 'org-open-at-point "open")
            (rangoli/set-local-leader-key "O" 'rangoli/open-file-external "open externally")

            (rangoli/set-local-leader-key "r" 'org-redisplay-inline-images "redisplay images")

            (rangoli/declare-prefix-for-mode "s" "subtree")
            (rangoli/set-local-leader-key "s a" 'org-archive-to-archive-sibling "archive to sibling heading")
            (rangoli/set-local-leader-key "s A" 'org-archive-subtree-default "archive to separate file")
            (rangoli/set-local-leader-key "s c" 'org-copy-subtree "copy")
            (rangoli/set-local-leader-key "s x" 'org-cut-subtree "cut")
            (rangoli/set-local-leader-key "s y" 'org-paste-subtree "yank")
            (rangoli/set-local-leader-key "s n" 'org-narrow-to-subtree "narrow")
            (rangoli/set-local-leader-key "s N" 'widen "widen")
            (rangoli/set-local-leader-key "s r" 'org-refile "refile")

            (rangoli/set-local-leader-key "t" 'org-todo "todo state")

            (rangoli/declare-prefix-for-mode "T" "toggle")
            (rangoli/set-local-leader-key "T c" 'org-toggle-checkbox "checkbox")
            (rangoli/set-local-leader-key "T i" 'org-toggle-inline-images "inline images")
            (rangoli/set-local-leader-key "T t" 'toc-org-mode "table of contents")

            (rangoli/declare-prefix-for-mode "x" "emphasize")
            (rangoli/set-local-leader-key "x b" 'rangoli/org-bold "bold")
            (rangoli/set-local-leader-key "x c" 'rangoli/org-code "code")
            (rangoli/set-local-leader-key "x i" 'rangoli/org-italic "italic")
            (rangoli/set-local-leader-key "x SPC" 'rangoli/org-clear "remove")
            (rangoli/set-local-leader-key "x s" 'rangoli/org-strikethrough "strikethrough")
            (rangoli/set-local-leader-key "x u" 'rangoli/org-underline "underline")
            (rangoli/set-local-leader-key "x v" 'rangoli/org-verbatim "verbatim")
            ))

;;; Agenda keys
(add-hook 'org-agenda-mode-hook
          (lambda ()
            (rangoli/declare-prefix-for-mode "c" "clock")
            (rangoli/set-local-leader-key "c k" 'org-clock-cancel "clock cancel")
            (rangoli/set-local-leader-key "c i" 'org-clock-in "clock in")
            (rangoli/set-local-leader-key "c o" 'org-clock-out "clock out")
            (rangoli/set-local-leader-key "c p" 'org-pomodoro "pomodoro")
            ))

;;; Programming source code blocks

;; https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-python.html
(org-babel-do-load-languages
 'org-babel-load-languages '((python . t)))

(provide 'rangoli-org)
;; rangoli-org.el ends here
