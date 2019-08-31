;; rangoli-ui.el --- ui configuration -*- lexical-binding: t; -*-

;;; Packages

(straight-use-package 'ivy)
(straight-use-package 'ivy-prescient)
(straight-use-package 'ivy-rich)
(straight-use-package 'counsel)
(straight-use-package 'swiper)
(straight-use-package 'hydra)
(straight-use-package 'avy)
(straight-use-package 'alert)
(straight-use-package 'all-the-icons)
(straight-use-package 'all-the-icons-ivy)
(straight-use-package 'saveplace)
(straight-use-package 'recentf)
(straight-use-package 'winum)
(straight-use-package 'vlf)
(straight-use-package 'restart-emacs)

;;; Require Common Packages

(require 'diminish)

;;; UI

(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t)

(delete-selection-mode +1)
(setq-default indent-tabs-mode nil)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq vc-follow-symlinks t)

(global-visual-line-mode +1)
(diminish 'visual-line-mode)

(diminish 'eldoc-mode)

;;; Backup files, Lock files, Auto-Restore, etc.

(setq auto-save-default nil
      make-backup-files nil
      create-lockfiles nil)

(global-auto-revert-mode +1)

;;; mode-line

(setq display-time-load-average-threshold 5)
(display-time)

;;; Narrowing framework - Ivy, Counsel, Swiper
;; https://oremacs.com/swiper/

(setq ivy-use-virtual-buffers t)

(ivy-mode +1)
(diminish 'ivy-mode)
;; Sorting by recency and usage
(ivy-prescient-mode)
(prescient-persist-mode)

(counsel-mode +1)
(diminish 'counsel-mode)

;; https://oremacs.com/2019/04/07/swiper-isearch/
(global-set-key (kbd "C-s") 'swiper-isearch)

(rangoli/set-leader-key "j" 'avy-goto-line "jump to line")
(rangoli/set-leader-key "s j" 'counsel-semantic-or-imenu)

;;; Dired

(setq
 ;; https://emacs.stackexchange.com/a/5604
 dired-dwim-target t

 ;; http://pragmaticemacs.com/emacs/dired-human-readable-sizes-and-sort-by-size/
 dired-listing-switches "-alh")

(rangoli/set-leader-key "f d b" 'dired-jump "dired for current buffer")
(rangoli/set-leader-key "f d d" 'dired "dired, asks for path")
(rangoli/set-leader-key "f d i" 'image-dired "image-dired, asks for path")

;;; Find File actions

(rangoli/set-leader-key "f f" 'counsel-find-file)
(rangoli/set-leader-key "f ." 'ffap)

;; https://www.emacswiki.org/emacs/InsertFileName
(ivy-add-actions t
                 '(("I" (lambda (x) (with-ivy-window (insert x))) "insert")))
(ivy-add-actions 'counsel-find-file
                 '(("F" (lambda (x) (with-ivy-window (insert (file-relative-name x))))
                    "insert relative file name")
                   ("B" (lambda (x)
                          (with-ivy-window
                            (insert (file-name-nondirectory (replace-regexp-in-string "/\\'" "" x)))))
                    "insert file name without any directory information")))

;;; Recent location

(save-place-mode +1)

(recentf-mode +1)
(setq recentf-max-saved-items 1000
      recentf-exclude '("/tmp/" "/ssh:"))
(rangoli/set-leader-key "f r" 'counsel-recentf)

;;; General key bindings

(defun rangoli/kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun rangoli/switch-to-messages-buffer ()
  (interactive)
  (switch-to-buffer (get-buffer "*Messages*")))

(defun rangoli/switch-to-scratch-buffer ()
  (interactive)
  (switch-to-buffer (get-buffer "*scratch*")))

(defun rangoli/open-init ()
  (interactive)
  (find-file user-init-file))

(defun rangoli/buffer-file-name ()
  "Copy current buffer's file name to clipboard, and display it."
  (interactive)
  (kill-new (buffer-file-name))
  (message (buffer-file-name)))

;; http://stackoverflow.com/a/10216338/4869
(defun rangoli/copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun rangoli/cut-whole-buffer-to-clipboard ()
  "Cut entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max))
  (delete-region (point-min) (point-max)))

(defun rangoli/copy-clipboard-to-whole-buffer ()
  "Copy clipboard and replace buffer"
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

(defun rangoli/save-some-buffers ()
  (interactive)
  (save-some-buffers
   t
   #'(lambda ()
       (and (not buffer-read-only)
            (buffer-file-name)))))

;; https://github.com/syl20bnr/spacemacs/blob/a69323fb3c09564217145ff4b151d157af39275b/layers/%2Bspacemacs/spacemacs-defaults/funcs.el#L329
(defun rangoli/rename-current-buffer-file ()
  (interactive)
  (let* ((rangoli/buffer-name (buffer-name))
         (rangoli/file-name (buffer-file-name)))
    (if (and rangoli/file-name
             (f-exists? rangoli/file-name))
        (let ((rangoli/new-file-name (read-file-name "New name: ")))
          (if (not (f-exists? rangoli/new-file-name))
              (progn
                (f-move rangoli/file-name rangoli/new-file-name)
                (rename-buffer rangoli/new-file-name)
                (set-visited-file-name rangoli/new-file-name)
                (set-buffer-modified-p nil)
                (message "File '%s' successfully renamed to '%s'"
                         rangoli/buffer-name
                         (f-filename rangoli/new-file-name)))
            (error (s-lex-format "File already exists: ${rangoli/new-file-name}"))))
      (error (s-lex-format "File does not exist : ${rangoli/file-name}")))))

;; https://github.com/syl20bnr/spacemacs/blob/a69323fb3c09564217145ff4b151d157af39275b/layers/%2Bspacemacs/spacemacs-defaults/funcs.el#L420
(defun rangoli/delete-current-buffer-file ()
  (interactive)
  ;; if file exists
  (let* ((rangoli/buffer (current-buffer))
         (rangoli/file-name (buffer-file-name)))
    (if (and rangoli/file-name
             (f-exists? rangoli/file-name))
        (when (y-or-n-p "Are you sure you want to delete this file? ")
          (kill-buffer rangoli/buffer)
          (f-delete rangoli/file-name))
      (error (s-lex-format "File does not exist : ${rangoli/file-name}")))))

(rangoli/set-leader-key "'" 'eshell "shell")
(rangoli/set-leader-key "\"" 'ielm "elisp REPL")

(rangoli/set-leader-key "b b" 'ivy-switch-buffer "switch")
(rangoli/set-leader-key "b c" 'rangoli/copy-whole-buffer-to-clipboard "copy buffer to clipboard")
(rangoli/set-leader-key "b d" 'rangoli/kill-current-buffer "delete")
(rangoli/set-leader-key "b h" 'bury-buffer "hide")
(rangoli/set-leader-key "b m" 'rangoli/switch-to-messages-buffer "messages")
(rangoli/set-leader-key "b s" 'rangoli/switch-to-scratch-buffer "scratch")
(rangoli/set-leader-key "b x" 'rangoli/cut-whole-buffer-to-clipboard "cut buffer to clipboard")
(rangoli/set-leader-key "b y" 'rangoli/copy-clipboard-to-whole-buffer "copy clipboard to buffer")

(rangoli/set-leader-key "f D" 'rangoli/delete-current-buffer-file)
(rangoli/set-leader-key "f R" 'rangoli/rename-current-buffer-file)
(rangoli/set-leader-key "f s" 'save-buffer)
(rangoli/set-leader-key "f S" 'rangoli/save-some-buffers)
(rangoli/set-leader-key "f e d" 'rangoli/open-init "open dot emacs file")
(rangoli/set-leader-key "f v" 'revert-buffer)
(rangoli/set-leader-key "f y" 'rangoli/buffer-file-name "current file name")

(rangoli/set-leader-key "i u" 'counsel-unicode-char "unicode")

(rangoli/set-leader-key "l" 'browse-url-at-point)
(when (eq system-type 'gnu/linux)
  (rangoli/set-leader-key "L" 'xwidget-webkit-browse-url))

(rangoli/set-leader-key "o a" 'org-agenda "agenda")
(rangoli/set-leader-key "o c" 'org-capture "capture")

(rangoli/set-leader-key "q q" 'save-buffers-kill-terminal)
(rangoli/set-leader-key "q r" 'restart-emacs)

(rangoli/set-leader-key "a d c" 'calendar "show calendar")
(rangoli/set-leader-key "a d w" 'display-time-world "show world time")

(rangoli/set-leader-key "w =" 'balance-windows-area)
(rangoli/set-leader-key "w d" 'delete-window)
(rangoli/set-leader-key "w f" 'toggle-frame-maximized)
(rangoli/set-leader-key "w m" 'delete-other-windows)
(rangoli/set-leader-key "w s" 'split-window-below)
(rangoli/set-leader-key "w v" 'split-window-right)
(rangoli/set-leader-key "w w" 'other-window)

;;; Window management
(winum-mode)

(rangoli/set-leader-key "1" 'winum-select-window-1)
(rangoli/set-leader-key "2" 'winum-select-window-2)
(rangoli/set-leader-key "3" 'winum-select-window-3)
(rangoli/set-leader-key "4" 'winum-select-window-4)
(rangoli/set-leader-key "5" 'winum-select-window-5)
(rangoli/set-leader-key "6" 'winum-select-window-6)
(rangoli/set-leader-key "7" 'winum-select-window-7)
(rangoli/set-leader-key "8" 'winum-select-window-8)
(rangoli/set-leader-key "9" 'winum-select-window-9)

;;; Notifications

;; https://github.com/jwiegley/alert#builtin-alert-styles
(setq alert-default-style
      (pcase system-type
        ('darwin 'notifier) ;; requires `terminal-notifier` command
        ('gnu/linux 'notifications)) ;; Uses notifications library via D-Bus
      )

;;; Icons

(require 'all-the-icons)

(require 'all-the-icons-ivy)
(all-the-icons-ivy-setup)

(require 'ivy-rich)
(ivy-rich-mode 1)

;;; Symbols

(global-prettify-symbols-mode)

;;; very large files
;; https://writequit.org/articles/working-with-logs-in-emacs.html

(require 'vlf-setup)

;;; very long lines
;; https://www.reddit.com/r/emacs/comments/ccoksw/solong_mitigating_slowness_due_to_extremely_long/

(global-so-long-mode)

;;; Frame size

(run-with-idle-timer 0.1 nil 'toggle-frame-maximized)

(provide 'rangoli-ui)
;; rangoli-ui.el ends here
