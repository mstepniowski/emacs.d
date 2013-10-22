;; Do not show startup message, splash screen and tooltips
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(tooltip-mode -1)

;; No stupid prompts
(defalias 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; Don't ask about running processes on exit
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

;; Do not pause on redisplay
(setq redisplay-dont-pause t)

;; Do not make any backup files
(setq make-backup-files nil)

;; Mute the annoing ring bell
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; Indent with spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Remove trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Make everything look nice
(global-font-lock-mode t)

;; Show column numbers in powerline
(column-number-mode 1)

;; Show matching parens
(show-paren-mode 1)
(setq show-paren-style 'mixed)
(set-face-foreground 'show-paren-mismatch-face "red")

;; Save file history
(setq savehist-file "~/.emacs.d/.savehist")
(savehist-mode 1)

;; Mouse
(setq mouse-wheel-scroll-amount '(0.01))

;; Show keystrokes in minibuffer early
(setq echo-keystrokes 0.1)

;; Set default browser
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "google-chrome")

;; Truncate lines
(set-default 'truncate-lines t)

;; Mark always active for selecting
(setq transient-mark-mode t)

;; Remove selected region if typing
(pending-delete-mode 1)

;; Allow some commands
(dolist (command '(downcase-region upcase-region))
  (put command 'disabled nil))

;; UTF-8 EVERYWHERE!!!
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Set font size
(set-face-attribute 'default nil :height 140)

;; Highlight symbol at point
(add-hook 'find-file-hook 'idle-highlight-mode)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Prevent dabbrev from replacing case
(setq dabbrev-case-replace nil)

;; I have tons of memory, stop the GC-madness.
(setq gc-cons-threshold 50000000)
(setq gc-cons-percentage 0.5)

(setq-default display-buffer-reuse-frames t)
(set-cursor-color "coral3")
(setq enable-recursive-minibuffers t)


;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
;; (custom-set-variables
;;   '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
;;   '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

;; ;; create the autosave dir if necessary, since emacs won't.
;; (make-directory "~/.emacs.d/autosaves/" t)
