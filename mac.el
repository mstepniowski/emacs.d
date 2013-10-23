(require 'use-package)

;; Menu bar is not annoying in OSX
(menu-bar-mode 1)

;; Use Option as Meta key
(setq mac-function-modifier 'meta)

;; Copy-Cut-Paste from clipboard with Super-C Super-X Super-V
(global-set-key (kbd "s-x") 'clipboard-kill-region) ;;cut
(global-set-key (kbd "s-c") 'clipboard-kill-ring-save) ;;copy
(global-set-key (kbd "s-v") 'clipboard-yank) ;;paste

;; Make the browser the OS X default
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;; In dired, move deletions to trash
(setq delete-by-moving-to-trash t)

;; Set font
(set-frame-font "-apple-Mensch-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1")
(setq default-frame-alist '((font . "-apple-Mensch-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1") (vertical-scroll-bars . nil)))

(defun finder ()
  "Opens file directory in Finder."
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
        (shell-command
         (format "%s %s" (executable-find "open") (file-name-directory file)))
      (error "Buffer is not attached to any file."))))

(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(global-set-key (kbd "<s-S-return>") 'toggle-fullscreen)

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))
