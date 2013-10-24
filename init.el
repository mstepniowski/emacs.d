;;; init.el --- Marek Stepniowski
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 's)
(require 'f)
(require 'git)
(require 'dash)
(require 'use-package)

(defun load-x (file)
  (load (f-expand file user-emacs-directory)))

(let ((default-directory user-emacs-directory))
  (load-x "defuns")
  (load-x "misc")
  (when (eq system-type 'darwin)
    (load-x "mac")))


;;;; Packages

(use-package dired-x)

(use-package ido
  :init
  (progn
    (ido-mode 1)
    (use-package ido-vertical-mode
      :init (ido-vertical-mode 1))
    (use-package flx-ido
      :init (flx-ido-mode 1))
    (use-package ido-ubiquitous)
    (use-package smex
      :init (smex-initialize)
      :bind ("M-x" . smex)))
  :config
  (progn
    (setq ido-enable-prefix nil
          ido-enable-flex-matching t
          ido-case-fold t
          ido-create-new-buffer 'always
          ido-use-filename-at-point nil
          ido-max-prospects 10)
    (add-to-list 'ido-ignore-files "\\.DS_Store")

    (defun my-ido-tweaks ()
      (define-key ido-file-completion-map
        (kbd "~")
        (lambda ()
          (interactive)
          (cond
           ((looking-back "~/") (insert "Projekty/"))
           ((looking-back "/") (insert "~/"))
           (:else (call-interactively 'self-insert-command)))))
      ;; C-n/p is more intuitive in vertical layout
      (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
      (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
      (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
      (define-key ido-completion-map (kbd "<up>") 'ido-prev-match))

    (add-hook 'ido-setup-hook 'my-ido-tweaks)))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

(use-package popwin
  :config (setq display-buffer-function 'popwin:display-buffer))

(use-package misc
  :bind ("M-z" . zap-up-to-char))

(use-package magit
  :init
  (progn
    (use-package magit-blame)
    (bind-key "C-c C-a" 'magit-just-amend magit-mode-map))
  :config
  (progn
    (setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)
    (setq magit-set-upstream-on-push t)
    (setq magit-completing-read-function 'magit-ido-completing-read)
    (setq magit-stage-all-confirm nil)
    (setq magit-unstage-all-confirm nil))
  :bind ("C-x g" . magit-status))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package cua-base
  :init (cua-mode 1)
  :config
  (progn
    (setq cua-enable-cua-keys nil)
    (setq cua-toggle-set-mark nil)))

(use-package uniquify
  :config
  (progn
	(setq uniquify-buffer-name-style 'post-forward)
	(setq uniquify-strip-common-suffix t)))

(use-package saveplace
  :config (setq-default save-place t))

(use-package diff-hl
  :init (global-diff-hl-mode)
  :config (add-hook 'vc-checkin-hook 'diff-hl-update))

(use-package page-break-lines
  :init (global-page-break-lines-mode 1)
  :config
  (progn
    (defadvice backward-page (after backward-page-mbol activate)
      (move-beginning-of-line 1))
    (defadvice forward-page (after forward-page-mbol activate)
      (move-beginning-of-line 1))))

(use-package python
  :config
  (progn
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args ""
          python-shell-prompt-regexp "In \\[[0-9]+\\]: "
          python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
          python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
          python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
          python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

    (define-key comint-mode-map (kbd "<up>")
      'comint-previous-matching-input-from-input)
    (define-key comint-mode-map (kbd "<down>")
      'comint-next-matching-input-from-input)))

(use-package markdown-mode
  :mode (("\\.markdown$" . markdown-mode)
         ("\\.md$" . markdown-mode)))

(use-package flycheck
  :config
  (progn
    ;; (setq flycheck-display-errors-function nil)
    (add-hook 'after-init-hook 'global-flycheck-mode)))

(use-package flycheck-cask
  :init (add-hook 'flycheck-mode-hook 'flycheck-cask-setup))

(use-package yasnippet
  :init
  (progn
    (let ((snippets-dir (f-expand "snippets" user-emacs-directory)))
      (yas/load-directory snippets-dir))
    (setq-default yas/prompt-functions '(yas/ido-prompt))
    (yas-global-mode 1)))

(use-package smart-tab
  :init
  (progn
    (setq hippie-expand-try-functions-list '(yas-hippie-try-expand
                                             try-complete-file-name-partially
                                             try-expand-dabbrev-visible
                                             try-expand-dabbrev
                                             try-expand-dabbrev-all-buffers
                                             try-complete-lisp-symbol-partially
                                             try-complete-lisp-symbol))
    (setq smart-tab-using-hippie-expand t)
    (setq smart-tab-disabled-major-modes '(org-mode term-mode eshell-mode inferior-python-mode))
    (global-smart-tab-mode 1)))

(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode))

(use-package cc-mode
  :config
  (progn
    (add-hook 'c-mode-hook (lambda () (c-set-style "bsd")))
    (add-hook 'java-mode-hook (lambda () (c-set-style "bsd")))
    (setq tab-width 4)
    (setq c-basic-offset 4)))

(use-package css-mode
  :config (setq css-indent-offset 4))

(use-package js-mode
  :mode ("\\.json$" . js-mode)
  :init
  (progn
    (add-hook 'js-mode-hook (lambda () (setq js-indent-level 2)))))

(use-package emmet-mode
  :init
  (progn
    (add-hook 'sgml-mode-hook 'emmet-mode)
    (add-hook 'css-mode-hook 'emmet-mode)))

(use-package js2-mode
  :mode (("\\.js$" . js2-mode)
         ("Jakefile$" . js2-mode))
  :interpreter ("node" . js2-mode)
  :bind (("C-a" . back-to-indentation-or-beginning-of-line)
         ("C-M-h" . backward-kill-word)
         ("M-j" . join-line-or-lines-in-region))
  :config
  (progn
    (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))))

(use-package coffee-mode
  :init
  (bind-key "C-j" 'coffee-newline-and-indent coffee-mode-map)
  :config
  (progn
    (setq coffee-tab-width 2)
    (setq coffee-cleanup-whitespace nil)))

(use-package emacs-lisp-mode
  :init
  (progn
    (use-package eldoc
      :init (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))
    (use-package macrostep
      :bind ("C-c e" . macrostep-expand))
    (use-package ert
      :config
      (progn
        (put 'ert-deftest 'lisp-indent-function 'defun)
        (add-hook 'emacs-lisp-mode-hook
                  (lambda ()
                    (font-lock-add-keywords
                     nil
                     '(("(\\(\\<ert-deftest\\)\\>\\s *\\(\\sw+\\)?"
                        (1 font-lock-keyword-face nil t)
                        (2 font-lock-function-name-face nil t)))))))))
  :bind (("M-&" . lisp-complete-symbol)
         ("M-." . find-function-at-point))
  :interpreter (("emacs" . emacs-lisp-mode))
  :mode ("Cask" . emacs-lisp-mode))

(use-package go-mode
  :config
  (progn
    (setenv "GOROOT" "/usr/local/opt/go")
    (setenv "GOPATH" "/Users/mstepniowski/Projekty/go")
    (add-hook 'before-save-hook 'gofmt-before-save)
    (add-hook 'go-mode-hook (lambda ()
                              (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
                              (local-set-key (kbd "C-c i") 'go-goto-imports)
                              (local-set-key (kbd "M-.") 'godef-jump)))))

(use-package ack-and-a-half
  :init
  (progn
    (defalias 'ack 'ack-and-a-half)
    (defalias 'ack-same 'ack-and-a-half-same)
    (defalias 'ack-find-file 'ack-and-a-half-find-file)
    (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)))


;;;; Color theme

(setq custom-theme-directory (f-expand "themes" user-emacs-directory))
(load-theme 'tango t)
(custom-theme-set-faces 'tango
			'(default ((t (:background "#ffffff"))))
			'(fringe ((t (:background "#ffffff"))))
			'(flycheck-error-face ((t (:underline "#ee6666"))))
			'(flycheck-warning-face ((t (:underline "#af8700")))))
			;; '(mode-line ((t (:underline nil :inverse-video t :box nil))))
			;; '(mode-line-inactive ((t (:underline nil :inverse-video t :box nil)))))


;;;; Bindings

(define-key global-map (kbd "RET") 'newline-and-indent)
(bind-key "C-a" 'back-to-indentation-or-beginning-of-line)
(bind-key "C-k" 'kill-and-join-forward)
(bind-key "s-/" 'comment-or-uncomment-current-line-or-region)
(bind-key "C-x C-b" 'ibuffer)
(bind-key "M-/" 'hippie-expand)
(bind-key "C-s" 'isearch-forward-regexp)
(bind-key "C-r" 'isearch-backward-regexp)
(bind-key "C-M-s" 'isearch-forward)
(bind-key "C-M-r" 'isearch-backward)
(bind-key "C-8" (lambda ()
                  (interactive)
                  (find-file (f-expand "init.el" user-emacs-directory))))


;;;; Start emacs server automatically

(let ((dir (getenv "EOPEN_DIR"))
      (file (getenv "EOPEN_FILE")))
  (if dir
      (cd dir))
  (if file
      (find-file file)))

(unless (server-running-p) (server-start))
