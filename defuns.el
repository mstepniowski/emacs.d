(require 'python)

(defun comment-or-uncomment-current-line-or-region ()
  "Comments or uncomments current current line or whole lines in region."
  (interactive)
  (save-excursion
    (let (min max)
      (if (region-active-p)
          (setq min (region-beginning) max (region-end))
        (setq min (point) max (point)))
      (comment-or-uncomment-region
       (progn (goto-char min) (line-beginning-position))
       (progn (goto-char max) (line-end-position))))))

(defun kill-and-join-forward (&optional arg)
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (progn (forward-char 1)
	     (just-one-space 0)
	     (backward-char 1)
	     (kill-line arg))
    (kill-line arg)))

(defun magit-just-amend ()
  (interactive)
  (save-window-excursion
    (magit-with-refresh
      (shell-command "git --no-pager commit --amend --reuse-message=HEAD"))))

(defun back-to-indentation-or-beginning-of-line ()
  "Moves point back to indentation if there is any
non blank characters to the left of the cursor.
Otherwise point moves to beginning of line."
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line)
    (back-to-indentation)))

;; Helper functions for yasnippets
(defun python-class-name ()
  "Finds beginning of innermost nested class definition.
Returns the name of the definition found at the end, or nil if
reached start of buffer."
  (save-excursion
    (let ((def-re (rx line-start (0+ space) "class" (1+ space)
		      (group (1+ (or word (syntax symbol))))))
	  found)
      (while (and (python-nav-beginning-of-defun) (not found))
	(if (looking-at def-re)
	    (setq found (match-string 1))))
      found)))

(defun python-def-name ()
  "Finds beginning of innermost nested class definition.
Returns the name of the definition found at the end, or nil if
reached start of buffer."
  (save-excursion
    (let ((def-re (rx line-start (0+ space) "def" (1+ space)
		      (group (1+ (or word (syntax symbol))))))
	  found)
      (while (and (python-nav-beginning-of-defun) (not found))
	(if (looking-at def-re)
	    (setq found (match-string 1))))
      found)))


(provide 'defuns)
;;; defuns.el ends here
