(require-package 'dired+)
(require-package 'dired-narrow)

(setq diredp-hide-details-initially-flag nil)

(require 'dired+)

(defun open-in-external-app ()
  (interactive)
  (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" (dired-get-filename) t t))
  )

(define-key dired-mode-map (kbd "M-RET") 'open-in-external-app)
(define-key dired-mode-map (kbd "C-<up>") 'diredp-up-directory)
(define-key dired-mode-map (kbd "C-n") 'dired-narrow)

(add-hook 'dired-mode-hook 'hl-line-mode)

(diredp-toggle-find-file-reuse-dir 1)

(setq ls-lisp-dirs-first t)
(setq ls-lisp-verbosity nil)
(setq ls-lisp-ignore-case t)

(defun group-number (num &optional size char)
  "Format NUM as string grouped to SIZE with CHAR."
  ;; Based on code for `math-group-float' in calc-ext.el
  (let* ((size (or size 3))
	 (char (or char ","))
	 (str (if (stringp num)
		  num
		(number-to-string num)))
	 ;; omitting any trailing non-digit chars
	 ;; NOTE: Calc supports BASE up to 36 (26 letters and 10 digits ;)
	 (pt (or (string-match "[^0-9a-zA-Z]" str) (length str))))
    (while (> pt size)
      (setq str (concat (substring str 0 (- pt size))
			char
			(substring str (- pt size)))
	    pt (- pt size)))
    str))

(defun ls-lisp-format-file-size (file-size human-readable)
  (format "%15s" (group-number file-size 3 ".")))

(defun my-ls-lisp-format (res)
  (let ((dirstring (if (string= "d" (substring res 0 1)) "d" " ")))
    (concat dirstring (substring res 10))))
(advice-add 'ls-lisp-format :filter-return #'my-ls-lisp-format)

(defun my-insert-directory(FILE SWITCHES &optional WILDCARD FULL-DIRECTORY-P)
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (when (re-search-forward "^ *total used in directory \\([0-9]+\\) available \\([0-9]+\\)")
	(replace-match (format "%s k" (group-number (match-string 1) 3 ".")) nil nil nil 1)
	(replace-match (format "%s k" (group-number (match-string 2) 3 ".")) nil nil nil 2)
	))))
(advice-add 'insert-directory :after #'my-insert-directory)

(defun my-dired-jump-to-first-entry ()
  (interactive)
  (beginning-of-buffer)
  (dired-goto-next-nontrivial-file))

(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'my-dired-jump-to-first-entry)

(defun my-dired-jump-to-last-entry ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'my-dired-jump-to-last-entry)

(setq ls-lisp-format-time-list  '("%Y-%m-%d %H:%M" "%Y-%m-%d %H:%M"))
(setq ls-lisp-use-localized-time-format t)

(provide 'jacobilsoe-dired)