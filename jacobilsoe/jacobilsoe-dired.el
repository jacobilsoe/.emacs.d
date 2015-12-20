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

(diredp-toggle-find-file-reuse-dir 1)

(provide 'jacobilsoe-dired)
