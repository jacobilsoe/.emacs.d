(require-package 'dired+)

(setq diredp-hide-details-initially-flag nil)

(require 'dired+)

(defun open-in-external-app ()
  (interactive)
  (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" (dired-get-filename) t t))
  )

(define-key dired-mode-map "\M-\r" 'open-in-external-app)
(define-key dired-mode-map [C-up] 'diredp-up-directory)

(diredp-toggle-find-file-reuse-dir 1)

(provide 'jacobilsoe-dired)
