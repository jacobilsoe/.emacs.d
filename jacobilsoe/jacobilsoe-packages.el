(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(defun require-package (package)
  (unless (package-installed-p package)
    (package-install package)))

(package-refresh-contents)

(require-package 'auto-package-update)
(setq auto-package-update-delete-old-versions t)
(auto-package-update-maybe)
(setq auto-package-update-interval 1)
(auto-package-update-at-time "05:00")

(provide 'jacobilsoe-packages)
