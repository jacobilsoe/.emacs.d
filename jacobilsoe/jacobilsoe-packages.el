(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(defun require-package (package)
  (unless (package-installed-p package)
    (package-install package)))

(package-refresh-contents)

(provide 'jacobilsoe-packages)
