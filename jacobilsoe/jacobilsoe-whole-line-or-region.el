(require-package 'whole-line-or-region)

(require 'whole-line-or-region)
(add-to-list 'whole-line-or-region-extensions-alist '(comment-dwim whole-line-or-region-comment-dwim nil))
(whole-line-or-region-mode t)

(provide 'jacobilsoe-whole-line-or-region)
