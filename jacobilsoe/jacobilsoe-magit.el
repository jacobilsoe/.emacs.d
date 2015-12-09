(require-package 'magit)

(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-push-always-verify nil)

(provide 'jacobilsoe-magit)
