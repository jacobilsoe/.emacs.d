(require-package 'magit)

(global-set-key (kbd "C-x g") 'magit-status)

(setq magit-push-always-verify nil)

(require-package 'fullframe)
(with-eval-after-load 'magit (fullframe magit-status magit-mode-quit-window))

(provide 'jacobilsoe-magit)
