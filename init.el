(server-start)

(set-face-attribute 'default nil :height 120 :family "Inconsolata")

(add-to-list 'load-path "~/.emacs.d/jacobilsoe/")

(toggle-frame-fullscreen)
(setq inhibit-splash-screen t)
(setq w32-grab-focus-on-raise nil)

(setq make-backup-files nil)
(setq large-file-warning-threshold nil)

(tool-bar-mode 0)
(setq show-paren-delay 0)
(show-paren-mode)
(blink-cursor-mode 0)
(delete-selection-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-set-key (kbd "C-x k") 'kill-this-buffer)

(setq org-confirm-shell-link-function nil)

(load-theme 'wombat t)

(setq display-time-string-forms '((propertize (format-time-string "%d/%m/%Y %H:%M" now) 'face 'font-lock-string-face)))
(display-time-mode)
(column-number-mode)

(prefer-coding-system 'utf-8)

(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "M-SPC") 'cycle-spacing)

(defun jic-proced-settings ()
  (proced-toggle-auto-update t)
  (setq proced-auto-update-interval 1)
  (setq proced-format 'medium)
  )
(add-hook 'proced-mode-hook 'jic-proced-settings)

(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-agenda-files '("~/Dropbox/Documents"))
(setq org-tags-column 0)

(add-to-list 'auto-mode-alist '("\\.\\(xslt\\|targets\\)\\'" . nxml-mode))

(require 'jacobilsoe-packages)
(require 'jacobilsoe-dired)
(require 'jacobilsoe-swiper)

(use-package magit
  :bind
  (("C-x g" . magit-status))
  :config
  (setq magit-push-always-verify nil)
  (use-package fullframe)
  (fullframe magit-status magit-mode-quit-window))

(use-package dos
  :mode ("\\.\\(bat\\|cmd\\)\\'" . dos-mode))

(use-package powershell)

(use-package avy
  :bind
  (("C-'" . avy-goto-char-timer))
  :config
  (setq avy-timeout-seconds 0.3))

(use-package scala-mode2)

(use-package json-mode)

(use-package groovy-mode
  :init (require 'cl))

(use-package whole-line-or-region
  :init
  (require 'whole-line-or-region)
  (add-to-list 'whole-line-or-region-extensions-alist '(comment-dwim whole-line-or-region-comment-dwim nil))
  (whole-line-or-region-mode t))
