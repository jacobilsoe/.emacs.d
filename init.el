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

(require 'jacobilsoe-packages)
(require 'jacobilsoe-magit)
(require 'jacobilsoe-dos)
(require 'jacobilsoe-powershell)
(require 'jacobilsoe-nxml)
(require 'jacobilsoe-scala-mode2)
(require 'jacobilsoe-groovy-mode)
(require 'jacobilsoe-whole-line-or-region)
(require 'jacobilsoe-json)
(require 'jacobilsoe-dired)
(require 'jacobilsoe-swiper)
