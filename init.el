(server-start)

(add-to-list 'load-path "~/.emacs.d/jacobilsoe/")

(toggle-frame-fullscreen)
(setq inhibit-splash-screen t)
(setq w32-grab-focus-on-raise nil)

(setq make-backup-files nil)
(setq large-file-warning-threshold nil)

(tool-bar-mode -1)
(show-paren-mode t)
(blink-cursor-mode 0)

(ido-mode t)

(setq org-confirm-shell-link-function nil)

(load-theme 'wombat t)

(setq display-time-string-forms '((propertize (concat day "/" month "/" year " " 24-hours ":" minutes) 'face 'font-lock-string-face)))
(display-time-mode)

(prefer-coding-system 'utf-8)

(global-set-key (kbd "M-SPC") 'cycle-spacing)

(require 'jacobilsoe-packages)
(require 'jacobilsoe-magit)
(require 'jacobilsoe-dos)
(require 'jacobilsoe-powershell)
(require 'jacobilsoe-nxml)
(require 'jacobilsoe-scala-mode2)
(require 'jacobilsoe-groovy-mode)
(require 'jacobilsoe-json)
