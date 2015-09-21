(server-start)

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(defvar packages '(dos powershell magit))

(defun require-package (package)
  (unless (package-installed-p package)
    (package-install package)))

(defun install-packages ()
    (package-refresh-contents)
    (mapc #'require-package packages))

(install-packages)

(add-to-list 'auto-mode-alist '("\\.\\(bat\\|cmd\\)\\'" . dos-mode))
(add-to-list 'auto-mode-alist '("\\.\\(ps1\\|psm1\\)\\'" . powershell-mode))

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
