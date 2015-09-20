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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (wombat))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

