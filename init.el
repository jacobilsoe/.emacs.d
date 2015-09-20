(server-start)

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(defvar packages '(dos))

(defun require-package (package)
  (unless (package-installed-p package)
    (package-install package)))

(defun install-packages ()
    (package-refresh-contents)
    (mapc #'require-package packages))

(install-packages)

(add-to-list 'auto-mode-alist '("\\.\\(bat\\|cmd\\)\\'" . dos-mode))

(setq inhibit-splash-screen t)

(setq make-backup-files nil)

(setq large-file-warning-threshold nil)

(setq w32-grab-focus-on-raise nil)

(ido-mode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(blink-cursor-mode nil)
 '(custom-enabled-themes (quote (wombat)))
 '(org-confirm-shell-link-function nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

