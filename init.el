;;; general

(server-start)

;;; visuals

(set-face-attribute 'default nil :height 105 :family "InputMono")
(load-theme 'wombat t)
(tool-bar-mode 0)
(setq inhibit-splash-screen t)

;; Pulse current line
(defun my-pulse-line (&rest _)
      (pulse-momentary-highlight-one-line (point)))
(dolist (command '(recenter-top-bottom other-window ace-window))
  (advice-add command :after #'my-pulse-line))

;;; fullscreen

(toggle-frame-maximized)
(setq w32-grab-focus-on-raise nil)

;;; editing

(setq show-paren-delay 0)
(show-paren-mode)
(blink-cursor-mode 0)
(delete-selection-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "M-SPC") 'cycle-spacing)
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'yes-or-no-p)
(defalias 'list-buffers 'ibuffer)

;;; files

(setq make-backup-files nil)
(setq large-file-warning-threshold nil)
(setq create-lockfiles nil)
(prefer-coding-system 'utf-8)

;;; package

(require 'package)

(if (file-directory-p "~/.emacs.d/mirror-elpa")
    (setq package-archives '(("gnu"   . "~/.emacs.d/mirror-elpa/gnu/")
			     ("melpa" . "~/.emacs.d/mirror-elpa/melpa/")))
  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			   ("melpa" . "http://melpa.org/packages/"))))

(package-initialize)
(package-refresh-contents)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;;; mode-line

(setq display-time-string-forms '((propertize (format-time-string "%d/%m/%Y %H:%M" now))))
(display-time-mode)
(column-number-mode)

(use-package minions
  :hook (doom-modeline-mode . minions-mode))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-minor-modes t))

;;; proced

(defun jic-proced-settings ()
  (proced-toggle-auto-update t)
  (setq proced-auto-update-interval 1)
  (setq proced-format 'medium)
  )
(add-hook 'proced-mode-hook 'jic-proced-settings)

;;; nxml-mode

(add-to-list 'auto-mode-alist '("\\.\\(xslt\\|targets\\)\\'" . nxml-mode))

;;; magit

(use-package magit
  :bind
  ("C-x g" . magit-status)
  :config
  (add-hook 'git-commit-setup-hook 'turn-off-auto-fill t)
  (setq git-commit-summary-max-length 999)
  (setq magit-push-always-verify nil)
  (use-package fullframe)
  (fullframe magit-status magit-mode-quit-window))

;;; ediff

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(defun jacobilsoe-ediff-hook ()
  (ediff-setup-keymap)
  (define-key ediff-mode-map (kbd "M-<down>") 'ediff-next-difference)
  (define-key ediff-mode-map (kbd "M-<up>") 'ediff-previous-difference))
(add-hook 'ediff-mode-hook 'jacobilsoe-ediff-hook)

;;; logview

(use-package logview
  :config
  (setq logview-additional-submodes '(("SLF4J-1" (format . "TIMESTAMP LEVEL [NAME] (THREAD)") (levels . "SLF4J"))("LOG4NET-1" (format . "TIMESTAMP [THREAD] LEVEL NAME (IGNORED)") (levels . "SLF4J")))))

;;; powershell

(use-package powershell)

;;; planuml-mode

(use-package plantuml-mode
  :mode "\\.puml\\'"
  :bind (:map plantuml-mode-map ("C-c C-c" . (lambda () (interactive) (plantuml-preview 4)))))

;;; avy

(use-package avy
  :bind
  ("C-'" . avy-goto-char-timer)
  :config
  (setq avy-timeout-seconds 0.3))

;;; ace-window

(use-package ace-window
  :bind
  ("M-o" . ace-window))

;;; scala-mode

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

;;; json-mode

(use-package json-mode
  :config
  (setq js-indent-level 2)
  (setq json-reformat:indent-width 2))

;;; csharp-mode

(use-package csharp-mode)

;;; dockerfile-mode

(use-package dockerfile-mode)

;;; yaml-mode

(use-package yaml-mode)

;;; rust-mode

(use-package rust-mode
  :config
  (add-hook 'rust-mode-hook (lambda () (setq indent-tabs-mode nil))))

;;; haskell-mode

(use-package haskell-mode
  :bind
  (:map haskell-mode-map
	("C-c C-c" . haskell-compile)))

;;; groovy-mode

(use-package groovy-mode)

;;; whole-line-or-region

(use-package whole-line-or-region
  :init
  (require 'whole-line-or-region)
  (whole-line-or-region-global-mode t))

;;; swiper

(defun jacobilsoe-swiper ()
  (interactive)
  (if (> (buffer-size) 1000000)
      (isearch-forward)
    (swiper)))

(use-package swiper
  :demand
  :bind
  ("C-s" . jacobilsoe-swiper)
  :config
  (ivy-mode))

;;; auto-package-update

(use-package auto-package-update
  :demand
  :config
  (setq auto-package-update-delete-old-versions t)
  (auto-package-update-maybe)
  (setq auto-package-update-interval 1)
  (auto-package-update-at-time "05:00"))

;;; openwith

(use-package openwith
  :config
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("mpg" "mpeg" "mp4" "avi" "wmv" "mov" "flv" "mkv" "m4v"))
               "mpv"
               '("--fullscreen" file))
         (list (openwith-make-extension-regexp
                '("xbm" "pbm" "pgm" "ppm" "pnm" "png" "gif" "bmp" "tif" "jpg" "jpeg" "cr2"))
               "feh"
               '("--fullscreen" "--start-at" file))))
  (openwith-mode 1))

;;; all-the-icons

(use-package all-the-icons)
(use-package all-the-icons-dired
  :config
  (setq all-the-icons-dired-monochrome nil)
  :hook
  (dired-mode . all-the-icons-dired-mode))
(use-package all-the-icons-ibuffer
  :ensure t
  :init (all-the-icons-ibuffer-mode 1))

(setq inhibit-compacting-font-caches t)

;;; org

(use-package org
  :bind
  ("C-c a" . org-agenda)
  :config
  (setq org-agenda-files '("~/Dropbox/Documents"))
  (setq org-tags-column 0)
  (setq org-src-window-setup 'current-window)
  (setq org-confirm-shell-link-function nil)
  (setq org-habit-graph-column 50)
  (setq org-habit-show-all-today t)
  (setq org-log-into-drawer t)
  (setq org-hide-leading-stars t)
  (setq org-startup-folded t)
  (add-to-list 'org-modules 'org-habit))

;;; hydra

(use-package hydra)

;;; org-superstar

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode))

;;; dired

;; Ensure jumping to beginning and end of buffer stays within file list
(defun my-dired-jump-to-first-entry ()
  (interactive)
  (beginning-of-buffer)
  (dired-goto-next-nontrivial-file))
(defun my-dired-jump-to-last-entry ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

;; Add possibility to force open a file in an external application
(defun dired-open-file-in-external-app ()
  (interactive)
  (cond ((eq system-type 'gnu/linux)
	 (let* ((file (dired-get-filename nil t))) (call-process "xdg-open" nil 0 nil file)))
	((eq system-type 'windows-nt)
	 (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" (dired-get-filename) t t)))))

(defhydra hydra-dired (:color pink :hint nil)
"
^Mark^                  ^Operate^                  ^View^
^^^^^^-------------------------------------------------------------------
_m_   : mark            _C_   : copy               _g_   : refresh
_% m_ : mark regexp     _% C_ : copy regexp        _(_   : toggle details
_% g_ : mark containing _R_   : rename/move        _C-n_ : narrow
_u_   : unmark          _% R_ : rename/move regexp _s_   : toggle sorting
_U_   : unmark all      _D_   : delete             _v_   : view file
_t_   : toogle marks    _Z_   : compress
^ ^                     _c_   : compress to
"
("m" dired-mark)
("% m" dired-mark-files-regexp)
("% g" dired-mark-files-containing-regexp)
("u" dired-unmark)
("U" dired-unmark-all-marks)
("t" dired-toggle-marks)
("C" dired-do-copy)
("% C" dired-do-copy-regexp)
("R" dired-do-rename)
("% R" dired-do-rename-regexp)
("D" dired-do-delete)
("Z" dired-do-compress)
("c" dired-do-compress-to)
("g" revert-buffer)
("(" dired-hide-details-mode)
("C-n" dired-narrow)
("s" dired-sort-toggle-or-edit)
("v" dired-view-file)
("q" quit-window "quit" :color blue)
("." nil :color blue))

(use-package dired
  :ensure nil
  :hook
  (dired-mode . dired-hide-details-mode)
  (dired-mode . hl-line-mode)
  (dired-mode . auto-revert-mode)
  :config
  (setq dired-listing-switches "-agho --group-directories-first --time-style=long-iso")
  (setq ls-lisp-use-insert-directory-program t)
  (setq auto-revert-verbose nil)
  (setq auto-revert-interval 1)
  (setq dired-dwim-target t)
  :bind (:map dired-mode-map
	      ([remap beginning-of-buffer] . 'my-dired-jump-to-first-entry)
	      ([remap end-of-buffer] . 'my-dired-jump-to-last-entry)
	      ("M-RET" . 'dired-open-file-in-external-app)
	      ("." . 'hydra-dired/body)))

;; Use dired-narrow
(use-package dired-narrow
  :after dired
  :bind (:map dired-mode-map ("C-n" . dired-narrow)))
