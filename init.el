;;; general

(server-start)

;;; visuals

(set-face-attribute 'default nil :height 105 :family "InputMono")
(load-theme 'wombat t)
(tool-bar-mode 0)
(setq inhibit-splash-screen t)

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

;;; files

(setq make-backup-files nil)
(setq large-file-warning-threshold nil)
(setq create-lockfiles nil)
(prefer-coding-system 'utf-8)

;;; mode-line

(setq display-time-string-forms '((propertize (format-time-string "%d/%m/%Y %H:%M" now) 'face 'font-lock-string-face)))
(display-time-mode)
(column-number-mode)

;;; proced

(defun jic-proced-settings ()
  (proced-toggle-auto-update t)
  (setq proced-auto-update-interval 1)
  (setq proced-format 'medium)
  )
(add-hook 'proced-mode-hook 'jic-proced-settings)

;;; nxml-mode

(add-to-list 'auto-mode-alist '("\\.\\(xslt\\|targets\\)\\'" . nxml-mode))

;;; package

(require 'package)

(if (file-directory-p "~/.emacs.d/mirror-elpa")
    (setq package-archives '(("gnu"   . "~/.emacs.d/mirror-elpa/gnu/")
			     ("melpa" . "~/.emacs.d/mirror-elpa/melpa/")
			     ("org"   . "~/.emacs.d/mirror-elpa/org/")))
  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			   ("melpa" . "http://melpa.org/packages/")
			   ("org" . "http://orgmode.org/elpa/"))))

(package-initialize)
(package-refresh-contents)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setq use-package-always-ensure t)

;;; magit

(use-package magit
  :bind
  (("C-x g" . magit-status))
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
  (("C-'" . avy-goto-char-timer))
  :config
  (setq avy-timeout-seconds 0.3))

;;; ace-window

(use-package ace-window
  :bind
  (("M-p" . ace-window)))

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
  (("C-s" . jacobilsoe-swiper))
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

;;; org

(use-package org
  :bind
  (("C-c a" . org-agenda))
  :config
  (setq org-agenda-files '("~/Dropbox/Documents"))
  (setq org-tags-column 0)
  (setq org-src-window-setup 'current-window)
  (setq org-confirm-shell-link-function nil))

;;; dired

(use-package dired+
  :load-path "packages/")

(use-package dired-narrow)

(setq diredp-hide-details-initially-flag nil)

(require 'dired+)
(require 'ls-lisp)

(defun dired-open-file-in-external-app ()
  (interactive)
  (cond ((eq system-type 'gnu/linux)
	 (let* ((file (dired-get-filename nil t))) (call-process "xdg-open" nil 0 nil file)))
	((eq system-type 'windows-nt)
	 (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" (dired-get-filename) t t)))))

(define-key dired-mode-map (kbd "M-RET") 'dired-open-file-in-external-app)
(define-key dired-mode-map (kbd "C-<up>") 'diredp-up-directory)
(define-key dired-mode-map (kbd "C-n") 'dired-narrow)

(add-hook 'dired-mode-hook 'hl-line-mode)

(diredp-toggle-find-file-reuse-dir 1)
(setq ls-lisp-use-insert-directory-program nil)
(setq ls-lisp-dirs-first t)
(setq ls-lisp-verbosity nil)
(setq ls-lisp-ignore-case t)

(defun group-number (num &optional size char)
  "Format NUM as string grouped to SIZE with CHAR."
  ;; Based on code for `math-group-float' in calc-ext.el
  (let* ((size (or size 3))
	 (char (or char ","))
	 (str (if (stringp num)
		  num
		(number-to-string num)))
	 ;; omitting any trailing non-digit chars
	 ;; NOTE: Calc supports BASE up to 36 (26 letters and 10 digits ;)
	 (pt (or (string-match "[^0-9a-zA-Z]" str) (length str))))
    (while (> pt size)
      (setq str (concat (substring str 0 (- pt size))
			char
			(substring str (- pt size)))
	    pt (- pt size)))
    str))

(defun ls-lisp-format-file-size (file-size human-readable)
  (format "%15s" (group-number file-size 3 ".")))

(defun my-ls-lisp-format (res)
  (let ((dirstring (if (string= "d" (substring res 0 1)) "d" " ")))
    (concat dirstring (substring res 10))))
(advice-add 'ls-lisp-format :filter-return #'my-ls-lisp-format)

(defun my-insert-directory(FILE SWITCHES &optional WILDCARD FULL-DIRECTORY-P)
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (when (re-search-forward "^ *total used in directory \\([0-9]+\\) available \\([0-9]+\\)")
	(replace-match (format "%s k" (group-number (match-string 1) 3 ".")) nil nil nil 1)
	(replace-match (format "%s k" (group-number (match-string 2) 3 ".")) nil nil nil 2)
	))))
(advice-add 'insert-directory :after #'my-insert-directory)

(defun my-dired-jump-to-first-entry ()
  (interactive)
  (beginning-of-buffer)
  (dired-goto-next-nontrivial-file))

(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'my-dired-jump-to-first-entry)

(defun my-dired-jump-to-last-entry ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'my-dired-jump-to-last-entry)

(setq ls-lisp-format-time-list  '("%Y-%m-%d %H:%M" "%Y-%m-%d %H:%M"))
(setq ls-lisp-use-localized-time-format t)

(add-hook 'dired-mode-hook 'auto-revert-mode)
(setq auto-revert-verbose nil)
(setq auto-revert-interval 1)
