;;; general

(server-start)

;;; visuals

(set-face-attribute 'default nil :height 115 :family "InputMono")
(load-theme 'wombat t)
(tool-bar-mode 0)
(setq inhibit-splash-screen t)

;; Pulse current line
(defun ji/pulse-line (&rest _)
      (pulse-momentary-highlight-one-line (point)))
(dolist (command '(recenter-top-bottom other-window ace-window))
  (advice-add command :after #'ji/pulse-line))

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
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-SPC") 'cycle-spacing)
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'yes-or-no-p)
(winner-mode 1)
(setq completion-ignore-case t)

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
(package-refresh-contents t)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;;; mode-line

(setq display-time-string-forms '((propertize (format-time-string "%d/%m/%Y %H:%M:%S" now))))
(setq display-time-interval 1)
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

(defun ji/proced-settings ()
  (proced-toggle-auto-update t)
  (setq proced-auto-update-interval 1)
  (setq proced-format 'medium)
  )
(add-hook 'proced-mode-hook 'ji/proced-settings)

;;; nxml-mode

(add-to-list 'auto-mode-alist '("\\.\\(xslt\\|targets\\|csproj\\)\\'" . nxml-mode))

;;; magit

(use-package magit
  :bind
  ("C-x g" . magit-status)
  :config
  (add-hook 'git-commit-setup-hook 'turn-off-auto-fill t)
  (setq magit-push-always-verify nil)
  (setq magit-diff-refine-hunk 'all)
  (use-package fullframe)
  (fullframe magit-status magit-mode-quit-window))

;;; ediff

(use-package ediff
  :bind
  ("M-<down>" . ediff-next-difference)
  ("M-<up>" . ediff-previous-difference)
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-diff-options "--text")
  :custom-face
  (ediff-current-diff-A ((t (:extend t :background "#553333" :foreground "white"))))
  (ediff-current-diff-B ((t (:extend t :background "#335533" :foreground "white"))))
  (ediff-current-diff-C ((t (:extend t :background "#888833" :foreground "white")))))

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

;;; tree-sitter

(use-package tree-sitter :ensure t)
(use-package tree-sitter-indent :ensure t)
(use-package tree-sitter-langs :ensure t)

;;; csharp-mode

(use-package csharp-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-tree-sitter-mode)))

;;; lsp-mode

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (csharp-tree-sitter-mode . lsp)
  :commands lsp)

(use-package lsp-ui
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-sideline-enable nil)
  :commands lsp-ui-mode)

;;; company-mode

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-backends-colors '((company-capf :all "white" :selected (:background "grey" :foreground "white")))))

;;; markdown-mode

(use-package markdown-mode)

;;; dockerfile-mode

(use-package dockerfile-mode)

;;; docker

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

;;; terraform-mode

(use-package terraform-mode)

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

(defun ji/swiper ()
  (interactive)
  (if (> (buffer-size) 1000000)
      (isearch-forward)
    (swiper)))

(use-package swiper
  :demand
  :bind
  ("C-s" . ji/swiper)
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
                '("mpg" "mpeg" "mp4" "avi" "wmv" "mov" "flv" "mkv" "m4v" "webm"))
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
  (setq org-image-actual-width nil)
  (add-to-list 'org-modules 'org-habit)
  (plist-put org-calc-default-modes 'calc-float-format '(float 12))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t) (shell . t)))
  (setq org-file-apps-gnu (append '((t . "setsid -w xdg-open %s")) org-file-apps-gnu)))

;;; valign

(use-package valign
  :hook (org-mode . valign-mode))

;;; hydra

(use-package hydra)

;;; org-superstar

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode))

;;; org-present

(defun ji/org-present-mode-hook ()
  (face-remap-set-base 'default '(:height 1.5) 'variable-pitch 'default)
  (face-remap-set-base 'org-document-title '(:height 3.0) 'org-document-title)
  (face-remap-set-base 'org-level-1 '(:height 2.0) 'org-level-1)
  (org-present-hide-cursor)
  (org-present-read-only)
  (org-display-inline-images))

(defun ji/org-present-mode-quit-hook ()
  (face-remap-reset-base 'default)
  (face-remap-reset-base 'org-document-title)
  (face-remap-reset-base 'org-level-1)
  (org-present-show-cursor)
  (org-present-read-write)
  (org-remove-inline-images))

(use-package org-present
  :after org
  :hook
  ((org-present-mode . ji/org-present-mode-hook)
   (org-present-mode-quit . ji/org-present-mode-quit-hook)))

;;; multiple-cursors

(use-package multiple-cursors
  :ensure t
  :bind (("C-c C-SPC" . mc/edit-lines)))

;;; helpful

(use-package helpful)
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)

;;; which-key

(use-package which-key
  :config
  (which-key-mode))

;;; dired

;; Ensure jumping to beginning and end of buffer stays within file list
(defun ji/dired-jump-to-first-entry ()
  (interactive)
  (beginning-of-buffer)
  (dired-goto-next-nontrivial-file))
(defun ji/dired-jump-to-last-entry ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

;; Add possibility to force open a file in an external application
(defun ji/dired-open-file-in-external-app ()
  (interactive)
  (cond ((eq system-type 'gnu/linux)
	 (let* ((file (dired-get-filename nil t))) (call-process "xdg-open" nil 0 nil file)))
	((eq system-type 'windows-nt)
	 (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" (dired-get-filename) t t)))))

(defun ji/dired-get-size ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "du" nil t nil "-sch" files)
      (message "Total size: %s"
               (progn
                 (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
                 (match-string 1))))))

(defun ji/ediff-marked-pair ()
  (interactive)
  (let* ((marked-files (dired-get-marked-files nil nil))
         (other-win (get-window-with-predicate
                     (lambda (window)
                       (with-current-buffer (window-buffer window)
                         (and (not (eq window (selected-window)))
                              (eq major-mode 'dired-mode))))))
         (other-marked-files (and other-win
                                  (with-current-buffer (window-buffer other-win)
                                    (dired-get-marked-files nil)))))
    (cond ((= (length marked-files) 2)
           (ediff-files (nth 0 marked-files) (nth 1 marked-files)))
          ((and (= (length marked-files) 1) (= (length other-marked-files) 1))
           (ediff-files (nth 0 marked-files) (nth 0 other-marked-files)))
          (t (error "Please mark exactly 2 files, at least one locally")))))

(defun ji/dired-duplicate-this-file ()
  (interactive)
  (let* ((this  (dired-get-filename t))
         (ctr   1)
         (new   (format "%s[%d].%s" (file-name-sans-extension this) ctr (file-name-extension this))))
    (while (file-exists-p new)
      (setq ctr  (1+ ctr)
            new  (format "%s[%d].%s" (file-name-sans-extension this) ctr (file-name-extension this))))
    (dired-copy-file this new nil)))

(defhydra hydra-dired (:color pink :hint nil)
"
^Mark^                  ^Operate^                       ^View^                              ^Search^
^^^^^^^^-------------------------------------------------------------------------------------------------------------
_m_   : mark            _C_       : copy                _g_       : refresh                 _M-s M-d_ : find files
_% m_ : mark regexp     _% C_     : copy regexp         _C-0 s_   : refresh using switches  _A_       : find in files
_% g_ : mark containing _R_       : rename/move         _C-n_     : narrow
_u_   : unmark          _% R_     : rename/move regexp  _s_       : toggle sorting
_U_   : unmark all      _D_       : delete              _v_       : view file
_t_   : toogle marks    _Z_       : compress/uncompress _M-s M-s_ : show total size
^ ^                     _c_       : compress to         _o_       : open in other window
^ ^                     _=_       : ediff marked pair   _w_       : copy file name
^ ^                     _C-x C-q_ : toggle read-only    _C-0 w_   : copy absolute file name
^ ^                     _M-s M-c_ : duplicate file      _(_       : toggle details
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
("C-0 s" (lambda () (interactive) (let ((current-prefix-arg 0)) (call-interactively #'dired-sort-toggle-or-edit))))
("C-n" dired-narrow)
("s" dired-sort-toggle-or-edit)
("v" dired-view-file)
("M-s M-s" dired-get-size)
("o" dired-find-file-other-window)
("=" ji/ediff-marked-pair)
("C-x C-q" dired-toggle-read-only)
("w" dired-copy-filename-as-kill)
("C-0 w" (lambda () (interactive) (let ((current-prefix-arg 0)) (call-interactively #'dired-copy-filename-as-kill))))
("(" dired-hide-details-mode)
("M-s M-d" find-name-dired)
("M-s M-c" ji/dired-duplicate-this-file)
("A" dired-do-find-regexp)
("q" quit-window "quit" :color blue)
("?" nil :color blue))

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
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
  :bind (:map dired-mode-map
	      ([remap beginning-of-buffer] . 'ji/dired-jump-to-first-entry)
	      ([remap end-of-buffer] . 'ji/dired-jump-to-last-entry)
	      ("M-RET" . 'ji/dired-open-file-in-external-app)
	      ("M-s M-s" . 'ji/dired-get-size)
	      ("M-s M-d" . 'find-name-dired)
	      ("M-s M-c" . 'ji/dired-duplicate-this-file)
	      ("=" . 'ji/ediff-marked-pair)
	      ("?" . 'hydra-dired/body)))

;; Use dired-narrow
(use-package dired-narrow
  :after dired
  :bind (:map dired-mode-map ("C-n" . dired-narrow)))
