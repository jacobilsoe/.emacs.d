(require-package 'swiper)

(ivy-mode)

(defun jacobilsoe-swiper ()
  (interactive)
  (if (> (buffer-size) 1000000)
      (isearch-forward)
    (swiper)))

(global-set-key (kbd "C-s") 'jacobilsoe-swiper)

(provide 'jacobilsoe-swiper)
