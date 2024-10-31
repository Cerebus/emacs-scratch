(unless (package-installed-p 'pdf-tools)
  (package-install 'pdf-tools)
  (pdf-tools-install))

(pdf-tools-install)

;; (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1)))

(provide 'my-pdf)
