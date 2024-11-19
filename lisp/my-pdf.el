(unless (package-installed-p 'pdf-tools)
  (package-install 'pdf-tools)
  (pdf-tools-install))

(pdf-tools-install)

(provide 'my-pdf)
