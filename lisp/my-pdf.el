(unless (package-installed-p 'pdf-tools)
  (package-install 'pdf-tools)
  (pdf-tools-install))

(pdf-loader-install)

(provide 'my-pdf)
