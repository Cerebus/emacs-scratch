(unless (package-installed-p 'pdf-tools)
  (package-install 'pdf-tools)
  (pdf-tools-install))

(pdf-loader-install)
(defalias 'doc-view-goto-page 'pdf-view-goto-page)

(provide 'my-pdf)
