(unless (package-installed-p 'visual-fill-column)
  (package-install 'visual-fill-column))

(unless (package-installed-p 'adaptive-wrap)
  (package-install 'adaptive-wrap))

(provide 'my-visual)
