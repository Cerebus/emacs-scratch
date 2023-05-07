(unless (package-installed-p 'visual-fill-column)
  (package-install 'visual-fill-column))

(unless (package-installed-p 'adaptive-wrap)
  (package-install 'adaptive-wrap))

(setq-default fill-column 80)

(provide 'my-visual)
