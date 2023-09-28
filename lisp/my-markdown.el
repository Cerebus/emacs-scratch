(unless (package-installed-p 'markdown-mode)
  (package-install 'markdown-mode))

(add-hook 'markdown-mode-hook #'visual-line-mode)
(add-hook 'markdown-mode-hook #'visual-fill-column-mode)
(add-hook 'markdown-mode-hook #'adaptive-wrap-prefix-mode)

(provide 'my-markdown)
