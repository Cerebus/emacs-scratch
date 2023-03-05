(require 'my-tree-sitter)

(unless (package-installed-p 'format-all)
  (package-install 'format-all))

(add-hook 'prog-mode-hook 'format-all-mode)

(provide 'my-ide)
