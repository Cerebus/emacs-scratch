(unless (package-installed-p 'combobulate)
  (package-vc-install "https://github.com/mickeynp/combobulate"))

(autoload 'combobulate-mode "combobulate" "Structured editing with tree-sitter." t)

(provide 'my-combobulate)
