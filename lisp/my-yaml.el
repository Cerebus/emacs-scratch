;;; YAML for all teh things.
(unless (package-installed-p 'yaml-mode)
  (package-install 'yaml-mode))

(autoload 'yaml-mode "yaml-mode" "A Major mode for YAML" t)

(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))

(my-tree-sitter-init 'yaml 'yaml-mode 'yaml-mode '("https://github.com/ikatyang/tree-sitter-yaml"))

(add-hook 'yaml-mode-hook #'eglot-ensure)
; (add-hook 'yaml-mode-hook 'combobulate-mode)

;;; END
(provide 'my-yaml)
