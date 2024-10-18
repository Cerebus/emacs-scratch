(unless (package-installed-p 'rego-mode)
  (package-install 'rego-mode))

(my-tree-sitter-init 'rego 'rego-mode 'rego-mode '("https://github.com/FallenAngel97/tree-sitter-rego"))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
	       `(rego-mode . ("regal" "language-server"))))



(add-hook 'rego-mode-hook #'eglot-ensure)
(add-hook 'rego-mode-hook (lambda ()
			    (add-hook 'before-save-hook 'eglot-format-buffer nil t)))

(provide 'my-rego)
