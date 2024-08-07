(my-tree-sitter-init 'dockerfile 'dockerfile-mode 'dockerfile-ts-mode '("https://github.com/smacker/go-tree-sitter" "master" "dockerfile"))

(add-hook 'dockerfile-ts-mode-hook 'eglot-ensure)
(add-hook 'dockerfile-ts-mode-hook (lambda ()
			    (add-hook 'before-save-hook 'eglot-format-buffer nil t)))

(autoload 'dockerfile-ts-mode "dockerfile-ts-mode" "A major mode for Dockerfiles" t)
(add-to-list 'auto-mode-alist '("Dockerfile" . dockerfile-ts-mode))

(provide 'my-docker)
