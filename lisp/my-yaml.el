;;; YAML for all teh things.
(unless (package-installed-p 'yaml-mode)
  (package-install 'yaml-mode))

(autoload 'yaml-mode "yaml-mode" "A Major mode for YAML" t)

(add-to-list 'auto-mode-alist '("[^\\.][^j][^2]\\.ya?ml" . yaml-mode))

(my-tree-sitter-init 'yaml 'yaml-mode 'yaml-mode '("https://github.com/ikatyang/tree-sitter-yaml"))

(add-hook 'yaml-mode-hook #'eglot-ensure)

;; yaml-ts-mode is (currently) crap, and yaml-mode doesn't grok tree-sitter, so we make it so.
(setq auto-mode-alist (delete '("\\.ya?ml\\'" . yaml-ts-mode) auto-mode-alist))
(add-hook 'yaml-mode-hook (lambda ()
			    (treesit-parser-create 'yaml)
			    (combobulate-mode)))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
	       `((yaml-mode yaml-ts-mode) . ("yaml-language-server" "--stdio" :initializationOptions (:yaml (:keyOrdering nil))))))

;;; END
(provide 'my-yaml)
