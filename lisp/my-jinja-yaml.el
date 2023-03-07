;;; Polymode for Jinja2 in YAML.

(unless (package-installed-p 'polymode)
  (package-install 'poly-ansible))

(autoload 'poly-ansible-mode "poly-ansible" "A polymode for Jinja2 in YAML." t)

(add-to-list 'auto-mode-alist '("\\.j2.yaml\\'" . poly-ansible-mode))

(provide 'my-jinja-yaml)
