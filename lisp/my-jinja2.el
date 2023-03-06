;;; my-jinja2.el --- Local configuration of jinja2 mode  -*- lexical-binding: t; -*-
(unless (package-installed-p 'jinja2-mode)
  (package-install 'jinja2-mode))

(autoload 'jinja2-mode "jinja2-mode" "A major mode for Jinja2 templates" t)
(add-to-list 'auto-mode-alist '("\\.j2(\\..*)?" . jinja2-mode))

(provide 'my-jinja2)
;;; my-jinja2.el ends here
