(unless (package-installed-p 'plantuml-mode)
  (package-install 'plantuml-mode))

(add-to-list 'auto-mode-alist '("\\.p(lant)?uml\\'" . plantuml-mode))

(customize-set-variable 'plantuml-jar-path "~/.local/lib/plantuml.jar")
(customize-set-variable 'plantuml-exec-mode 'jar)

(if (not (file-exists-p plantuml-jar-path))
    (plantuml-download-jar))

(with-eval-after-load 'org
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))
  (setq org-plantuml-jar-path plantuml-jar-path))

(provide 'my-plantuml)
