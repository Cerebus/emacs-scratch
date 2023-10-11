(unless (package-installed-p 'plantuml-mode)
  (package-install 'plantuml-mode))

;; mermaid
(unless (package-installed-p 'ob-mermaid)
  (package-install 'ob-mermaid))

(unless (package-installed-p 'mermaid-mode)
  (package-install 'mermaid-mode))

(add-to-list 'auto-mode-alist '("\\.p(lant)?uml\\'" . plantuml-mode))

(customize-set-variable 'plantuml-jar-path "~/.local/lib/plantuml.jar")
(customize-set-variable 'plantuml-exec-mode 'jar)

(with-eval-after-load 'plantuml-mode
  (if (not (file-exists-p plantuml-jar-path))
    (plantuml-download-jar)))

(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang '("plantuml" "mermaid"))))

(with-eval-after-load 'org
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t) (mermaid . t) (scheme . t)))
  (setq org-plantuml-jar-path plantuml-jar-path)
  (setq org-confirm-babel-evaluate #'my-org-confirm-babel-evaluate))


(provide 'my-plantuml)
