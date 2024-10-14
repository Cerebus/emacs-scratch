;; PlantUML
(unless (package-installed-p 'plantuml-mode)
  (package-install 'plantuml-mode))

(customize-set-variable 'plantuml-jar-path "~/.local/lib/plantuml.jar")
(customize-set-variable 'plantuml-exec-mode 'jar)
(customize-set-variable 'org-plantuml-jar-path plantuml-jar-path)

(add-to-list 'org-babel-load-languages '(plantuml . t))
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))

(with-eval-after-load 'plantuml-mode
  (if (not (file-exists-p plantuml-jar-path))
    (plantuml-download-jar)))

;; Mermaid
(unless (package-installed-p 'mermaid-mode)
   (package-install 'mermaid-mode))

(customize-set-variable 'ob-mermaid-cli-path "/opt/homebrew/bin/mmdc")
(add-to-list 'org-babel-load-languages '(mermaid . t))
(add-to-list 'org-src-lang-modes '("mermaid" . mermaid))

;; Graphviz
(add-to-list 'org-babel-load-languages '(dot . t))

;; (defun my-org-confirm-babel-evaluate (lang body)
;;   (not (member lang '("plantuml" "mermaid"))))
;; (customize-set-variable 'org-confirm-babel-evaluate #'my-org-confirm-babel-evaluate)


;; (with-eval-after-load 'org
;;   (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t) (mermaid . t) (scheme . t) (ditaa . t) (dot . t)))


(provide 'my-diagramming)
