(unless (package-installed-p 'org-fragtog)
  (package-install 'org-fragtog))

(unless (package-installed-p 'org-glossary)
  (package-vc-install "https://github.com/tecosaur/org-glossary.git"))

(customize-set-variable 'org-modules '(ol-doi ol-w3m ol-bibtex ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-eww org-tempo))

(require 'org-glossary)

(add-hook 'org-mode-hook (lambda ()
           (setq-local electric-pair-inhibit-predicate
                   `(lambda (c)
                  (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

(add-hook 'org-mode-hook #'visual-line-mode)
(add-hook 'org-mode-hook #'visual-fill-column-mode)
(add-hook 'org-mode-hook #'adaptive-wrap-prefix-mode)
(add-hook 'org-mode-hook #'org-fragtog-mode)
(add-hook 'org-mode-hook #'flyspell-mode)

(provide 'my-org)
