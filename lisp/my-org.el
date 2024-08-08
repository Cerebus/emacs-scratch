(unless (package-installed-p 'org-fragtog)
  (package-install 'org-fragtog))

(unless (package-installed-p 'org-glossary)
  (package-vc-install "https://github.com/tecosaur/org-glossary.git"))

(customize-set-variable 'org-modules '(ol-doi ol-w3m ol-bibtex ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-eww org-tempo))
(customize-set-variable 'org-startup-folded 'nofold)
(customize-set-variable 'org-startup-with-inline-images t)

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
	       '("mitretr"
		 "\\documentclass[12pt]{mitretr}
\\usepackage[style=ieee,sorting=none]{biblatex}
\\usepackage[inkscapelatex=false]{svg}
\\usepackage{capt-of}
\\usepackage[colorlinks,linkcolor=blue,citecolor=blue,urlcolor=red,plainpages=false,pdfusetitle]{hyperref}
[NO-DEFAULT-PACKAGES]"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (customize-set-variable 'org-latex-subtitle-separate t)
  (customize-set-variable 'org-latex-subtitle-format "\\subtitle{%s}")
  (customize-set-variable 'org-latex-pdf-process '("latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f")))

(with-eval-after-load 'org-glossary
  (org-glossary-set-export-spec 'latex 'glossary
    :backref ""
    :backref-seperator "")
  (org-glossary-set-export-spec 'latex 'acronym
    :backref ""
    :backref-seperator ""))

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
(add-hook 'org-mode-hook #'org-glossary-mode)

(provide 'my-org)
