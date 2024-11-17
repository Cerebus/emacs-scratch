;; Temp variables for other modes to load into.
;; (defcustom my/org-babel-load-languages '((emacs-lisp . t))
;;   "Org Babel languages."
;;   :type '(alist :key-type (symbol)
;; 		:value-type (boolean)))

;; (defcustom my/org-src-lang-modes '(("C" . c))
;;   "Org Babel languages."
;;   :type '(alist :key-type (string)
;; 		:value-type (symbol)))

;; Extra packages
(use-package org-modern
  :ensure t
  :defer t
  :hook
  ((org-mode . org-modern-mode)
   (org-agenda-finalize . org-modern-agenda))
  :config
  (global-org-modern-mode))

(unless (package-installed-p 'org-fragtog)
  (package-install 'org-fragtog))

(unless (package-installed-p 'org-glossary)
  (package-vc-install "https://github.com/tecosaur/org-glossary.git"))

(unless (package-installed-p 'flymake-proselint)
  (package-install 'flymake-proselint))

(with-eval-after-load 'flymake
  (flymake-proselint-setup))

(add-hook 'text-mode-hook 'flymake-mode)
(add-hook 'text-mode-hook 'flymake-proselint-setup)

(defun my/org-ispell-setup ()
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("#\\+" . ".$"))
  (add-to-list 'ispell-skip-region-alist '("\\[" . ".\\]"))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("<<" ">>"))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))

(add-hook 'org-mode-hook #'my/org-ispell-setup)

(customize-set-variable 'org-modules '(ol-doi ol-w3m ol-bibtex ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-eww org-tempo))
(customize-set-variable 'org-startup-folded 'nofold)
(customize-set-variable 'org-startup-with-inline-images t)
(customize-set-variable 'org-export-with-smart-quotes t)

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
    :backref-seperator ""
    :letter-heading ""
    :definition-structure "\\noindent *%d*\\emsp{}%v\\ensp{}%b\n")
  (org-glossary-set-export-spec 'latex 'acronym
    :backref ""
    :backref-seperator ""
    :letter-heading ""
    :definition-structure "\\noindent *%d*\\emsp{}%v\\ensp{}%b\n"))

(require 'org-glossary)

(add-hook 'org-mode-hook (lambda ()
           (setq-local electric-pair-inhibit-predicate
                   `(lambda (c)
                  (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

;; Init org-babel
(add-hook 'org-mode-hook (lambda ()
			   (org-babel-do-load-languages 'org-babel-do-load-languages org-babel-load-languages)))

;; ;; Init src modes
;; (add-hook 'org-mode-hook (lambda ()
;; 			   (mapc (lambda (x) (#'my/customize-add-to-list 'org-src-lang-modes x)) my/org-src-lang-modes)))

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

(add-hook 'org-mode-hook #'visual-line-mode)
;; (add-hook 'org-mode-hook #'visual-fill-column-mode)
(add-hook 'org-mode-hook #'adaptive-wrap-prefix-mode)
(add-hook 'org-mode-hook #'org-fragtog-mode)
(add-hook 'org-mode-hook #'flyspell-mode)
(add-hook 'org-mode-hook #'org-glossary-mode)

(bind-key (kbd "C-c (") #'org-mark-ring-goto 'org-mode-map)

(provide 'my-org)
