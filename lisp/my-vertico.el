(unless (package-installed-p 'vertico)
  (package-install 'vertico))

(require 'vertico)
(require 'vertico-directory)

(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(setq enable-recursive-minibuffers t)

;; Cycle back to top/bottom result when the edge is reached
(customize-set-variable 'vertico-cycle t)

(with-eval-after-load "consult"
  (setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args))))

(vertico-mode 1)

(provide 'my-vertico)
