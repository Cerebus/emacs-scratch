(unless (package-installed-p 'vertico)
  (package-install 'vertico))

(require 'vertico)
(require 'vertico-directory)

;; Cycle back to top/bottom result when the edge is reached
(customize-set-variable 'vertico-cycle t)

(vertico-mode 1)

(provide 'my-vertico)
