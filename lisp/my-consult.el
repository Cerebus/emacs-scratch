(unless (package-installed-p 'consult)
  (package-install 'consult))

(require 'consult)

(keymap-global-set "C-s" 'consult-line)
(keymap-set minibuffer-local-map "C-r" 'consult-history)

(setq completion-in-region-function #'consult-completion-in-region)

(provide 'my-consult)
