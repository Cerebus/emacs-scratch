(unless (package-installed-p 'consult)
  (package-install 'consult))

(require 'consult)

(keymap-global-set "C-s" 'consult-line)
(keymap-set minibuffer-local-map "C-r" 'consult-history)

(setq completion-in-region-function #'consult-completion-in-region)
(setq consult-narrow-key "<")
(bind-key (kbd "C-x b") 'consult-buffer)

(provide 'my-consult)
