(unless (package-installed-p 'embark)
  (package-install 'embark))

(unless (package-installed-p 'embark-consult)
  (package-install 'embark-consult))

(require 'embark)

(keymap-global-set "<remap> <describe-bindings>" #'embark-bindings)
(keymap-global-set "C-." 'embark-act)
(keymap-global-set "C-;" 'embark-dwim)

;; Use Embark to show bindings in a key prefix with `C-h`
(setq prefix-help-command #'embark-prefix-help-command)

(require 'my-consult)
(require 'embark-consult)

(add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)

(define-key embark-expression-map "d" #'delete-pair)

(provide 'my-embark)
