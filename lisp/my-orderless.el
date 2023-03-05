(unless (package-installed-p 'orderless)
  (package-install 'orderless))

(require 'orderless)

(customize-set-variable 'completion-styles '(orderless basic))
(customize-set-variable 'completion-category-overrides
                        '((file (styles . (partial-completion)))))

(provide 'my-orderless)
