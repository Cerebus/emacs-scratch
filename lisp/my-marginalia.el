(unless (package-installed-p 'marginalia)
  (package-install 'marginalia))

(require 'marginalia)

(customize-set-variable 'marginalia-annotators
                        '(marginalia-annotators-heavy
                          marginalia-annotators-light
                          nil))
(marginalia-mode 1)

(provide 'my-marginalia)
