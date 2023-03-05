(unless (package-installed-p 'corfu)
  (package-install 'corfu))

(require 'corfu)

(unless (display-graphic-p)
  (unless (package-installed-p 'corfu-terminal)
    (package-install 'corfu-terminal))
  (require 'corfu-terminal)
  (corfu-terminal-mode +1))

;; Setup corfu for popup like completion
(customize-set-variable 'corfu-cycle t)	; Allows cycling through
					; candidates
(customize-set-variable 'corfu-auto t)	; Enable auto completion
(customize-set-variable 'corfu-auto-prefix 2) ; Complete with less
					      ; prefix keys
(customize-set-variable 'corfu-auto-delay 0.0) ; No delay for completion
(customize-set-variable 'corfu-echo-delay 0.25)	; Echo docs for
						; current completion
						; option

(global-corfu-mode 1)

(require 'corfu-popupinfo)

(corfu-popupinfo-mode 1)
(eldoc-add-command #'corfu-insert)
(keymap-set corfu-map "M-p" #'corfu-popupinfo-scroll-down)
(keymap-set corfu-map "M-n" #'corfu-popupinfo-scroll-up)
(keymap-set corfu-map "M-d" #'corfu-popupinfo-toggle)

(provide 'my-corfu)
