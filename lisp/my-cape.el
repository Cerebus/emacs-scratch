(unless (package-installed-p 'cape)
  (package-install 'cape))

(require 'cape)

;; Add useful defaults completion sources from cape
(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)

;; Silence the pcomplete capf, no errors or messages!
;; Important for corfu
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

;; Ensure that pcomplete does not write to the buffer
;; and behaves as a pure `completion-at-point-function'.
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
(add-hook 'eshell-mode-hook (lambda () (setq-local corfu-quit-at-boundary t
						   corfu-quit-no-match t
						   corfu-auto nil)
			      (corfu-mode)))

(provide 'my-cape)
