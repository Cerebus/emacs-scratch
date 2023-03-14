;;; my-eshell.el --- Local shell extensions      -*- lexical-binding: t; -*-

;;; Code:
(customize-set-variable 'eshell-prefer-lisp-functions t)

(with-eval-after-load 'eshell
  (add-to-list 'eshell-modules-list 'eshell-tramp) ; gimme that sudo goodness
  (add-to-list 'eshell-modules-list 'eshell-rebind) ; really only care avout C-d
  )


;; Keys
;;(define-key 'my-open-things-key-map (kbd "e") #'eshell)
;;(define-key 'my-open-things-key-map (kbd "t") #'vterm)

;; Shells go to a popup
;;(add-to-list 'display-buffer-alist '("\\(?:shell\\|vterm\\)" display-buffer-pop-up-window))
;;(add-to-list 'popwin:special-display-config '("*vterm*"))
;;(add-to-list 'popwin:special-display-config '("*eshell*"))
;;(popwin-mode 1)

(provide 'my-eshell)
;;; my-vterm.el ends here
