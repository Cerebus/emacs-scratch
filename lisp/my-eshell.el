;;; my-eshell.el --- Local shell extensions      -*- lexical-binding: t; -*-

;;; Code:
(customize-set-variable 'eshell-prefer-lisp-functions t)

(with-eval-after-load 'eshell
  (add-to-list 'eshell-modules-list 'eshell-tramp) ; gimme that sudo goodness
  (add-to-list 'eshell-modules-list 'eshell-rebind) ; really only care avout C-d
  )


;; Keys
(global-set-key (kbd "C-c o e") #'eshell)

;;(define-key 'my-open-things-key-map (kbd "t") #'vterm)

;; Shells go to a popup
(add-to-list 'display-buffer-alist '("^.*eshell\*" (display-buffer-in-side-window) (side . bottom)))

(provide 'my-eshell)
;;; my-vterm.el ends here
