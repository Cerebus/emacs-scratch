;;; my-eshell.el --- Local shell extensions      -*- lexical-binding: t; -*-

;;; Code:
(customize-set-variable 'eshell-prefer-lisp-functions t)

(with-eval-after-load 'eshell
  (add-to-list 'eshell-modules-list 'eshell-tramp) ; gimme that sudo goodness
  (add-to-list 'eshell-modules-list 'eshell-rebind) ; really only care avout C-d
  )

(defun my/do-buffer-eshell (buffer-dir)
  "Start or switch to an EShell in BUFFER-DIR."
  (let* ((default-directory (expand-file-name buffer-dir))
	 (eshell-buffer-name "*eshell*")
	 (eshell-buffer (get-buffer eshell-buffer-name)))
    (if eshell-buffer
	(pop-to-buffer eshell-buffer (bound-and-true-p display-comint-buffer-action))
      (eshell))))

(defun my/project-eshell (arg)
  "Start or switch to an EShell in the current project.  With one
\\[universal-argument], start in the current buffer's directory
instead.  With multiple \\[universal-argument], start in the
user's home directory."
  (interactive "P")
  (cond ((eq (prefix-numeric-value arg) 4) (my/do-buffer-eshell default-directory))
	((> (prefix-numeric-value arg) 4) (my/do-buffer-eshell "~/"))
	(t (project-eshell))))

;; Keys
(global-set-key (kbd "C-x p e") #'eshell)

;;(define-key 'my-open-things-key-map (kbd "t") #'vterm)

;; Shells go to a popup
(add-to-list 'display-buffer-alist '("^.*eshell\*" (display-buffer-in-side-window) (side . bottom)))

(provide 'my-eshell)
;;; my-vterm.el ends here
