;;; Python mode stuff.

;; Python package dependencies
;; - black
;; - flake8
;; - isort
;; - importmagic

;; TODO
;; - blacken code formatting?

;;; Baseline

;; Default to tree-sitter concrete mode
(my-tree-sitter-init 'python
		     'python-mode
		     'python-ts-mode
		     '("https://github.com/tree-sitter/tree-sitter-python"))

;;; Integration with emacs-scratch
(add-hook 'python-ts-mode-hook 'combobulate-mode)
(add-hook 'python-ts-mode-hook 'eglot-ensure)
(add-hook 'python-ts-mode-hook (lambda ()
			    (add-hook 'before-save-hook 'eglot-format-buffer nil t)))

(add-hook 'python-ts-mode-hook (lambda () (add-to-list 'format-all-formatters '(("Python" (black) (isort))))))

;;; Add-ons

;; pyvenv
(unless (package-installed-p 'pyvenv)
  (package-install 'pyvenv))

(add-hook 'pyvenv-post-activate-hooks #'pyvenv-restart-python)

;; Integrate pyvenv with pyenv venv
(setenv "WORKON_HOME" "~/.pyenv/versions")

(defun try/pyvenv-workon ()
  (when (buffer-file-name)
    (let* ((python-version ".python-version")
           (project-dir (locate-dominating-file (buffer-file-name) python-version)))
      (when project-dir
        (pyvenv-workon
         (with-temp-buffer
           (insert-file-contents (expand-file-name python-version project-dir))
           (car (split-string (buffer-string)))))))))

(add-hook 'python-ts-mode-hook #'try/pyvenv-workon)

;; isort
;; (unless (package-installed-p 'py-isort)
;;   (package-install 'py-isort))

;; (add-hook 'python-ts-mode-hook
;; 	  (lambda () (add-hook 'before-save-hook 'py-isort-buffer nil 'local)))

;; ;; importmagic
;; (unless (package-installed-p 'importmagic)
;;   (package-install 'importmagic))

;; (add-hook 'python-ts-mode-hook 'importmagic-mode)

(defvar-local my-python-shell-from-buffer ""
  "Records originating buffer when switching to python shell.")

(defun my-python-shell-save-buffer (oldfun &rest args)
  "Record the current shell for `my-python-shell-switch-to-buffer'."
  (let* ((buf (current-buffer))
	 (res (apply oldfun args)))
    (setq-local my-python-shell-from-buffer buf)
    res))

(defun my-python-shell-switch-to-buffer ()
  "Switch back from python shell to last python buffer."
  (interactive)
  (if (buffer-live-p my-python-shell-from-buffer)
      (pop-to-buffer my-python-shell-from-buffer)
    (let ((proc (run-python)))
      (pop-to-buffer (process-buffer proc)))
    ))

(with-eval-after-load "python"
  (define-key python-ts-mode-map (kbd "C-c C-z") #'my-python-shell-switch-to-buffer)
  (define-key inferior-python-mode-map (kbd "C-c C-z") #'my-python-shell-switch-to-buffer)
  )

(advice-add 'python-shell-switch-to-shell :around #'my-python-shell-save-buffer)
(advice-add 'my-python-shell-switch-to-buffer :around #'my-python-shell-save-buffer)

;;; END
(provide 'my-python)
