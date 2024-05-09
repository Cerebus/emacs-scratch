(require 'my-tree-sitter)

(unless (package-installed-p 'format-all)
  (package-install 'format-all))

(add-hook 'prog-mode-hook 'format-all-ensure-formatter)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'prog-mode-hook (lambda ()
			    (add-hook 'before-save-hook 'eglot-format-buffer nil t)))

(with-eval-after-load "flyspell"
  (define-key flyspell-mode-map (kbd "C-.") nil))

(defvar-local my/flymake-diagnostic-buffer-name nil)

(defun my/toggle-flymake-diagnostics-buffer ()
  (interactive)
  (let ((window-buffer (get-buffer-window (flymake--diagnostics-buffer-name))))
    (if window-buffer
        (quit-window t window-buffer)
      (flymake-show-diagnostics-buffer))))

(global-set-key (kbd "C-c t e") #'my/toggle-flymake-diagnostics-buffer)
(global-auto-revert-mode t)

;; When creating a new frame or tab, open with the scratch buffer.
(defun my/switch-to-scratch (&rest args)
  "Select scratch buffer, used for hooks and advice."
  (switch-to-buffer "*scratch*" t))

(defalias 'my/make-frame-command #'make-frame-command)
(defalias 'my/tab-new #'tab-new)

;; Perform the actual switch with advice rather than hooks b/c we want tools like project.el to be able to control the
;; displayed buffer.
(advice-add 'my/make-frame-command :after #'my/switch-to-scratch)
(advice-add 'my/tab-new :after #'my/switch-to-scratch)

(define-key ctl-x-5-map (kbd "2") #'my/make-frame-command)
(define-key tab-prefix-map (kbd "2") #'my/tab-new)

;; Set polymode to dispatch LSPs stuff to the correct major mode indirect buffer.
;; https://github.com/polymode/polymode/issues/305#issuecomment-1018700437
(with-eval-after-load 'polymode
    (add-to-list 'polymode-run-these-after-change-functions-in-other-buffers 'eglot--after-change)
    (add-to-list 'polymode-run-these-before-change-functions-in-other-buffers 'eglot--before-change))

(provide 'my-ide)
