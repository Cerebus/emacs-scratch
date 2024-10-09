(require 'my-tree-sitter)

(unless (package-installed-p 'format-all)
  (package-install 'format-all))

(unless (package-installed-p 'yasnippet)
  (package-install 'yasnippet))

(unless (package-installed-p 'yasnippet-snippets)
  (package-install 'yasnippet-snippets))

;; Snippet support in general, but mainly for eglot
(require 'yasnippet)
(yas-global-mode 1)

;; (add-hook 'prog-mode-hook 'format-all-mode)
;; (add-hook 'format-all-mode-hook 'format-all-ensure-formatter)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(with-eval-after-load 'eglot
  (setq eglot-report-progress nil))

(with-eval-after-load "flyspell"
  (define-key flyspell-mode-map (kbd "C-.") nil))

(define-key global-map (kbd "s-i") 'completion-at-point)

(defvar-local my/flymake-diagnostic-buffer-name nil)

(defun my/toggle-flymake-diagnostics-buffer ()
  (interactive)
  (let ((window-buffer (get-buffer-window (flymake--diagnostics-buffer-name))))
    (if window-buffer
        (quit-window t window-buffer)
      (flymake-show-diagnostics-buffer))))

(global-set-key (kbd "C-c t e") #'my/toggle-flymake-diagnostics-buffer)
(global-auto-revert-mode t)

(customize-set-variable 'eldoc-echo-area-use-multiline-p 1)

;; When creating a new frame or tab, open with the scratch buffer.
(defun my/switch-to-scratch (&rest args)
  "Select scratch buffer, used for hooks and advice."
  (switch-to-buffer "*scratch*" t))

(defalias 'my/make-frame-command #'make-frame-command)
(defalias 'my/tab-new #'tab-new)

;; Close tab when killing project buffers
(advice-add 'project-kill-buffers :after 'tab-close)

;; Perform the actual switch with advice rather than hooks b/c we want tools like project.el to be able to control the
;; displayed buffer.
(advice-add 'my/make-frame-command :after #'my/switch-to-scratch)
(advice-add 'my/tab-new :after #'my/switch-to-scratch)

(define-key ctl-x-5-map (kbd "2") #'my/make-frame-command)
(define-key tab-prefix-map (kbd "2") #'my/tab-new)
(define-key project-prefix-map (kbd "R") #'project-find-regexp)

;; Set polymode to dispatch LSPs stuff to the correct major mode indirect buffer.
;; https://github.com/polymode/polymode/issues/305#issuecomment-1018700437
(with-eval-after-load 'polymode
  (add-to-list 'polymode-run-these-after-change-functions-in-other-buffers 'eglot--after-change)
  (add-to-list 'polymode-run-these-before-change-functions-in-other-buffers 'eglot--before-change))

(provide 'my-ide)
