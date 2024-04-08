(require 'my-tree-sitter)

(unless (package-installed-p 'format-all)
  (package-install 'format-all))

(add-hook 'prog-mode-hook 'format-all-mode)

(defvar-local my/flymake-diagnostic-buffer-name nil)

(defun my/toggle-flymake-diagnostics-buffer ()
  (interactive)
  (let ((window-buffer (get-buffer-window (flymake--diagnostics-buffer-name))))
    (if window-buffer
        (quit-window t window-buffer)
      (flymake-show-diagnostics-buffer))))

(global-set-key (kbd "C-c t e") #'my/toggle-flymake-diagnostics-buffer)
(global-auto-revert-mode t)

;; When I open a new frame, I like to have it default to scratch
(defun my/switch-to-scratch (frame)
  (switch-to-buffer "*scratch*"))

(add-hook 'after-make-frame-functions #'my/switch-to-scratch)

;; Set polymode to dispatch LSPs stuff to the correct major mode indirect buffer.
;; https://github.com/polymode/polymode/issues/305#issuecomment-1018700437
(with-eval-after-load 'polymode
    (add-to-list 'polymode-run-these-after-change-functions-in-other-buffers 'eglot--after-change)
    (add-to-list 'polymode-run-these-before-change-functions-in-other-buffers 'eglot--before-change))

(provide 'my-ide)
