;;; Golang language config

(unless (package-installed-p 'go-mode)
  (package-install 'go-mode)
  (package-install 'go-gen-test)
  (package-install 'gotest))

(my-tree-sitter-init 'go 'go-mode 'go-mode '("https://github.com/smacker/go-tree-sitter" "master" "golang"))

;; Activate LSP
(add-hook 'go-ts-mode-hook #'eglot-ensure)
(add-hook 'go-ts-mode-hook (lambda ()
			    (add-hook 'before-save-hook 'eglot-format-buffer nil t)))


;; Autoloads just to be sure
(autoload 'go-mode "go-mode" "A major mode for Golang" t)
(autoload 'go-gen-test-dwim "go-gen-test" "A package for gotest generation" t)
(autoload 'go-test-current-test "gotest" "A package for gotest execution" t)

(add-to-list 'auto-mode-alist '("\\.go\'" . go-ts-mode))

;; Add gotest to embark.
(with-eval-after-load 'embark
  (add-to-list 'embark-pre-action-hooks '(go-gen-test-dwim embark--mark-target))
  (define-key embark-region-map (kbd "g t") #'go-gen-test-dwim)
  (define-key embark-defun-map (kbd "g t") #'go-gen-test-dwim)
  (define-key embark-defun-map (kbd "g r") #'go-test-current-test))

;; Because.
(with-eval-after-load "go-mode"
  (add-to-list 'before-save-hook #'gofmt-before-save))

;; Hacks

; hack for projects without vc backend
(require 'project)

(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)

;;; END
(provide 'my-golang)
