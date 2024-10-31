(unless (package-installed-p 'markdown-mode)
  (package-install 'markdown-mode))

(add-hook 'markdown-mode-hook #'visual-line-mode)
;; (add-hook 'markdown-mode-hook #'visual-fill-column-mode)
(add-hook 'markdown-mode-hook #'adaptive-wrap-prefix-mode)

(defun my-markdown-fenced-code-block ()
  "Target a region between fenced code block markers."
  (save-excursion
    (let* ((head (search-backward "```" nil t))
	   (start (progn (forward-line)
			 (point)))
	   (tail (search-forward "```" nil t))
	   (end (progn (beginning-of-line)
		       (point))))
      (when (and head tail)
	`(region ,(buffer-substring start end) . (,start . ,end))))))

(with-eval-after-load 'embark
  (add-to-list 'embark-target-finders 'my-markdown-fenced-code-block)
  (add-to-list 'embark-around-action-hooks '(mermaid-compile-region embark--mark-target)))

(provide 'my-markdown)
