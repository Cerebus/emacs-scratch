;;; Highlight-todo mode

(unless (package-installed-p 'hl-todo)
  (package-install 'hl-todo))

(global-hl-todo-mode)

;;; END
(provide 'my-hl-todo)
